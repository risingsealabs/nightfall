{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Nightfall.Targets.Miden ( Context(..)
                               , defaultContext
                               , Config(..)
                               , defaultConfig
                               , VarInfo(..)
                               , toHashModule
                               , transpile
                               ) where

import Nightfall.Lang.Internal.Types as NFTypes
import Nightfall.Lang.Types
import Nightfall.MASM.Types as MASM
import Nightfall.MASM.Miden

import Control.Monad
import Control.Monad.State
import Data.Coerce (coerce)
import Data.Foldable
import Data.List (genericLength, singleton)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Word
import System.IO.Unsafe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as SText
import qualified Data.Text as Strict (Text)

-- | Applicatively fold a 'Foldable'.
foldMapA
    :: forall b m f a. (Monoid b, Applicative m, Foldable f)
    => (a -> m b)
    -> f a
    -> m b
foldMapA = coerce (foldMap :: (a -> Ap m b) -> f a -> Ap m b)

-- | Transpilation configuration options
data Config = Config {
      cgfTraceVariablesDecl :: Bool     -- ^ Whether or not adding comments when declaring variables
    , cfgTraceVariablesUsage :: Bool    -- ^ Whether or not adding comments when using ("calling") variables
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
    { cgfTraceVariablesDecl = False
    , cfgTraceVariablesUsage = False
}

data VarInfo = VarInfo
    { _varInfoType :: VarType
    , _varInfoPos  :: MemoryIndex
    }

-- TODO: a Note about 'adviceMapExt' and 'importExt'.
-- | Context for the transpilation, this is the state of the State Monad in which transpilation happens
data Context = Context
    { progName :: String                   -- ^ Name of the program, for outputs logs, etc.
    , memPos :: MemoryIndex                -- ^ Next free indice in Miden's global memory
    , variables :: Map String VarInfo      -- ^ Variables in the EDSL are stored in Miden's random
                                             -- access memory, we keep a map to match them
    , adviceMapExt :: Map Strict.Text [MidenWord]  -- ^ Entries to add to the inputs file
    , importExt :: Set Strict.Text         -- ^ Imports to add to the final program
    , config :: Config                     -- ^ Transpilation configuration options
    }

defaultContext :: Context
defaultContext = Context
    { progName = "<unnamed-program>"
    , memPos = case toMemoryIndex 0 of
            Nothing  -> error "Internal error: wrong index for initial memory position"
            Just pos -> pos
    , variables = Map.empty
    , adviceMapExt = Map.empty
    , importExt = Set.empty
    , config = defaultConfig
    }

toHashModule :: Word32 -> [MidenWord] -> Module
toHashModule numWords midenWords =
      Module
          ["std::mem"]  -- @std::mem@ provides @mem::pipe_words_to_memory@ used above.
          Map.empty
          (Program instrs)
          (Left $ SecretInputs (midenWords >>= midenWordToList) Map.empty)
  where
      instrs =
          [ Push [0, fromIntegral numWords]
            -- Stack: [numWords, 0, <zeros>]
          , Exec "mem::pipe_words_to_memory"
            -- Stack: [Hash, memPos', <zeros>]
          ]


-- | Compute the hash of the padded inputs by invoking the Miden executable with the inputs put on
-- @advice_stack@ and subsequently loaded into memory via @exec.mem::pipe_words_to_memory@,
-- which results in the top word on the stack being the hash in reversed form (so we
-- additionally reverse it to get the actual hash).
getHash :: Word32 -> [MidenWord] -> Either String [Felt]
getHash numWords midenWords =
    -- TODO: should transpilation work in IO instead or can we treat 'runMiden' as "morally-pure"?
    fmap reverse . unsafePerformIO $ runMiden DontKeep (Just 4) $ toHashModule numWords midenWords

toNumWords :: VarType -> Word32
toNumWords VarFelt              = 1
toNumWords VarBool              = 1
toNumWords (VarArrayOfFelt len) = len

-- | Allocate memory for a variable.
alloc
    :: VarName
    -> VarType
    -> State Context MemoryIndex
alloc var varType = do
    vars <- gets variables
    if Map.member var vars
        then error $ "Variable '" ++ var ++ "' has already been declared!"
        else do
            varPos <- gets memPos
            let numWords = toNumWords varType
            modify $ \ctx -> ctx
                { memPos = case toMemoryIndex $ unMemoryIndex varPos + fromIntegral numWords of
                    Nothing      -> error "Out of Miden's memory"
                    Just memPos' -> memPos'
                , variables = Map.insert var (VarInfo varType varPos) vars
                }
            pure varPos

-- | Entry point: transpile a EDSL-described ZK program into a Miden Module
transpile :: ZKProgram -> State Context Module
transpile zkProg = do
    warning <- foldMapA transpileStatement . runBody $
        comment "This program was generated by Nightfall (https://github.com/qredo/nightfall), avoid editing by hand.\n"
    modify $ \ctx -> ctx { progName = pName zkProg }
    midenInstr <- foldMapA transpileStatement . runBody $ pBody zkProg
    ctx <- get
    return $ Module { moduleImports = Set.toList $ importExt ctx
                    , moduleProcs = Map.empty -- No procs either (TODO)
                    , moduleProg = Program (warning ++ midenInstr)
                    , moduleSecretInputs = case pSecretInputs zkProg of
                        Left inputs -> Left $ inputs
                            { _adviceMap =
                                Map.unionWithKey
                                    (\hash _ _ -> error $ concat
                                        [ "a mapping for "
                                        , SText.unpack hash
                                        , " occurs twice"
                                        ])
                                    (adviceMapExt ctx)
                                    (_adviceMap inputs)
                            }
                        -- TODO: no, we're not supposed to ignore inputs that the program needs just
                        -- because we expect some of them to be provided via a file.
                        Right inputsFile -> Right inputsFile
                    }

transpileStatement :: Statement_ -> State Context [Instruction]
transpileStatement (NFTypes.Comment str) = return . singleton . MASM.Comment . SText.pack $ str
transpileStatement (IfElse cond ifBlock elseBlock) = do
    ifBlock' <- foldMapA transpileStatement ifBlock
    elseBlock' <- foldMapA transpileStatement elseBlock
    -- Not entirely sure, Miden talks about a "a small, but non-negligible overhead", but they don't say.
    -- I'm supposing it takes a pop/drop operation + a comparison
    cond' <- transpileExpr cond
    return $ cond' <> [ MASM.If ifBlock' elseBlock' ]
transpileStatement (NFTypes.Repeat count body) = do
    body' <- foldMapA transpileStatement body
    return $ [ MASM.Repeat count body' ]
transpileStatement (NFTypes.While cond body) = do
    body' <- foldMapA transpileStatement body
    cond' <- transpileExpr cond
    return $ cond' <> [ MASM.While $ body' <> cond' ]

-- | Declaring a variable loads the expression into Miden's global memory, and we track the index in memory
transpileStatement (DeclVariable varType var e) = do
    cfg <- gets config
    varPos <- alloc var varType
    -- Trace the variable declaration if configured
    let traceVar = [MASM.Comment $ "var " <> SText.pack var | cgfTraceVariablesDecl cfg]
    -- Transpile the variable value
    e' <- transpileExpr e
    -- Return instruction for the variable value and instruction to store the value in global memory
    return $ e' <> traceVar <> [ MASM.MemStore $ Just varPos ]

-- | Assigning a new value to a variable if just erasing the memory location
transpileStatement (AssignVar var e) = do
    vars <- gets variables
    shouldTrace <- gets (cfgTraceVariablesUsage . config)
    -- Fetch the memory location for that variable.
    case Map.lookup var vars of
        Nothing -> error $ concat
            [ "Variable \""
            , var
            , "\" has not been declared before: can't assign value"
            ]
        Just varInfo -> do
            -- Trace the variable usage if configured
            let traceVar = [MASM.Comment $ "var " <> SText.pack var | shouldTrace]
            e' <- transpileExpr e
            return $ e' <> traceVar <> [ MASM.MemStore . Just $ _varInfoPos varInfo ]

transpileStatement (SetAt arr i val) = do
    vars <- gets variables
    case Map.lookup arr vars of
        Nothing -> error $ "variable \"" ++ arr ++ "\" unknown (undeclared)"
        Just varInfo -> do
            unless (isArrayOfFelt $ _varInfoType varInfo) $
                error $ "'" ++ arr ++ "' is used as an array, but it's not one"
            let arrPos = unMemoryIndex $ _varInfoPos varInfo
            shouldTrace <- gets (cfgTraceVariablesUsage . config)
            let traceVar =
                    [ MASM.Comment $ "an element of '" <> SText.pack arr <> "' number"
                    | shouldTrace
                    ]
            valInstrs <- transpileExpr val
            iInstrs <- transpileExpr i
            return $ concat
                [ traceVar
                , valInstrs
                , iInstrs
                , -- Add the position of the array to @i@ to make the top felt on the stack peek at
                  -- @arr[i]@. Unless the array starts at the very beginning of memory, in which
                  -- case do nothing.
                  [MASM.Add . Just $ fromInteger arrPos | arrPos /= 0]
                -- TODO: add (optional) bounds checking.
                , [MemStore Nothing]
                ]

-- | A (naked) function call is done by pushing the argument on the stack and caling the procedure name
transpileStatement (NakedCall fname args) = do
    args' <- foldMapA transpileExpr args
    return $ args' <> [ MASM.Exec . SText.pack $ fname ]

transpileStatement (InitArray arr inputs) = do
    let numWords = genericLength inputs
        inputsAsWords = padListAsWords inputs
    arrPos <- alloc arr $ VarArrayOfFelt numWords
    let hash = either error id $ getHash numWords inputsAsWords
    -- TODO: use @lens@ already.
    modify $ \ctx -> ctx
        { -- @std::mem@ provides @mem::pipe_preimage_to_memory@ used below.
          importExt = Set.insert "std::mem" $ importExt ctx
        , adviceMapExt =
            case listToMidenWord hash of
                Nothing       -> error $ "Panic: a hash is not a Word: " ++ show hash
                Just hashWord -> Map.insert
                    (midenWordToHexKey hashWord)
                    inputsAsWords
                    (adviceMapExt ctx)
        }
    ctx <- get
    let traceVar =
            [ MASM.Comment $ "var " <> SText.pack arr
            | cgfTraceVariablesDecl $ config ctx
            ]
    pure $ traceVar ++
        [ Push hash
          -- Stack: [Hash, <rest>]
        , -- Look @hash@ up in @advice_map@ and put the associated felts onto @advice_stack@.
          Adv "push_mapval"
          -- Stack: [Hash, <rest>]
        , Push [fromIntegral $ unMemoryIndex arrPos, fromIntegral numWords]
          -- Stack: [numWords, arrPos, Hash, <rest>]
        , -- Load the felts (@numWords@ of them, i.e. all that were put there by @adv.push_mapval@)
          -- from @advice_stack@ into memory at @arrPos@ and check that they hash to the same @hash@
          -- that we computed above via 'runMiden'.
          Exec "mem::pipe_preimage_to_memory"
          -- Stack: [memPos', <rest>]
        , -- Drop the memory position (right after the last element of the array) remaining on the
          -- stack.
          Drop
          -- Stack: [<rest>]
        ]

transpileStatement (Return e) = transpileExpr e

transpileStatement EmptyLine = return . singleton $ MASM.EmptyL

-- | Transpile an unary operation.
transpileUnOp :: UnOp -> [Instruction]
transpileUnOp NFTypes.Not   = [ MASM.Not   ]
transpileUnOp NFTypes.IsOdd = [ MASM.IsOdd ]

-- | Transpile a binary operation.
transpileBinOp :: BinOp -> [Instruction]
-- Arithmetics operations are matched to their corresponding Miden operations
transpileBinOp NFTypes.Add    = [ MASM.Add Nothing ]
transpileBinOp NFTypes.Sub    = [ MASM.Sub Nothing ]
transpileBinOp NFTypes.Mul    = [ MASM.Mul Nothing ]
transpileBinOp NFTypes.Div    = [ MASM.Div Nothing ]
transpileBinOp NFTypes.IDiv32 = [ MASM.IDiv ]
transpileBinOp Equal          = [ MASM.Eq Nothing ]
transpileBinOp Lower          = [ MASM.Lt ]
transpileBinOp LowerEq        = [ MASM.Lte ]
transpileBinOp Greater        = [ MASM.Gt ]
transpileBinOp GreaterEq      = [ MASM.Gte ]

transpileLiteral :: Literal -> State Context [Instruction]
transpileLiteral (LiteralFelt x) = return . singleton $ Push [x]
transpileLiteral (LiteralBool b) = return . singleton $ Push [if b then 1 else 0]

-- TODO: range check, etc.
transpileExpr :: Expr_ -> State Context [Instruction]
-- transpileExpr (Lit _) = error "Can't transpile standalone literal" -- should be simply push it to the stack??
-- transpileExpr (Bo _)  = error "Can't transpile standalone boolean"
-- | Literals are simply pushed onto the stack
transpileExpr (Literal l) = transpileLiteral l

transpileExpr (Var varname) = do
    -- Fetch the memory location of that variable in memory, and push it to the stack
    vars <- gets variables
    case Map.lookup varname vars of
        Nothing -> error $ "variable '" ++ varname ++ "' unknown (undeclared)"
        Just varInfo -> do
            shouldTrace <- gets (cfgTraceVariablesUsage . config)
            let traceVar =
                    [ MASM.Comment . SText.pack $ fold
                        ["var ", varname, " (", ppVarType $ _varInfoType varInfo, ")"]
                    | shouldTrace
                    ]
            return $ traceVar <> [MemLoad . Just $ _varInfoPos varInfo]

transpileExpr (GetAt arr i) = do
    vars <- gets variables
    case Map.lookup arr vars of
        Nothing -> error $ "Array variable \"" ++ arr ++ "\" unknown (undeclared)"
        Just varInfo -> do
            unless (isArrayOfFelt $ _varInfoType varInfo) $
                error $ "'" ++ arr ++ "' is used as an array, but it's not one"
            let arrPos = unMemoryIndex $ _varInfoPos varInfo
            shouldTrace <- gets (cfgTraceVariablesUsage . config)
            let traceVar =
                    [ MASM.Comment $ "an element of '" <> SText.pack arr <> "' number"
                    | shouldTrace
                    ]
            iInstrs <- transpileExpr i
            return $ concat
                [ traceVar
                , iInstrs
                , -- Add the position of the array to @i@ to make the top felt on the stack peek at
                  -- @arr[i]@. Unless the array starts at the very beginning of memory, in which
                  -- case do nothing.
                  [MASM.Add . Just $ fromInteger arrPos | arrPos /= 0]
                -- TODO: add (optional) bounds checking.
                , [MemLoad Nothing]
                ]

transpileExpr (UnOp op e) = do
    es <- transpileExpr e
    return $ es <> transpileUnOp op
transpileExpr (BinOp op e1 e2) = do
    e1s <- transpileExpr e1
    e2s <- transpileExpr e2
    return $ e1s <> e2s <> transpileBinOp op

transpileExpr NextSecret =
    case toStackIndex 1 of
        Nothing  -> error "Internal error: wrong index for 'AdvPush'"
        Just idx -> return . singleton . MASM.AdvPush $ idx

transpileExpr FCall{} = error "transpileExpr::TODO"
