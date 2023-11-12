{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Nightfall.Targets.Miden ( dynamicMemoryHead
                               , Context(..)
                               , defaultContext
                               , Config(..)
                               , defaultConfig
                               , VarInfo(..)
                               , decorateM
                               , decorate
                               , onStack
                               , loadNat
                               , toHashModule
                               , transpileZKProgram
                               , Transpile(..)
                               , cgfTraceVariablesDecl
                               , cfgTraceVariablesUsage
                               , varInfoType
                               , varInfoPos
                               , progName
                               , statMemPos
                               , mayDynMemPtr
                               , variables
                               , adviceMapExt
                               , importExt
                               , config
                               ) where

import Nightfall.Lang.Internal.Types as NFTypes
import qualified Nightfall.Lang.Syntax.Default as Syntax
import Nightfall.Lang.Types
import Nightfall.MASM.Miden
import Nightfall.MASM.Types as MASM
import Nightfall.Prelude

import Control.Lens ((.=), (%=), (?=), use)
import Control.Monad.State
import Data.Word
import System.IO.Unsafe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as SText
import qualified Data.Text as Strict (Text)

dynamicMemoryHead :: Felt
dynamicMemoryHead = 1000000

dynPtrName :: VarName
dynPtrName = "__dynPtr"

-- | Transpilation configuration options
data Config = Config {
      _cgfTraceVariablesDecl :: Bool   -- ^ Whether or not adding comments when declaring variables
    , _cfgTraceVariablesUsage :: Bool  -- ^ Whether or not adding comments when using ("calling") variables
} deriving (Eq, Show)

data VarInfo = VarInfo
    { _varInfoType :: VarType
    , _varInfoPos  :: MemoryIndex
    }

-- TODO: a Note about 'adviceMapExt' and 'importExt'.
-- | Context for the transpilation, this is the state of the State Monad in which transpilation happens
data Context = Context
    { _progName :: String                   -- ^ Name of the program, for outputs logs, etc.
    , _statMemPos :: MemoryIndex            -- ^ Next free indice in Miden's global memory
    , _mayDynMemPtr :: Maybe MemoryIndex
    , _variables :: Map String VarInfo      -- ^ Variables in the EDSL are stored in Miden's random
                                            -- access memory, we keep a map to match them
    , _adviceMapExt :: Map Strict.Text [MidenWord]  -- ^ Entries to add to the inputs file
    , _importExt :: Set Strict.Text         -- ^ Imports to add to the final program
    , _config :: Config                     -- ^ Transpilation configuration options
    }

$(foldMapA makeLenses [''Context, ''VarInfo, ''Config])

defaultConfig :: Config
defaultConfig = Config
    { _cgfTraceVariablesDecl = False
    , _cfgTraceVariablesUsage = False
    }

defaultContext :: Context
defaultContext = Context
    { _progName = "<unnamed-program>"
    , _statMemPos = case toMemoryIndex 0 of
        Nothing  -> error "Internal error: wrong index for initial memory position"
        Just pos -> pos
    , _mayDynMemPtr = Nothing
    , _variables = Map.empty
    , _adviceMapExt = Map.empty
    , _importExt = Set.empty
    , _config = defaultConfig
    }

toHashModule :: Word32 -> [MidenWord] -> Module
toHashModule numWords midenWords =
      Module
          ["std::mem"]  -- @std::mem@ provides @mem::pipe_words_to_memory@ used above.
          Map.empty
          (Program instrs)
          (Left $ SecretInputs (midenWords >>= midenWordToFelts) Map.empty)
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
toNumWords VarNat               = 1

staticMaxMemoryIndex :: Integer -> Maybe MemoryIndex
staticMaxMemoryIndex = toMemoryIndexBelow $ 10 ^ (7 :: Int)

-- | Allocate memory for a variable.
alloc :: VarName -> VarType -> State Context MemoryIndex
alloc var varType = do
    vars <- use variables
    if Map.member var vars
        then error $ "Variable '" ++ var ++ "' has already been declared!"
        else do
            varPos <- use statMemPos
            let numWords = toNumWords varType
                mayStatMemPos' =
                    staticMaxMemoryIndex $ unMemoryIndex varPos + fromIntegral numWords
            statMemPos .= case mayStatMemPos' of
                Nothing          -> error "Out of Miden's memory"
                Just statMemPos' -> statMemPos'
            variables .= Map.insert var (VarInfo varType varPos) vars
            pure varPos

-- > repeat.count
-- >   body
--
-- to
--
-- > var counter = count
-- > while (counter /= 0)
-- >   body
-- >   counter = counter - 1
repeatDynamic :: VarName -> Expr asm Felt -> Body asm () -> Body asm ()
repeatDynamic counterName count body = do
    counter <- Syntax.declare counterName count
    while (not' $ Syntax.get counter `eq` 0) $ do
        body
        Syntax.set counter $ Syntax.get counter - 1

-- | Entry point: transpile a EDSL-described ZK program into a Miden Module
transpileZKProgram :: Transpile asm => ZKProgramAsm asm -> State Context Module
transpileZKProgram zkProg = do
    warning <- transpile $
        comment @Void "This program was generated by Nightfall (https://github.com/qredo/nightfall), avoid editing by hand.\n"
    progName .= pName zkProg
    midenInstr <- transpile $ pBody zkProg
    ctx <- get
    return $ Module { _moduleImports = Set.toList $ _importExt ctx
                    , _moduleProcs = Map.empty -- No procs either (TODO)
                    , _moduleProg = Program (warning ++ midenInstr)
                    , _moduleSecretInputs = case pSecretInputs zkProg of
                        Left inputs -> Left $ inputs
                            { _adviceMap =
                                Map.unionWithKey
                                    (\hash _ _ -> error $ concat
                                        [ "a mapping for "
                                        , SText.unpack hash
                                        , " occurs twice"
                                        ])
                                    (_adviceMapExt ctx)
                                    (_adviceMap inputs)
                            }
                        -- TODO: no, we're not supposed to ignore inputs that the program needs just
                        -- because we expect some of them to be provided via a file.
                        Right inputsFile -> Right inputsFile
                    }

transpileStatement :: Transpile asm => Statement_ asm -> State Context [Instruction]
transpileStatement (NFTypes.Comment str) = return . singleton . MASM.Comment . SText.pack $ str
transpileStatement (IfElse cond ifBlock elseBlock) = do
    ifBlock' <- foldMapA transpileStatement ifBlock
    elseBlock' <- foldMapA transpileStatement elseBlock
    -- Not entirely sure, Miden talks about a "a small, but non-negligible overhead", but they don't
    -- say. I'm supposing it takes a pop/drop operation + a comparison
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
    varPos <- alloc var varType
    -- Trace the variable declaration if configured
    traceVar <- do
        traceDecl <- use $ config . cgfTraceVariablesDecl
        pure [MASM.Comment $ "var " <> SText.pack var | traceDecl]
    -- Transpile the variable value
    e' <- transpileExpr e
    -- Return instruction for the variable value and instruction to store the value in global memory
    return $ e' <> traceVar <> [ MASM.MemStore $ Just varPos ]

-- | Assigning a new value to a variable if just erasing the memory location
transpileStatement (AssignVar var e) = do
    vars <- use variables
    shouldTrace <- use $ config . cfgTraceVariablesUsage
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
    vars <- use variables
    case Map.lookup arr vars of
        Nothing -> error $ "variable \"" ++ arr ++ "\" unknown (undeclared)"
        Just varInfo -> do
            unless (isArrayOfFelt $ _varInfoType varInfo) $
                error $ "'" ++ arr ++ "' is used as an array, but it's not one"
            let arrPos = unMemoryIndex $ _varInfoPos varInfo
            shouldTrace <- use $ config . cfgTraceVariablesUsage
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
        inputsAsWords = padFeltsAsMidenWords inputs
    arrPos <- alloc arr $ VarArrayOfFelt numWords
    let hash = either error id $ getHash numWords inputsAsWords
    -- @std::mem@ provides @mem::pipe_preimage_to_memory@ used below.
    importExt %= Set.insert "std::mem"
    adviceMapExt %= case feltsToMidenWord hash of
        Nothing       -> error $ "Panic: a hash is not a Word: " ++ show hash
        Just hashWord -> Map.insert
            (midenWordToHexKey hashWord)
            inputsAsWords

    traceVar <- do
        traceUsage <- use $ config . cfgTraceVariablesUsage
        pure [MASM.Comment $ "var " <> SText.pack arr | traceUsage]
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

decorateM
    :: asm ~ State Context [Instruction]
    => asm -> Expr asm a -> asm -> Expr asm b
decorateM asmPre expr asmPost = assembly $ foldA [asmPre, transpile expr, asmPost]

decorate
    :: asm ~ State Context [Instruction]
    => [Instruction] -> Expr asm a -> [Instruction] -> Expr asm b
decorate asmPre expr asmPost = decorateM (pure asmPre) expr (pure asmPost)

deref :: asm ~ State Context [Instruction]  => Expr asm Felt -> Expr asm Felt
deref ptr = decorate [] ptr [MemLoad Nothing]

derefw :: asm ~ State Context [Instruction] => Expr asm Felt -> Expr asm MidenWord
derefw ptr = decorate [Padw] ptr [MemLoadw Nothing]

deref256 :: asm ~ State Context [Instruction] => Expr asm Felt -> Expr asm word256
deref256 ptr = decorate [] (derefw ptr) [Padw]

onStack :: Expr (State Context [Instruction]) a
onStack = assembly $ pure []

loadNat :: State Context [Instruction]
loadNat = transpile $ do
    natPtr <- Syntax.declare "natPtr2" onStack
    repeatDynamic "counter3" (deref $ Syntax.get natPtr) $ do
        Syntax.set natPtr $ Syntax.get natPtr + 1
        ret . derefw $ Syntax.get natPtr

-- | Transpile a binary operation.
transpileBinOp :: BinOp -> State Context [Instruction]
-- Arithmetics operations are matched to their corresponding Miden operations
transpileBinOp NFTypes.Add    = pure [MASM.Add Nothing]
transpileBinOp NFTypes.Sub    = pure [MASM.Sub Nothing]
transpileBinOp NFTypes.Mul    = pure [MASM.Mul Nothing]
transpileBinOp NFTypes.Div    = pure [MASM.Div Nothing]
transpileBinOp NFTypes.IDiv32 = pure [MASM.IDiv]
transpileBinOp NFTypes.IMax32 = pure [MASM.IMax]
transpileBinOp NFTypes.Add256 = do
    importExt %= Set.insert "std::math::u256"
    pure [MASM.IAdd256]
transpileBinOp NFTypes.AddNat = transpile $ do
    yPtr <- Syntax.declare "yPtr" onStack
    xPtr <- Syntax.declare "xPtr" onStack
    ySize <- Syntax.declare "ySize" . deref $ Syntax.get yPtr
    xSize <- Syntax.declare "xSize" . deref $ Syntax.get xPtr
    i <- Syntax.declare "i" 1
    ret . assembly $ pure [Padw]  -- @carry@
    repeatDynamic "counter1" (binOp IMax32 (Syntax.get xSize) (Syntax.get ySize)) $ do
        ret . assembly $ pure [Padw]
        simpleIf (binOp LowerEq (Syntax.get i) (Syntax.get xSize)) $
           ret $ binOp Add256 (deref256 $ Syntax.get xPtr + Syntax.get i) onStack
        simpleIf (binOp LowerEq (Syntax.get i) (Syntax.get ySize)) $
           ret $ binOp Add256 (deref256 $ Syntax.get yPtr + Syntax.get i) onStack
        Syntax.set i $ Syntax.get i + 1
    ret . assembly $ pure [Drop, Drop, Drop]
    ifElse onStack
        (ret . assembly $ pure [Push [1, 0, 0, 0]])
        (Syntax.set i $ Syntax.get i - 1)
    let dynPtr = Syntax.Binding Syntax.Felt dynPtrName
    elPtr <- Syntax.declare "elPtr" $ Syntax.get dynPtr + Syntax.get i
    Syntax.set dynPtr $ Syntax.get elPtr + 1
    repeatDynamic "counter2" (Syntax.get i) $ do
        ret $ Syntax.get elPtr
        ret . assembly $ pure [MemStorew Nothing, Dropw]
        Syntax.set elPtr $ Syntax.get elPtr - 1
    ret $ Syntax.get i
    ret $ Syntax.get elPtr
    ret . assembly $ pure [MemStore Nothing]
    ret $ Syntax.get elPtr

transpileBinOp Equal          = pure [MASM.Eq Nothing]
transpileBinOp Lower          = pure [MASM.Lt]
transpileBinOp LowerEq        = pure [MASM.Lte]
transpileBinOp Greater        = pure [MASM.Gt]
transpileBinOp GreaterEq      = pure [MASM.Gte]

transpileLiteral :: Literal -> Felt
transpileLiteral (LiteralFelt x)        = x
transpileLiteral (LiteralBool b)        = if b then 1 else 0
transpileLiteral (LiteralNatPtr natPtr) = natPtr

transpileDynamic :: Dynamic -> State Context [Instruction]
transpileDynamic (DynamicNat nat) = do
    instrsInitDynPtr <- use mayDynMemPtr >>= \case
        Just _  -> pure []
        Nothing -> do
            dynPtr <- use statMemPos
            mayDynMemPtr ?= dynPtr
            transpileStatement @Void $
                DeclVariable VarFelt dynPtrName . unExpr $ lit dynamicMemoryHead
    instrsGetDynPtr <- transpileExpr @Void $ Var dynPtrName
    instrsIncDynPtr <- transpileStatement @Void . AssignVar dynPtrName $
        BinOp NFTypes.Add (Var dynPtrName) (Literal $ LiteralFelt 1)
    let natWords = naturalToMidenWords nat
        instrsSize = concat
            [ [Push [fromIntegral $ length natWords]]
            , instrsGetDynPtr
            , [MemStore Nothing]
            , instrsIncDynPtr
            ]
        instrsLimbs = do
            mword <- natWords
            -- TODO: this can be optimized, we don't really need to update 'dynPtr' in memory at
            -- each iteration.
            concat
                [ [Push $ midenWordToFelts mword]
                , instrsGetDynPtr
                , [MemStorew Nothing, Dropw]
                , instrsIncDynPtr
                ]
    pure $ instrsInitDynPtr ++ instrsGetDynPtr ++ instrsSize ++ instrsLimbs

class Transpile a where
    transpile :: a -> State Context [Instruction]

instance Transpile Void where
    transpile = \case{}

instance Transpile a => Transpile [a] where
    transpile = foldMapA transpile

instance Transpile Instruction where
    transpile = pure . pure

instance Transpile a => Transpile (State Context a) where
    transpile a = a >>= transpile

instance Transpile asm => Transpile (Expr_ asm) where
    transpile = transpileExpr

instance Transpile asm => Transpile (Expr asm a) where
    transpile = transpile . unExpr

instance Transpile asm => Transpile (Statement_ asm) where
    transpile = transpileStatement

instance (Transpile asm, a ~ ()) => Transpile (Body asm a) where
    transpile = transpile . runBody

-- TODO: range check, etc.
transpileExpr :: Transpile asm => Expr_ asm -> State Context [Instruction]
transpileExpr (Assembly asm) = transpile asm

-- Literals are simply pushed onto the stack
transpileExpr (Literal l) = return . singleton $ Push [transpileLiteral l]
transpileExpr (Dynamic d) = transpileDynamic d

transpileExpr (Var varname) = do
    -- Fetch the memory location of that variable in memory, and push it to the stack
    vars <- use variables
    case Map.lookup varname vars of
        Nothing -> error $ "variable '" ++ varname ++ "' unknown (undeclared)"
        Just varInfo -> do
            shouldTrace <- use $ config . cfgTraceVariablesUsage
            let traceVar =
                    [ MASM.Comment . SText.pack $ fold
                        ["var ", varname, " (", ppVarType $ _varInfoType varInfo, ")"]
                    | shouldTrace
                    ]
            return $ traceVar <> [MemLoad . Just $ _varInfoPos varInfo]

transpileExpr (GetAt arr i) = do
    vars <- use variables
    case Map.lookup arr vars of
        Nothing -> error $ "Array variable \"" ++ arr ++ "\" unknown (undeclared)"
        Just varInfo -> do
            unless (isArrayOfFelt $ _varInfoType varInfo) $
                error $ "'" ++ arr ++ "' is used as an array, but it's not one"
            let arrPos = unMemoryIndex $ _varInfoPos varInfo
            shouldTrace <- use $ config . cfgTraceVariablesUsage
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
    ops <- transpileBinOp op
    return $ e1s <> e2s <> ops

transpileExpr NextSecret =
    case toStackIndex 1 of
        Nothing  -> error "Internal error: wrong index for 'AdvPush'"
        Just idx -> return . singleton . MASM.AdvPush $ idx

transpileExpr FCall{} = error "transpileExpr::TODO"
