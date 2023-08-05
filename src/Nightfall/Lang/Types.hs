{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

module Nightfall.Lang.Types ( Felt
                            , VarName
                            , FunName
                            , Statement_
                            , Expr
                            , ZKProgram(..)
                            , BodyF(..)
                            , Body
                            , unBody
                            , mkSimpleProgram
                            , mkZKProgram
                            , lit
                            , bool
                            , add
                            , sub
                            , mul
                            , div'
                            , idiv32
                            , eq
                            , not'
                            , lt
                            , lte
                            , gt
                            , gte
                            , isOdd
                            , varF
                            , varB
                            -- , fcall
                            , nextSecret
                            , declareVarF
                            , declareVarB
                            , assignVarF
                            , assignVarB
                            , ifElse
                            , simpleIf
                            , while
                            -- , nakedCall
                            , ret
                            , comment
                            , emptyLine
                            , incVarF
                            , decVarF
                            , updateVarF
                            , withVarF
                            , withVarF2
                            , withVarF3
                            ) where

import Nightfall.Lang.Internal.Types

import Control.Monad.Free.Church
import Data.Word (Word32)

-- | Expression wrapper type, typed for safety, exposed to use
newtype Expr a = Expr
  { unExpr :: Expr_
  } deriving (Eq, Show)

-- | Num instance to make writings easier, to allow wriring expressions with "+", "-", etc.
instance a ~ Felt => Num (Expr a) where
  (+) = add
  (-) = sub
  (*) = mul
  fromInteger = Expr . Lit . fromInteger
  abs = error "'abs' not implemented for 'Expr'"
  signum = error "'signum' not implemented for 'Expr'"

data BodyF a = BodyF Statement_ a
  deriving (Eq, Show, Functor)

type Body = F BodyF ()

data ZKProgram = ZKProgram
    { pName :: String            -- ^ Program name, is this needed?
    , pBody :: Body              -- ^ List of statements comprising the program
    , pPublicInputs :: [Felt]    -- ^ Defining the public inputs as a list of field elements for now
    , pSecretInputs :: FilePath  -- ^ For now, for simplicty, we'll be referring to a '.inputs' file
    }

-- | Helper to quickly make a simple @ZKProgram from a list of statements, no inputs
mkSimpleProgram :: String -> Body -> ZKProgram
mkSimpleProgram name body = ZKProgram
    { pName = name
    , pBody = body
    , pPublicInputs = []
    , pSecretInputs = ""
    }

-- | Helper to build a @ZKPeogram
mkZKProgram :: String -> Body -> [Felt] -> FilePath -> ZKProgram
mkZKProgram name body pubs secretFP = ZKProgram
    { pName = name
    , pBody = body
    , pPublicInputs = pubs
    , pSecretInputs = secretFP
    }

-- * "Smart Constructors" for building (type-safe) @Expr. They are the ones exposed for users to use instead of constructing the
-- types directly.

-- ** Literals

lit :: Felt -> Expr Felt
lit = Expr . Lit

bool :: Bool -> Expr Bool
bool = Expr . Bo

-- ** Arithmetic operations

add :: Expr Felt -> Expr Felt -> Expr Felt
add  (Expr e1) (Expr e2) = Expr $ BinOp Add e1 e2

sub :: Expr Felt -> Expr Felt -> Expr Felt
sub (Expr e1) (Expr e2)= Expr $ BinOp Sub e1 e2

mul :: Expr Felt -> Expr Felt -> Expr Felt
mul (Expr e1) (Expr e2) = Expr $ BinOp Mul e1 e2

div' :: Expr Felt -> Expr Felt -> Expr Felt
div' (Expr e1) (Expr e2) = Expr $ BinOp Div e1 e2

idiv32 :: Expr Word32 -> Expr Word32 -> Expr Word32
idiv32 (Expr e1) (Expr e2) = Expr $ BinOp IDiv32 e1 e2

-- ** Boolean operations

eq :: Expr Felt -> Expr Felt -> Expr Bool
eq (Expr e1) (Expr e2)= Expr $ BinOp Equal e1 e2

not' :: Expr Bool -> Expr Bool
not' (Expr e) = Expr $ UnOp Not e

lt :: Expr Felt -> Expr Felt -> Expr Bool
lt (Expr e1) (Expr e2)= Expr $ BinOp Lower e1 e2

lte :: Expr Felt -> Expr Felt -> Expr Bool
lte (Expr e1) (Expr e2) = Expr $ BinOp LowerEq e1 e2

gt :: Expr Felt -> Expr Felt -> Expr Bool
gt (Expr e1) (Expr e2) = Expr $ BinOp Greater e1 e2

gte :: Expr Felt -> Expr Felt -> Expr Bool
gte (Expr e1) (Expr e2) = Expr $ BinOp GreaterEq e1 e2

isOdd :: Expr Felt -> Expr Bool
isOdd (Expr e) = Expr $ UnOp IsOdd e

-- ** Variables (typed)
varF :: VarName -> Expr Felt
varF = Expr . VarF

varB :: VarName -> Expr Bool
varB = Expr . VarB

-- Function calls will come later
-- fcall :: FunName -> [Expr] -> Expr
-- fcall = FCall

-- * Inputs

-- ** Secret input

nextSecret :: Expr a
nextSecret = Expr NextSecret

-- * "Smart Constructors" for building (type-safe) @Statement. They are the ones exposed for users to use

statement :: Statement_ -> Body
statement stmt = liftF $ BodyF stmt ()

unBody :: Body -> [Statement_]
unBody body = runF body (const []) $ \(BodyF stmt stmts) -> stmt : stmts

-- ** Variables
declareVarF :: VarName -> Expr Felt -> Body
declareVarF varname (Expr e) = statement $ DeclVariable varname e

declareVarB :: VarName -> Expr Bool -> Body
declareVarB varname (Expr e) = statement $ DeclVariable varname e

assignVarF :: VarName -> Expr Felt -> Body
assignVarF varname (Expr e) = statement $ AssignVar varname e

assignVarB :: VarName -> Expr Bool -> Body
assignVarB varname (Expr e) = statement $ AssignVar varname e

ifElse :: Expr Bool -> Body -> Body -> Body
ifElse (Expr cond) ifBlock elseBlock = statement $ IfElse cond (unBody ifBlock) (unBody elseBlock)

-- | A constructor for when you don't want 'else' statement
simpleIf :: Expr Bool -> Body -> Body
simpleIf cond ifBlock = ifElse cond ifBlock $ pure ()

while :: Expr Bool -> Body -> Body
while (Expr cond) body = statement $ While cond (unBody body)

-- nakedCall :: FunName -> [Expr] -> Body
-- nakedCall = NakedCall

ret :: Expr a -> Body
ret = statement . Return . unExpr

comment :: String -> Body
comment = statement . Comment

emptyLine :: Body
emptyLine = statement EmptyLine

-- ** A few helpers for common code patterns

-- | Apply a function of 1 argument over @var and store result in @targetVar
withVarF :: VarName -> VarName -> (Expr Felt -> Expr Felt) -> Body
withVarF var targetVar f = assignVarF targetVar $ f . varF $ var

-- | Apply a function of 2 arguments over two variables and store result in @targetVar
withVarF2 :: (VarName, VarName) -> VarName -> (Expr Felt -> Expr Felt -> Expr Felt) -> Body
withVarF2 (var1, var2) targetVar f = assignVarF targetVar $ f (varF var1) (varF var2)

-- | Apply a function of 3 arguments over three variables and store result in @targetVar
withVarF3 :: (VarName, VarName, VarName) -> VarName -> (Expr Felt -> Expr Felt -> Expr Felt -> Expr Felt) -> Body
withVarF3 (var1, var2, var3) targetVar f = assignVarF targetVar $ f (varF var1) (varF var2) (varF var3)

-- | Shorthand to update a variable value with a computation (which might depend on its current value)
-- It's shorter and easier to write 'updateVarF "cnt" $ \n -> 3 * n + 1' than writing 'assignVarF "cnt" (varF "cnt" * 3 + 1)'
-- especially if we use the variable value several times
updateVarF :: VarName -> (Expr Felt -> Expr Felt) -> Body
-- updateVarF varname f = assignVarF varname (f (varF varname))
updateVarF varname = withVarF varname varname

-- | Shorthand to increment a variable (i += n)
-- It's shorter and easier to write 'incVarF "i" 1' than writing 'assignVarF "i" (varF "i" + 1)`
incVarF :: VarName -> Felt -> Body
-- incVarF varname val = assignVarF varname (varF varname + (Expr . Lit $ val))
incVarF varname val = updateVarF varname (`add` lit val)

-- | Same as above, but for decrementing
decVarF :: VarName -> Felt -> Body
decVarF varname val = updateVarF varname (`sub` lit val)
