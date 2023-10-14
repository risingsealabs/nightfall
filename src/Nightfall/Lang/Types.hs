{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Nightfall.Lang.Types ( Felt
                            , MidenWord(..)
                            , VarName
                            , FunName
                            , Statement_
                            , Expr(..)
                            , ZKProgram(..)
                            , BodyF(..)
                            , Body(..)
                            , SecretInputs(..)
                            , emptySecretInputs
                            , runBody
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
                            , getAt
                            , setAt
                            , statement
                            , statements
                            -- , fcall
                            , nextSecret
                            , ifElse
                            , simpleIf
                            , while
                            , initArray
                            -- , nakedCall
                            , ret
                            , comment
                            , emptyLine
                            ) where

import Nightfall.Lang.Internal.Types

import Control.Monad.Free.Church
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word (Word32)
import qualified Data.Map.Strict as Map

-- | Expression wrapper type, typed for safety, exposed to use
newtype Expr a = Expr
    { unExpr :: Expr_
    } deriving (Eq, Show)

class IsBuiltin a where
    mkConstant :: a -> Constant

instance IsBuiltin Felt where
    mkConstant = ConstantFelt

constant :: IsBuiltin a => a -> Expr a
constant = Expr . Constant . mkConstant

-- | Num instance to make writings easier, to allow wriring expressions with "+", "-", etc.
instance a ~ Felt => Num (Expr a) where
    (+) = add
    (-) = sub
    (*) = mul
    fromInteger = constant . fromInteger
    abs = error "'abs' not implemented for 'Expr'"
    signum = error "'signum' not implemented for 'Expr'"

data BodyF a = BodyF Statement_ a
    deriving (Eq, Show, Functor)

-- | A 'Body' represents a list of 'Statement_'s. We do not simply use @[Statement_]@, because
--
-- 1. a 'Body' can be constructed using the do notation, which is much nicer than the list syntax
-- 2. concatenation of 'Body's is more efficient than concatentation of lists, because 'Body' is
--    implemented as a Church-encoded free monad (which is to the regular free monad what @DList@ is
--    to @[]@).
newtype Body a = Body
    { unBody :: F BodyF a
    } deriving newtype (Functor, Applicative, Monad)

data SecretInputs = SecretInputs
    { _adviceStack :: [Felt],
      _adviceMap :: Map Text [MidenWord]
    } deriving (Eq, Ord, Show)

emptySecretInputs :: SecretInputs
emptySecretInputs = SecretInputs [] Map.empty

data ZKProgram = ZKProgram
    { pName :: String
      -- ^ Program name, useful for tests.
    , pBody :: Body ()
      -- ^ List of statements comprising the program
    , pPublicInputs :: [Felt]
      -- ^ Defining the public inputs as a list of field elements for now
    , pSecretInputs :: Either SecretInputs FilePath
      -- ^ Either a list with private inputs or an inputs file path.
      -- TODO: make it either just 'SecretInputs' or at least @IO SecretInputs@ or something.
      -- 'FilePath' doesn't belong there.
      -- TODO: is there a reason to merge public and secret inputs into a single data type?
    }

-- | Helper to quickly make a simple @ZKProgram@ from a list of statements, no inputs
mkSimpleProgram :: String -> Body () -> ZKProgram
mkSimpleProgram name body = ZKProgram
    { pName = name
    , pBody = body
    , pPublicInputs = []
    , pSecretInputs = Left emptySecretInputs
    }

-- | Helper to build a @ZKProgram@.
mkZKProgram :: String -> Body () -> [Felt] -> FilePath -> ZKProgram
mkZKProgram name body pubs secretFP = ZKProgram
    { pName = name
    , pBody = body
    , pPublicInputs = pubs
    , pSecretInputs = Right secretFP
    }

-- * "Smart Constructors" for building (type-safe) @Expr. They are the ones exposed for users to use
-- instead of constructing the types directly.

-- ** Literals

lit :: Felt -> Expr Felt
lit = Expr . Lit

bool :: Bool -> Expr Bool
bool = Expr . Bo

cast128to256 :: Expr (UInt 128) -> Expr (UInt 256)
cast128to256 (Expr e) = Expr e

-- ** Arithmetic operations

add :: Expr Felt -> Expr Felt -> Expr Felt
add  (Expr e1) (Expr e2) = Expr $ BinOp Add e1 e2

sub :: Expr Felt -> Expr Felt -> Expr Felt
sub (Expr e1) (Expr e2) = Expr $ BinOp Sub e1 e2

mul :: Expr Felt -> Expr Felt -> Expr Felt
mul (Expr e1) (Expr e2) = Expr $ BinOp Mul e1 e2

div' :: Expr Felt -> Expr Felt -> Expr Felt
div' (Expr e1) (Expr e2) = Expr $ BinOp Div e1 e2

idiv32 :: Expr Word32 -> Expr Word32 -> Expr Word32
idiv32 (Expr e1) (Expr e2) = Expr $ BinOp IDiv32 e1 e2

-- ** Boolean operations

eq :: Expr Felt -> Expr Felt -> Expr Bool
eq (Expr e1) (Expr e2) = Expr $ BinOp Equal e1 e2

not' :: Expr Bool -> Expr Bool
not' (Expr e) = Expr $ UnOp Not e

lt :: Expr Felt -> Expr Felt -> Expr Bool
lt (Expr e1) (Expr e2) = Expr $ BinOp Lower e1 e2

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
varF = Expr . Var

varB :: VarName -> Expr Bool
varB = Expr . Var

getAt :: VarName -> Expr Felt -> Expr Felt
getAt var (Expr i) = Expr $ GetAt var i

-- Function calls will come later
-- fcall :: FunName -> [Expr] -> Expr
-- fcall = FCall

-- * Inputs

-- ** Secret input

nextSecret :: Expr a
nextSecret = Expr NextSecret

-- * "Smart Constructors" for building (type-safe) @Statement. They are the ones exposed for users to use

-- | Turn a 'Statement_' into a 'Body' representing the statement.
statement :: Statement_ -> Body ()
statement stmt = Body . liftF $ BodyF stmt ()

-- | Turn a list of 'Statement_'s into a 'Body' representing the statements.
statements :: [Statement_] -> Body ()
statements = traverse_ statement

-- | Turn a 'Body' into the list of 'Statement_'s that it represents.
runBody :: Body () -> [Statement_]
runBody body = runF (unBody body) (const []) $ \(BodyF stmt stmts) -> stmt : stmts

setAt :: VarName -> Expr Felt -> Expr Felt -> Body ()
setAt var (Expr i) (Expr val) = statement $ SetAt var i val

ifElse :: Expr Bool -> Body () -> Body () -> Body ()
ifElse (Expr cond) ifBlock elseBlock = statement $ IfElse cond (runBody ifBlock) (runBody elseBlock)

-- | A constructor for when you don't want 'else' statement
simpleIf :: Expr Bool -> Body () -> Body ()
simpleIf cond ifBlock = ifElse cond ifBlock $ pure ()

while :: Expr Bool -> Body () -> Body ()
while (Expr cond) body = statement $ While cond (runBody body)

initArray :: VarName -> [Felt] -> Body ()
initArray var = statement . InitArray var

-- nakedCall :: FunName -> [Expr] -> Body ()
-- nakedCall = NakedCall

ret :: Expr a -> Body ()
ret = statement . Return . unExpr

comment :: String -> Body ()
comment = statement . Comment

emptyLine :: Body ()
emptyLine = statement EmptyLine
