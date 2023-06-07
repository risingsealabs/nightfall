module Nightfall.Lang.Internal.Types where

import Data.Word ( Word64 )
import Data.Coerce ( coerce )

-- | Field Element type
type Felt = Word64

type VarName = String

type FunName = String

-- | Expression, internal type, not exposed
data Expr_ =
    -- | Literals
      Lit Felt  -- ^ 309183, 2398713, whatever
    | Bo Bool   -- ^ true/false, 1/0 

    -- | Arithmetic operations
    | Add Expr_ Expr_ -- ^ a + b
    | Sub Expr_ Expr_ -- ^ a - b
    | Mul Expr_ Expr_ -- ^ a * b
    | Div Expr_ Expr_ -- ^ a / b (integer division)
    | Mod Expr_ Expr_ -- ^ a % b

    -- | Boolean operations
    | Equal Expr_ Expr_     -- ^ a == b
    | Not Expr_             -- ^ !a
    | Lower Expr_ Expr_     -- ^ a < b
    | LowerEq Expr_ Expr_   -- ^ a <= b
    | Greater Expr_ Expr_   -- ^ a > b
    | GreaterEq Expr_ Expr_ -- ^ a >= b
    | IsOdd Expr_           -- a `mod` 2 == 1
    
    -- | Variables
    | VarF VarName     -- ^ "calling" a variable of type Felt by its name (e.g. "foo")
    | VarB VarName     -- ^ same, but with boolean variable

    -- | Functions
    | FCall FunName [Expr_]
    deriving (Eq, Show)

-- | Expression wrapper type, typed for safety, exposed to use
newtype Expr a = Expr Expr_
  deriving (Eq, Show)

-- | Num instance to make writings easier, to allow wriring expressions with "+", "-", etc.
instance Num a => Num (Expr a) where
  (+) = add
  (-) = sub
  (*) = mul
  abs = id
  -- fromInteger x = Expr (Lit (fromInteger x))
  fromInteger = Expr . Lit . fromInteger
  signum _ = error "Signum not implemented for Expr"

-- | Simple, internal type, not exposed
data Statement_ =
    -- | Variable declaration
      DeclVariable VarName Expr_  -- ^ let a = 634
    
    -- | Variable assignment
    | AssignVar VarName Expr_     -- ^ a <- 368

    -- | Conditionals
    | IfElse Expr_ [Statement_] [Statement_] -- ^ condition if-block else-block
    | While Expr_ [Statement_]               -- ^ condition body

    -- | Naked function call
    | NakedCall FunName [Expr_]

    -- | Return
    | Return (Maybe Expr_)

    -- | Comment
    | Comment String

    -- | Allow to add empty lines in the generated code, for clarity
    | EmptyLine
    deriving (Eq, Show)

-- | Statement wrapper type, typed for safety, exposed to use
newtype Statement = Statement Statement_
  deriving (Eq, Show)

data ZKProgram = ZKProgram
    { pName :: String            -- ^ Program name, is this needed?
    , pStatements :: [Statement] -- ^ List of statements comprising the program
    , pPublicInputs :: [Felt]    -- ^ Defining the public inputs as a list of field elements for now
    , pSecretInputs :: FilePath  -- ^ For now, for simplicty, we'll be referring to a '.inputs' file
    }

-- * "Smart Constructors" for building (type-safe) @Expr. They are the ones exposed for users to use instead of constructing the
-- types directly.

-- ** Literals

lit :: Word64 -> Expr Felt
lit = Expr . Lit

bool :: Bool -> Expr Bool
bool = Expr . Bo

-- ** Arithmetic operations

add :: Num a => Expr a -> Expr a -> Expr a
add  (Expr e1) (Expr e2) = Expr $ Add e1 e2

sub :: Num a => Expr a -> Expr a -> Expr a
sub (Expr e1) (Expr e2)= Expr $ Sub e1 e2

mul :: Num a => Expr a -> Expr a -> Expr a
mul (Expr e1) (Expr e2) = Expr $ Mul e1 e2

div' :: Num a => Expr a -> Expr a -> Expr a
div' (Expr e1) (Expr e2) = Expr $ Div e1 e2

mod' :: Num a => Expr a -> Expr a -> Expr a
mod' (Expr e1) (Expr e2) = Expr $ Mod e1 e2

-- ** Boolean operations

eq :: Eq a => Expr a -> Expr a -> Expr Bool
eq (Expr e1) (Expr e2)= Expr $ Equal e1 e2

not' :: Expr Bool -> Expr Bool
not' (Expr e) = Expr $ Not e

lt :: Ord a => Expr a -> Expr a -> Expr Bool
lt (Expr e1) (Expr e2)= Expr $ Lower e1 e2

lte :: Ord a => Expr a -> Expr a -> Expr Bool
lte (Expr e1) (Expr e2) = Expr $ LowerEq e1 e2

gt :: Ord a => Expr a -> Expr a -> Expr Bool
gt (Expr e1) (Expr e2) = Expr $ Greater e1 e2

gte :: Ord a => Expr a -> Expr a -> Expr Bool
gte(Expr e1) (Expr e2) = Expr $ GreaterEq e1 e2

isOdd :: Num a => Expr a -> Expr Bool
isOdd (Expr e) = Expr $ IsOdd e

-- ** Variables (typed)
varF :: VarName -> Expr Felt
varF = Expr . VarF

varB :: VarName -> Expr Bool
varB = Expr . VarB

-- Function calls will come later
-- fcall :: FunName -> [Expr] -> Expr
-- fcall = FCall

-- * "Smart Constructors" for building (type-safe) @Statement. They are the ones exposed for users to use

-- ** Variables
declareVarF :: VarName -> Expr Felt -> Statement
declareVarF varname (Expr e) = Statement $ DeclVariable varname e

declareVarB :: VarName -> Expr Bool -> Statement
declareVarB varname (Expr e) = Statement $ DeclVariable varname e

assignVarF :: VarName -> Expr Felt -> Statement
assignVarF varname (Expr e) = Statement $ AssignVar varname e

assignVarB :: VarName -> Expr Bool -> Statement
assignVarB varname (Expr e) = Statement $ AssignVar varname e

ifElse :: Expr Bool -> [Statement] -> [Statement] -> Statement
ifElse (Expr cond) ifBlock elseBlock = Statement $ IfElse cond (coerce ifBlock) (coerce elseBlock)

-- | A constructor for when you don't want 'else' statement
simpleIf :: Expr Bool -> [Statement] -> Statement
simpleIf cond ifBlock = ifElse cond ifBlock []

while :: Expr Bool -> [Statement] -> Statement
while (Expr cond) body = Statement $ While cond (coerce body)

-- nakedCall :: FunName -> [Expr] -> Statement
-- nakedCall = NakedCall

ret :: Maybe (Expr a)-> Statement
ret Nothing = Statement $ Return Nothing
ret (Just (Expr e)) = Statement $ Return (Just e)

comment :: String -> Statement
comment = Statement . Comment

emptyLine :: Statement
emptyLine = Statement EmptyLine