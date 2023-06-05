module Nightfall.Lang.Internal.Types ( Felt
                            , VarName
                            , FunName
                            , Expr(..)
                            , Statement(..)
                            , ZKProgram(..)
                            , lit
                            , bool
                            , add
                            , sub
                            , mul
                            , div'
                            , mod'
                            , eq
                            , not'
                            , lt
                            , lte
                            , gt
                            , gte
                            , var
                            , fcall
                            , declareVar
                            , ifElse
                            , simpleIf
                            , nakedCall
                            , ret
                            , comment
                            , emptyLine
                            ) where

import Data.Word ( Word64 )

-- | Field Element type
type Felt = Word64

type VarName = String

type FunName = String

-- | Simple expression type, quite unsafe at the moment
data Expr =
    -- | Literals
      Lit Felt  -- ^ 309183, 2398713, whatever
    | Bo Bool   -- ^ true/false, 1/0 

    -- | Arithmetic operations
    | Add Expr Expr -- ^ a + b
    | Sub Expr Expr -- ^ a - b
    | Mul Expr Expr -- ^ a * b
    | Div Expr Expr -- ^ a / b (integer division)
    | Mod Expr Expr -- ^ a % b

    -- | Boolean operations
    | Equal Expr Expr     -- ^ a == b
    | Not Expr            -- ^ !a
    | Lower Expr Expr     -- ^ a < b
    | LowerEq Expr Expr   -- ^ a <= b
    | Greater Expr Expr   -- ^ a > b
    | GreaterEq Expr Expr -- ^ a >= b
    
    -- | Variable
    | Var VarName     -- ^ "calling" a variable by its name (e.g. "foo")

    -- | Functions
    | FCall FunName [Expr]
    deriving (Eq, Show)
  
-- | Num instance to make writing easier. Yes I know this makes little sense since @Expr is not type-safe
instance Num Expr where
  (+) = Add
  (*) = Mul
  abs e = e
  signum = error "Signum not implemented for Expr"
  -- fromInteger = error "fromInteger not implemented for Expr"
  fromInteger = lit . fromInteger
  (-) = Sub

-- | Simple statement type, also unsafe and very basic
data Statement =
    -- | Variable declaration
      DeclVariable VarName Expr  -- ^ let a = 634

    -- | Conditionals
    | IfElse Expr [Statement] [Statement] -- ^ condition if-block else-block

    -- | Naked function call
    | NakedCall FunName [Expr]

    -- | Return
    | Return (Maybe Expr)

    -- | Comment
    | Comment String

    -- | Allow to add empty lines in the generated code, for clarity
    | EmptyLine
    deriving (Eq, Show)

data ZKProgram = ZKProgram
    { pName :: String            -- ^ Program name, is this needed?
    , pStatements :: [Statement] -- ^ List of statements comprising the program
    , pPublicInputs :: [Felt]    -- ^ Defining the public inputs as a list of field elements for now
    , pSecretInputs :: FilePath  -- ^ For now, for simplicty, we'll be referring to a '.inputs' file
    }

-- * "Smart Constructors" for building (type-safe) @Expr. They are the ones exposed for users to use
-- NOTE: most of them are still unsafe at this point, this is more a placeholder for when we switch to
-- a better structure, (internal, unexposed 'Expr_' type, then newtype Expr a, etc.)
lit :: Word64 -> Expr
lit = Lit

bool :: Bool -> Expr
bool = Bo

add :: Expr -> Expr -> Expr
add  = Add

sub :: Expr -> Expr -> Expr
sub = Sub

mul :: Expr -> Expr -> Expr
mul = Mul

div' :: Expr -> Expr -> Expr
div' = Div

mod' :: Expr -> Expr -> Expr
mod' = Mod

eq :: Expr -> Expr -> Expr
eq = Equal

not' :: Expr -> Expr
not' = Not

lt :: Expr -> Expr -> Expr
lt = Lower

lte :: Expr -> Expr -> Expr
lte = LowerEq

gt :: Expr -> Expr -> Expr
gt = Greater

gte :: Expr -> Expr -> Expr
gte = GreaterEq

var :: VarName -> Expr
var = Var

fcall :: FunName -> [Expr] -> Expr
fcall = FCall

-- * "Smart Constructors" for building (type-safe) @Statement. They are the ones exposed for users to use
-- NOTE: same as above: it's trivial right now, and not really type safe or anything. It's just placeholder
declareVar :: VarName -> Expr -> Statement
declareVar = DeclVariable

ifElse :: Expr -> [Statement] -> [Statement] -> Statement
ifElse = IfElse

-- | A constructor for when you don't want 'else' statement
simpleIf :: Expr -> [Statement] -> Statement
simpleIf cond body = IfElse cond body []

nakedCall :: FunName -> [Expr] -> Statement
nakedCall = NakedCall

ret :: Maybe Expr -> Statement
ret = Return

comment :: String -> Statement
comment = Comment

emptyLine :: Statement
emptyLine = EmptyLine