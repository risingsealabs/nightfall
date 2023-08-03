{-# LANGUAGE TypeOperators #-}

module Nightfall.Lang.Types ( Felt
                            , VarName
                            , FunName
                            , Expr
                            , Statement(..)
                            , ZKProgram(..)
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

import Data.Word (Word32)
import Data.Coerce (coerce)

-- | Expression wrapper type, typed for safety, exposed to use
newtype Expr a = Expr Expr_
  deriving (Eq, Show)

-- | Num instance to make writings easier, to allow wriring expressions with "+", "-", etc.
instance a ~ Felt => Num (Expr a) where
  (+) = add
  (-) = sub
  (*) = mul
  fromInteger = Expr . Lit . fromInteger
  abs = error "'abs' not implemented for 'Expr'"
  signum = error "'signum' not implemented for 'Expr'"

-- | Statement wrapper type, typed for safety, exposed to use
newtype Statement = Statement Statement_
  deriving (Eq, Show)

data ZKProgram = ZKProgram
    { pName :: String            -- ^ Program name, is this needed?
    , pStatements :: [Statement] -- ^ List of statements comprising the program
    , pPublicInputs :: [Felt]    -- ^ Defining the public inputs as a list of field elements for now
    , pSecretInputs :: FilePath  -- ^ For now, for simplicty, we'll be referring to a '.inputs' file
    }

-- | Helper to quickly make a simple @ZKProgram from a list of statements, no inputs
mkSimpleProgram :: String -> [Statement] -> ZKProgram
mkSimpleProgram name stmts = ZKProgram
    { pName = name
    , pStatements = stmts
    , pPublicInputs = []
    , pSecretInputs = ""
    }

-- | Helper to build a @ZKPeogram
mkZKProgram :: String -> [Statement] -> [Felt] -> FilePath -> ZKProgram
mkZKProgram name stmts pubs secretFP = ZKProgram
    { pName = name
    , pStatements = stmts
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

-- ** A few helpers for common code patterns

-- | Apply a function of 1 argument over @var and store result in @targetVar
withVarF :: VarName -> VarName -> (Expr Felt -> Expr Felt) -> Statement
withVarF var targetVar f = assignVarF targetVar $ f . varF $ var

-- | Apply a function of 2 arguments over two variables and store result in @targetVar
withVarF2 :: (VarName, VarName) -> VarName -> (Expr Felt -> Expr Felt -> Expr Felt) -> Statement
withVarF2 (var1, var2) targetVar f = assignVarF targetVar $ f (varF var1) (varF var2)

-- | Apply a function of 3 arguments over three variables and store result in @targetVar
withVarF3 :: (VarName, VarName, VarName) -> VarName -> (Expr Felt -> Expr Felt -> Expr Felt -> Expr Felt) -> Statement
withVarF3 (var1, var2, var3) targetVar f = assignVarF targetVar $ f (varF var1) (varF var2) (varF var3)

-- | Shorthand to update a variable value with a computation (which might depend on its current value)
-- It's shorter and easier to write 'updateVarF "cnt" $ \n -> 3 * n + 1' than writing 'assignVarF "cnt" (varF "cnt" * 3 + 1)'
-- especially if we use the variable value several times
updateVarF :: VarName -> (Expr Felt -> Expr Felt) -> Statement
-- updateVarF varname f = assignVarF varname (f (varF varname))
updateVarF varname = withVarF varname varname

-- | Shorthand to increment a variable (i += n)
-- It's shorter and easier to write 'incVarF "i" 1' than writing 'assignVarF "i" (varF "i" + 1)`
incVarF :: VarName -> Felt -> Statement
-- incVarF varname val = assignVarF varname (varF varname + (Expr . Lit $ val))
incVarF varname val = updateVarF varname (`add` lit val)

-- | Same as above, but for decrementing
decVarF :: VarName -> Felt -> Statement
decVarF varname val = updateVarF varname (`sub` lit val)
