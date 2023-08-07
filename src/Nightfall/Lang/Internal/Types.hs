module Nightfall.Lang.Internal.Types
    ( module Nightfall.Lang.Internal.Types
    , Felt
    , unFelt
    , feltOrder
    , feltOrderInteger
    ) where

import Nightfall.Lang.Internal.Felt

type VarName = String

type FunName = String

-- | Unary operations.
data UnOp =
      Not    -- ^ !a
    | IsOdd  -- a `mod` 2 == 1
    deriving (Eq, Show)

-- | Binary operations.
data BinOp =
    -- Arithmetic operations
      Add     -- ^ a + b
    | Sub     -- ^ a - b
    | Mul     -- ^ a * b
    | Div     -- ^ a / b (integer division)
    | IDiv32  -- ^ a `quot` b with a and b being 'Word32'

    -- Boolean operations
    | Equal      -- ^ a == b
    | Lower      -- ^ a < b
    | LowerEq    -- ^ a <= b
    | Greater    -- ^ a > b
    | GreaterEq  -- ^ a >= b
    deriving (Eq, Show)

-- | Expression, internal type, not exposed
data Expr_ =
    -- | Literals
      Lit Felt  -- ^ 309183, 2398713, whatever
    | Bo Bool   -- ^ true/false, 1/0

    | UnOp UnOp Expr_
    | BinOp BinOp Expr_ Expr_

    -- | Variables
    | VarF VarName     -- ^ "calling" a variable of type Felt by its name (e.g. "foo")
    | VarB VarName     -- ^ same, but with boolean variable

    -- | Functions
    | FCall FunName [Expr_]

    -- | Secret Input
    | NextSecret       -- ^ The next available secret input
    deriving (Eq, Show)

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
    | Return Expr_

    -- | Comment
    | Comment String

    -- | Allow to add empty lines in the generated code, for clarity
    | EmptyLine
    deriving (Eq, Show)
