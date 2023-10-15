{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Nightfall.Lang.Internal.Types
    ( module Nightfall.Lang.Internal.Types
    , Felt
    , unFelt
    , feltOrder
    , feltOrderInteger
    ) where

import Data.String
import Data.Word
import GHC.Natural
import Nightfall.Lang.Internal.Felt
import Text.Printf (printf)

{- Note [The design of arrays]
Currently we support three operations:

1. 'InitArray' taking the name of an array and a list of all its elements
2. 'GetAt' taking the name of an array and an index in it and returning the element at the index
3. 'SetAt' taking the name of an array, an index in it and a value to put at the index

In the Miden backend each cell in an array is backed by an entire 'MidenWord' even though we store a
single 'Felt' there (just like with variables), i.e. each cell of an array takes 4 times more memory
than what it actually needs (the unneeded parts are zeroed out).

We have two reasons for this design:

1. it's simpler to arrange things this way, particularly given that variables had already worked
   like that when support for arrays was added
2. we don't just waste memory, we trade it for performance of the final Miden program, since
   accessing a subword that isn't at the beginning of a word is expensive. See this issue for
   details: https://github.com/0xPolygonMiden/miden-vm/issues/1064

So even if we had "dense" arrays, it would still make sense to use the current "sparse" ones in
certain situations (particularly for small arrays), hence we decided to implement the
simple-and-still-useful thing first and only add support for dense arrays once we actually need it.
-}

type VarName = String

type FunName = String

data VarType =
      VarFelt
    | VarBool
    | VarArrayOfFelt Word32  -- The argument is the length of the array.
    deriving (Eq, Show)

ppVarType :: VarType -> String
ppVarType VarFelt              = "felt"
ppVarType VarBool              = "bool"
ppVarType (VarArrayOfFelt len) = "[felt; " ++ show len ++ "]"

isArrayOfFelt :: VarType -> Bool
isArrayOfFelt VarArrayOfFelt{} = True
isArrayOfFelt _                = False

-- | Unary operations.
data UnOp =
      Not    -- ^ @!a@
    | IsOdd  -- ^ @a `mod` 2 == 1@
    deriving (Eq, Show)

-- | Binary operations.
data BinOp =
    -- Arithmetic operations
      Add     -- ^ @a + b@
    | Sub     -- ^ @a - b@
    | Mul     -- ^ @a * b@
    | Div     -- ^ @a / b@ (integer division)
    | IDiv32  -- ^ @a `quot` b@ with @a and b@ being 'Word32'

    -- Boolean operations
    | Equal      -- ^ @a == b@
    | Lower      -- ^ @a < b@
    | LowerEq    -- ^ @a <= b@
    | Greater    -- ^ @a > b@
    | GreaterEq  -- ^ @a >= b@
    deriving (Eq, Show)

data Literal =
      LiteralFelt Felt
    | LiteralBool Bool
    deriving (Eq, Show)

-- | Expression, internal type, not exposed
data Expr_ =
      Literal Literal

    | UnOp UnOp Expr_
    | BinOp BinOp Expr_ Expr_

    | Var VarName

    | GetAt VarName Expr_

    -- | Functions
    | FCall FunName [Expr_]

    -- | Secret Input
    | NextSecret       -- ^ The next available secret input
    deriving (Eq, Show)

-- | Simple, internal type, not exposed
data Statement_ =
    -- | Variable declaration
      DeclVariable VarType VarName Expr_  -- ^ let a = 634
    
    -- | Variable assignment
    | AssignVar VarName Expr_     -- ^ a <- 368

    | SetAt VarName Expr_ Expr_

    -- | Conditionals
    | IfElse Expr_ [Statement_] [Statement_] -- ^ condition if-block else-block
    | Repeat Natural [Statement_]
    | While Expr_ [Statement_]               -- ^ condition body

    -- | Naked function call
    | NakedCall FunName [Expr_]

    | InitArray VarName [Felt]

    -- | Return
    | Return Expr_

    -- | Comment
    | Comment String

    -- | Allow to add empty lines in the generated code, for clarity
    | EmptyLine
    deriving (Eq, Show)

-- | What Miden considers to be a Word.
data MidenWord = MidenWord
    { _midenWord0 :: Felt
    , _midenWord1 :: Felt
    , _midenWord2 :: Felt
    , _midenWord3 :: Felt
    } deriving (Eq, Ord, Show)

midenWordToList :: MidenWord -> [Felt]
midenWordToList (MidenWord x0 x1 x2 x3) = [x0, x1, x2, x3]

-- | Convert a list of 'Felt's into a list of 'MidenWord's by padding each of the 'Felt's with
-- zeros.
padListAsWords :: [Felt] -> [MidenWord]
padListAsWords = map $ \x0 -> MidenWord x0 0 0 0

-- | Parse a list of 'Felt's as a 'MidenWord'.
listToMidenWord :: [Felt] -> Maybe MidenWord
listToMidenWord [x0, x1, x2, x3] = Just $ MidenWord x0 x1 x2 x3
listToMidenWord _                = Nothing

{- Note [The encoding of keys in advice_map]
Encoding of keys in an @advice_map@ happens at
    https://github.com/0xPolygonMiden/miden-vm/blob/105ee9e241fafeac97fbcd14861b2eaed5548e1e/processor/src/advice/providers.rs#L92

Inlining the code here:

    AdviceSource::Map { key, include_len } => {
        let values = self
            .map
            .get(&key.into_bytes())
            .ok_or(ExecutionError::AdviceKeyNotFound(key))?;

@into_bytes@ is defined at
   https://github.com/0xPolygonMiden/miden-vm/blob/105ee9e241fafeac97fbcd14861b2eaed5548e1e/core/src/utils/mod.rs#L49

Inlining the code here:

    impl IntoBytes<32> for [Felt; 4] {
        fn into_bytes(self) -> [u8; 32] {
            let mut result = [0; 32];

            result[..8].copy_from_slice(&self[0].as_int().to_le_bytes());
            result[8..16].copy_from_slice(&self[1].as_int().to_le_bytes());
            result[16..24].copy_from_slice(&self[2].as_int().to_le_bytes());
            result[24..].copy_from_slice(&self[3].as_int().to_le_bytes());

            result
        }
    }

where @to_le_bytes@ is a library function:
    https://doc.rust-lang.org/std/primitive.u32.html#method.to_le_bytes

An example @advice_map@ can be found at
    https://github.com/0xPolygonMiden/examples/blob/df0eeeeac29eab0e5e3d5cbe06921e372a189039/examples/advice_provider.inputs#L4

which is used in
    https://github.com/0xPolygonMiden/examples/blob/df0eeeeac29eab0e5e3d5cbe06921e372a189039/examples/advice_provider.masm

with some very helpful comments, in particular

    # push the key for the advice map onto the stack ("0000001000000000000000000000000000000000000000000000000000000000")
    push.268435456.0.0.0

and

    # assert the word is what we would expect ("0000000000000000000000000000000333000000000000000000000000000000")
    push.0.216172782113783808.51.0 assert_eqw

So what it all means is that a Miden Word is encoded 'Felt'-by-'Felt' sequentially with every 'Felt'
encoded as a little-endian hexadecimal number (irrespective of the endianness used by the specific
machine, since @to_le_bytes@ always returns a little-endian hexadecimal number).

We need to replicate this behavior in order to be able to put elements into @advice_map@s.

Before we start, if you're not familiar with endianness issues, this is a great Rust-guided intro:
    https://www.thecodedmessage.com/posts/endian_polymorphism/

We normally write numbers using big endianness. E.g. 1234 is one thousand two hunder thirty four.
How this number gets represented in the memory depends on the machine and doesn't matter for us,
because @to_le_bytes@ always return a little-endian one and so we should too, regardless of the
memory representation.

If we 'printf' a number in hexadecimal format, we'll get a big-endian output:

    >>> printf "%08x" (1234567890 :: Word32)
    499602d2

just like if we 'printf' a number in decimal format, we'll get back our original big-endian number:

    >>> printf "%u" (1234567890 :: Word32)
    1234567890

But we want little-endianness, i.e. we want the bytes (a sequence of two digits) in @499602d2@ to
be reversed such that we get @d2029649@. We could achieve that by 'printf'ing a big-endian number,
then parsing it and reversing the bytes manually, but a simpler thing is to just reverse the bytes
in the memory using a built-in function, so that when it's 'printf'ed as a big-endian number we
actually get a little-endian one:

    >>> printf "%08x" $ byteSwap32 (1234567890 :: Word32)
    d2029649

Note that the memory representation doesn't matter: it can be big endian or little endian, the trick
will work regardless. It's because 'printf' gives us a big-endian number regardless of the memory
representation and so 'printf'ing a byte-swapped number will give us a little-endian one, also
regardless of the memory representation.

We used 'Word32' throughout this elaboration, because that makes it easier to visually discern
numbers. In the actual code it's @printf "%016x"@ and 'byteSwap64', since 'Felt' is 'Word64'-backed.
-}

-- | Convert a 'MidenWord' to a hex representation of keys from @advice_map@.
midenWordToHexKey :: IsString str => MidenWord -> str
midenWordToHexKey
    = fromString
    . concatMap (printf "%016x" . byteSwap64 . fromIntegral . unsafeUnFelt)
    . midenWordToList
