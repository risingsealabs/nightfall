{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Nightfall.Lang.Internal.Felt where

import Data.Word (Word64)
import Data.Coerce
import GHC.Generics
import Data.Typeable
import qualified Data.Field.Galois as Galois
import GHC.TypeNats
import Data.Ratio

{- Note [Handling on integral types]
Without completely paranoic handling of integral types it is trivial to cause an overflow, which may
easily result in a vulnerability. We therefore 'newtype'-wrap every integral type (including the
type of field elements), specify its range (sometimes as right at the type level, sometimes as a
comment) and discourage usage of functions that operate on the underlying representation by
attaching the "unsafe" prefix to their name.

Any wrapping is only considered safe when either any input is handled correctly or bounds are
checked and 'Nothing' is returned if the provided value doesn't fit. Thus both throwing and implicit
conversions between types of different cardinality are considered unsafe, although between the two
unsafe options the former is highly preferable.

Any unwrapping is only considered safe as long as the result is converted to 'Integer' before the
unwrapping function returns, so that the user cannot accidentally cause an overflow using the
result of the unwrapping function and subsequently "safely" embed that overflow back into the
integral type.
-}

-- TODO: given that 'Felt' doesn't cover the entire 'Word64' anyway, should we simply use 'Natural'
-- here? That would be more efficient, since 'Galois.Prime' is a wrapper around 'Natural' and we
-- derive a lot of operations using that wrapper. We can't do better than 'Natural' performance-wise
-- anyway, because we need to be able to handle, say, @(2 * Felt (maxBound :: Word64))@ somehow,
-- i.e. an intermediate step can go well beyond 'Word64' before the modulo operation is executed and
-- the result is back within 'Word64'.
--
-- Plus, having a 'Natural' here is simply safer as it doesn't pose any risk of causing an
-- overflow. On the other hand there's still risk of underflow, but at least that causes an
-- exception with 'Natural'. 'Integer' perhaps would be the safest and the slowest option. Although
-- given Note [Handling on integral types] we should be able to afford the minor unsafety of
-- 'Natural' here, since we're sufficiently paranoic when it comes to constructing and
-- deconstructing 'Felt's.
-- | The type of finite field that the Miden VM operates on.
--
-- Note that it's extremely unsafe to use the constructor directly since 'Word64' is prone to
-- overflows, here's an example:
--
-- >>> let hundredOnes = replicate 100 '1'
-- >>> Felt $ read hundredOnes
-- Felt 8198552921648689607
-- >>> toFelt $ read hundredOnes
-- Felt 9806726161887342870
--
-- Plus, Miden's 'Felt' doesn't include the entire range of 'Word64', which is another reason why
-- using the constructor directly is extremely unsafe.
--
-- Hence always use 'toFelt' (or, equivalently, 'fromInteger' or numeric syntax) unless you can
-- prove that no overflow can happen (and even in that case perhaps still use 'toFelt'). Do not
-- reexport the constructor from any other module.
--
-- Invariant:
--
-- > forall (i :: Felt). 0 <= i && i < feltOrder
newtype Felt = Felt Word64
    deriving (Eq, Ord, Show, Generic, Typeable)

-- | The order (a.k.a. size) of the 'Felt' field.
type FeltOrder = 2 ^ 64 - 2 ^ 32 + 1

-- | Convert a 'Felt' element to the corresponding 'Galois.Prime' element. Useful for defining
-- operations over 'Felt' that are already defined over 'Galois.Prime'.
feltToPrime :: Felt -> Galois.Prime FeltOrder
feltToPrime = coerce (fromIntegral :: Word64 -> Galois.Prime FeltOrder)
{-# INLINE feltToPrime #-}

-- | Convert a 'Galois.Prime' element to the corresponding 'Felt' element. Useful for defining
-- operations over 'Felt' that are already defined over 'Galois.Prime'.
primeToFelt :: Galois.Prime FeltOrder -> Felt
primeToFelt = coerce (fromIntegral :: Galois.Prime FeltOrder -> Word64)
{-# INLINE primeToFelt #-}

instance Num Felt where
    fromInteger = toFelt
    {-# INLINE fromInteger #-}

    i + j = primeToFelt $ feltToPrime i + feltToPrime j
    {-# INLINE (+) #-}

    i * j = primeToFelt $ feltToPrime i * feltToPrime j
    {-# INLINE (*) #-}

    i - j = primeToFelt $ feltToPrime i - feltToPrime j
    {-# INLINE (-) #-}

    abs = error "'abs' is not defined for 'Felt'"
    signum = error "'signum' is not defined for 'Felt'"

instance Fractional Felt where
    i / j = primeToFelt $ feltToPrime i / feltToPrime j
    {-# INLINE (/) #-}

    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
    {-# INLINE fromRational #-}

-- | Convert a 'Felt' to the corresponding 'Integer'.
unFelt :: Felt -> Integer
unFelt = coerce (fromIntegral :: Word64 -> Integer)
{-# INLINE unFelt #-}

-- See Note [Handling on integral types].
-- | Unwrap a 'Felt' to get the underlying 'Word64'. This is only unsafe in the sense that
-- performing operations over the resulting 'Word64' may result in overflow and hence usage of
-- 'unsafeUnFelt' is discouraged. Use 'unFelt' whenever possible instead.
unsafeUnFelt :: Felt -> Word64
unsafeUnFelt = coerce
{-# INLINE unsafeUnFelt #-}

-- | The order (a.k.a. size) of the 'Felt' field as a 'Word64'.
feltOrder :: Word64
feltOrder = fromIntegral . natVal $ Proxy @FeltOrder
-- No point in inlining this definition, it would be neither efficient nor readable in the generated
-- Core.
{-# NOINLINE feltOrder #-}

-- | The order (a.k.a. size) of the 'Felt' field as an 'Integer', provided for solely for the
-- purpose of not recomputing the 'Integer' version of 'feltOrder'.
feltOrderInteger :: Integer
feltOrderInteger = fromIntegral feltOrder
{-# NOINLINE feltOrderInteger #-}

-- | Convert an 'Integer' of an arbitrary size to a 'Felt'.
toFelt :: Integer -> Felt
toFelt = primeToFelt . fromInteger
{-# INLINE toFelt #-}
