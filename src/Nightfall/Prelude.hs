{-# LANGUAGE ScopedTypeVariables #-}

module Nightfall.Prelude
     ( module Export
     , module Nightfall.Prelude
     ) where

-- base
import Control.Applicative as Export
import Control.Exception as Export
import Control.Monad as Export
import Control.Monad.IO.Class as Export
import Data.Bifunctor as Export
import Data.Bool as Export
import Data.Char as Export
import Data.Coerce as Export
import Data.Complex as Export
import Data.Either as Export
import Data.Foldable as Export
import Data.Function as Export
import Data.Functor as Export
import Data.Functor.Compose as Export
import Data.Functor.Identity as Export
import Data.List as Export
import Data.Maybe as Export
import Data.Monoid as Export hiding (First (..), Last (..))
import Data.Ord as Export
import Data.Proxy as Export
import Data.Ratio as Export
import Data.Semigroup as Export
import Data.String as Export
import Data.Traversable as Export
import Data.Tuple as Export
import Data.Void as Export
import Debug.Trace as Export
import GHC.Exts as Export (groupWith, sortWith)
import GHC.Generics as Export (Generic, Generic1)
import GHC.Stack as Export
import Numeric as Export
import System.IO as Export
import Text.Read as Export (Read (..), readEither, readMaybe)

-- containers
import Data.IntMap.Strict as Export (IntMap)
import Data.IntSet as Export (IntSet)
import Data.Map.Strict as Export (Map)
import Data.Set as Export (Set)

-- text
import Data.Text as Export (Text)

-- lens
import Control.Lens as Export (makeLenses)

infixr 8 ^:
(^:) :: Integral a => a -> Int -> a
(^:) = (^)

-- | Applicatively fold a 'Foldable'.
foldMapA
    :: forall b m f a. (Monoid b, Applicative m, Foldable f)
    => (a -> m b) -> f a -> m b
foldMapA = coerce (foldMap :: (a -> Ap m b) -> f a -> Ap m b)

foldFor
    :: forall b m f a. (Monoid b, Applicative m, Foldable f)
    => f a -> (a -> m b) -> m b
foldFor = flip foldMapA
