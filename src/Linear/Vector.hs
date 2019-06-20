module Linear.Vector (
    vadd,
    vsub,
    vneg,
    smult,
    sdiv,
    length,
    normalize,
    dot,
    module Linear.V2,
    module Linear.V3,
    module Linear.V4
  ) where

import Control.Applicative(liftA2)
import Data.Foldable(foldl')
import Prelude hiding (length)

import Linear.V2
import Linear.V3
import Linear.V4

vadd :: (Applicative m, Num a) => m a -> m a -> m a
vadd = liftA2 (+)

vsub :: (Applicative m, Num a) => m a -> m a -> m a
vsub = liftA2 (-)

vneg :: (Functor m, Num a) => m a -> m a
vneg = fmap negate

smult :: (Functor m, Num a) => a -> m a -> m a
smult a = fmap (*a)

sdiv :: (Functor m, Fractional a) => a -> m a -> m a
sdiv a = fmap (/a)

length :: (Foldable m, Applicative m, Floating a) => m a -> a
length v = sqrt $ dot v v

normalize :: (Foldable m, Applicative m, Floating a) => m a -> m a
normalize v = length v `sdiv` v

dot :: (Foldable m, Applicative m, Num a) => m a -> m a -> a
dot v1 v2 = foldl' (+) 0 $ liftA2 (*) v1 v2
