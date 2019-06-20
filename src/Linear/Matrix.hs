module Linear.Matrix (
    M44(..),
    zero,
    identity,
    scale,
    mult,
    toList
  ) where

import Linear.Vector(vadd, smult)
import Linear.V2
import Linear.V3
import Linear.V4
import Data.Traversable(mapAccumL)
import Linear.Transposable
import Data.Foldable(foldl')
import qualified Data.Foldable as F(toList)
import Control.Applicative(liftA2)

type M44 a = V4 (V4 a)

zero :: (Applicative t, Num a) => t (t a)
zero = pure $ pure 0

identity :: (Traversable t, Applicative t, Num a) => t (t a)
identity = scale $ pure 1

scale :: (Traversable t, Applicative t, Num a) => t a -> t (t a)
scale v = iter (\i x -> iter (\j _ -> if i == j then x else 0) v) v

mult :: (Functor t, Foldable m, Applicative m, Applicative n, Num a) => t (m a) -> m (n a) -> t (n a)
mult f g = fmap (\f' -> foldl' vadd (pure 0) $ liftA2 smult f' g) f

iter :: (Traversable t, Applicative t) => (Int -> a -> b) -> t a -> t b
iter f = snd . mapAccumL (\i e -> (i+1, f i e)) 0

toList :: Foldable t => t (t a) -> [a]
toList = concatMap (F.toList)
