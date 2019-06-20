module Linear.V3 (
    V3(..),
    cross,
    fromV2
  ) where

import Linear.V2(V2(..))
import Linear.Transposable(Transposable(..))
import Foreign.Storable(Storable(..))
import Foreign.Ptr(castPtr)

data V3 a = V3 !a !a !a deriving Eq

instance Show a => Show (V3 a) where
  show (V3 x y z) = "(" ++ show x ++ " " ++ show y ++ " " ++ show z ++ ")"

instance Functor V3 where
  f `fmap` V3 x y z = V3 (f x) (f y) (f z)

instance Applicative V3 where
  pure x = V3 x x x
  V3 fx fy fz <*> V3 x y z = V3 (fx x) (fy y) (fz z)

instance Foldable V3 where
  foldr op ini (V3 x y z) = op x (op y (op z ini))

instance Traversable V3 where
  traverse f (V3 x y z) = V3 <$> f x <*> f y <*> f z

instance Transposable V3 where
  transpose m = V3 (fmap (\(V3 x _ _) -> x) m) (fmap (\(V3 _ y _) -> y) m) (fmap (\(V3 _ _ z) -> z) m)

instance Storable a => Storable (V3 a) where
  sizeOf (V3 x _ _) = 3 * sizeOf x
  alignment (V3 x _ _) = alignment x
  peek ptr = V3 <$> peek ptr' <*> peekElemOff ptr' 1 <*> peekElemOff ptr' 2
    where ptr' = castPtr ptr
  poke ptr (V3 x y z) = do
      poke ptr' x
      pokeElemOff ptr' 1 y
      pokeElemOff ptr' 2 z
    where ptr' = castPtr ptr
              
cross :: Num a => V3 a -> V3 a -> V3 a
cross (V3 ax ay az) (V3 bx by bz) = V3 (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)

fromV2 :: V2 a -> a -> V3 a
fromV2 (V2 x y) z = V3 x y z
