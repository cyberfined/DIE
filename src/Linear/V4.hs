module Linear.V4 (
    V4(..),
    fromV3
  ) where

import Linear.V3(V3(..))
import Linear.Transposable(Transposable(..))
import Foreign.Storable(Storable(..))
import Foreign.Ptr(castPtr)

data V4 a = V4 !a !a !a !a deriving Eq

instance Show a => Show (V4 a) where
  show (V4 x y z w) = "(" ++ show x ++ " " ++ show y ++ " " ++ show z ++ " " ++ show w ++ ")"

instance Functor V4 where
  f `fmap` V4 x y z w = V4 (f x) (f y) (f z) (f w)

instance Applicative V4 where
  pure x = V4 x x x x
  V4 fx fy fz fw <*> V4 x y z w = V4 (fx x) (fy y) (fz z) (fw w)

instance Foldable V4 where
  foldr op ini (V4 x y z w) = op x (op y (op z (op w ini)))

instance Traversable V4 where
  traverse f (V4 x y z w) = V4 <$> f x <*> f y <*> f z <*> f w

instance Transposable V4 where
  transpose m = V4 (fmap (\(V4 x _ _ _) -> x) m) (fmap (\(V4 _ y _ _) -> y) m) (fmap (\(V4 _ _ z _) -> z) m) (fmap (\(V4 _ _ _ w) -> w) m)

instance Storable a => Storable (V4 a) where
  sizeOf (V4 x _ _ _) = 4 * sizeOf x
  alignment (V4 x _ _ _) = alignment x
  peek ptr = V4 <$> peek ptr' <*> peekElemOff ptr' 1
                <*> peekElemOff ptr' 2 <*> peekElemOff ptr' 3
    where ptr' = castPtr ptr
  poke ptr (V4 x y z w) = do
      poke ptr' x
      pokeElemOff ptr' 1 y
      pokeElemOff ptr' 2 z
      pokeElemOff ptr' 3 w
    where ptr' = castPtr ptr

fromV3 :: V3 a -> a -> V4 a
fromV3 (V3 x y z) w = V4 x y z w
