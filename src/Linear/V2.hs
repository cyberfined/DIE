module Linear.V2 (
    V2(..)
  ) where

import Linear.Transposable(Transposable(..))
import Foreign.Storable(Storable(..))
import Foreign.Ptr(castPtr)

data V2 a = V2 !a !a deriving Eq

instance Show a => Show (V2 a) where
  show (V2 x y) = "(" ++ show x ++ " " ++ show y ++ ")"

instance Functor V2 where
  f `fmap` V2 x y = V2 (f x) (f y)

instance Applicative V2 where
  pure x = V2 x x
  V2 fx fy <*> V2 x y = V2 (fx x) (fy y)

instance Foldable V2 where
  foldr op ini (V2 x y) = op x (op y ini)

instance Traversable V2 where
  traverse f (V2 x y) = V2 <$> f x <*> f y

instance Transposable V2 where
  transpose m = V2 (fmap (\(V2 x _) -> x) m) (fmap (\(V2 _ y) -> y) m)

instance Storable a => Storable (V2 a) where
  sizeOf (V2 x _) = 2 * sizeOf x
  alignment (V2 x _) = alignment x
  peek ptr = V2 <$> peek ptr' <*> peekElemOff ptr' 1
    where ptr' = castPtr ptr
  poke ptr (V2 x y) = do
      poke ptr' x
      pokeElemOff ptr' 1 y
    where ptr' = castPtr ptr
