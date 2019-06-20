module Linear.Transposable (
    Transposable(..)
  ) where

class Functor t => Transposable t where
  transpose :: Functor f => f (t a) -> t (f a)
