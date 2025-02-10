module Lib.Nested where

import Control.Monad (join)

newtype Nested f g a = Nested
  { getNested :: f (g a)
  }

instance (Functor f, Functor g) => Functor (Nested f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Nested f g a -> Nested f g b
  fmap f (Nested fg) = Nested $ fmap (fmap f) fg

instance (Applicative f, Applicative g) => Applicative (Nested f g) where
  -- Inject a value into the Nestedd Applicative
  pure x = Nested (pure (pure x)) -- Uses `pure` for both `f` and `g`

  -- Apply a wrapped function to a wrapped value
  Nested fgab <*> Nested fga = Nested $ liftA2 (<*>) fgab fga

instance (Monad f, Monad g, Traversable f, Traversable g) => Monad (Nested f g) where
  return = pure -- Inherited from Applicative

  -- The bind operation: Nested f g a >>= (a -> Nested f g b) -> Nested f g b
  Nested fga >>= k = Nested $ do
    ga <- fga -- Extract `g a` from `f (g a)` (using `f`'s monad)
    ggb <- sequenceA (fmap (getNested . k) ga) -- Swap layers: `g (f (g b))` → `f (g (g b))`
    pure (join ggb)
