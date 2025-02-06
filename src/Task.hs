module Task where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Exception (Exception)
import Data.Bifunctor
import Data.Data (Typeable)
import Data.List (intercalate)
import Log (Log, showLog)

-- todo: transform [Log e] to Trace (Log e)
data Task e a = Task [Log e] (Maybe a)

mapLogs :: (Log e -> Log e') -> Task e a -> Task e' a
mapLogs f (Task logs ma) = Task (f <$> logs) ma

mapShowLogs :: (Show e) => Task e a -> Task String a
mapShowLogs = mapLogs showLog

instance (Show a, Show e) => Show (Task e a) where
  show :: Task e a -> String
  show (Task logs ma) =
    "Task " <> showMA ma <> "\n  " <> formatLogs logs
    where
      showMA (Just a) = show a
      showMA Nothing = "Nothing"
      formatLogs [] = "[No Logs]"
      formatLogs es = intercalate "\n  " (map show es)

instance Functor (Task e) where
  fmap :: (a -> b) -> Task e a -> Task e b
  fmap f (Task logs ma) = Task logs (f <$> ma)

instance Applicative (Task e) where
  pure :: a -> Task e a
  pure a = Task [] (Just a)

  (<*>) :: Task e (a -> b) -> Task e a -> Task e b
  (Task fs mf) <*> (Task es ma) = Task (fs ++ es) (mf <*> ma)

instance Monad (Task e) where
  (>>=) :: Task e a -> (a -> Task e b) -> Task e b
  (Task logs ma) >>= f =
    case ma of
      Just a -> f a
      Nothing -> Task logs Nothing

instance Alternative (Task e) where
  empty :: Task e a
  empty = Task [] Nothing

  (<|>) :: Task e a -> Task e a -> Task e a
  (Task logs ma) <|> (Task logs' ma') = Task (logs ++ logs') (ma <|> ma')

instance Bifunctor Task where
  bimap :: (e -> e') -> (a -> b) -> Task e a -> Task e' b
  bimap f g (Task logs ma) = Task ((fmap f) <$> logs) (g <$> ma)

instance (Exception e, Show a, Typeable a) => Exception (Task e a)

instance Foldable (Task e) where
  foldMap :: (Monoid m) => (a -> m) -> Task e a -> m
  foldMap f (Task _ ma) = maybe mempty f ma

instance Traversable (Task e) where
  sequenceA :: (Applicative f) => Task e (f a) -> f (Task e a)
  sequenceA (Task logs ma) = Task logs <$> sequenceA ma
