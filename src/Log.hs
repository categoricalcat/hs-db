module Log where

data LogType = LogError | LogAny | LogWarning | LogInfo | NoLog
  deriving (Show, Eq)

data Log a = Log LogType a

newtype Trace a = Trace [Log a] deriving (Show, Semigroup, Monoid)

type LogString = Log String

instance (Show a) => Show (Log a) where
  show :: (Show a) => Log a -> String
  show (Log t a) = "[" <> show t <> "] " <> show a

type TraceString = Trace String

logError :: a -> Log a
logError = Log LogError

logWarning :: a -> Log a
logWarning = Log LogWarning

logInfo :: a -> Log a
logInfo = Log LogInfo

showLog :: (Show a) => Log a -> Log String
showLog (Log t a) = Log t (show a)

instance Functor Log where
  fmap :: (a -> b) -> Log a -> Log b
  fmap f (Log t a) = Log t (f a)

instance (Semigroup a) => Semigroup (Log a) where
  (<>) :: Log a -> Log a -> Log a
  (Log _ a) <> (Log t a') = Log t (a <> a')

instance (Monoid a) => Monoid (Log a) where
  mempty :: Log a
  mempty = Log NoLog mempty
