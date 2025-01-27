module Log where

data LogType = LogError | LogAny | LogWarning | LogInfo
  deriving (Show, Eq)

data Log a = Log LogType a

newtype Trace a = Trace [Log a] deriving (Show)

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
