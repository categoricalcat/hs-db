module MyLib where

import Control.Exception.Base (IOException, try)
import Log (logError)
import Task (Task (Task))

-- INSTANCES
-- <$> = fmap :: (a -> b) -> f a -> f b
-- <*> = ap :: f (a -> b) -> f a -> f b
-- =<< = monad fmap :: (a -> m b) -> m a -> m b
-- >>= = right monad fmap :: m a -> (a -> m b) -> m b
-- <$$> = fmap . fmap :: (a -> b) -> f (g a) -> f (g b)
-- =<<< = fmap . fmap :: m (n a) -> (a -> m (n b)) -> m (n b)
(<$$>) :: (Functor m, Functor n) => (a -> b) -> m (n a) -> m (n b)
(<$$>) = fmap . fmap

ping :: IO ()
ping = putStrLn "pong"

safeReadFile :: FilePath -> IO (Task IOException String)
safeReadFile path = do
  result <- try $ readFile path

  case result of
    Left e -> return $ Task [logError e] Nothing
    Right contents -> return $ pure contents

withTaskLog :: (Show a) => String -> IO a -> IO a
withTaskLog task a = do
  putStrLn "\n"
  putStrLn $ "--------- >>>> " ++ task ++ " <<<< ---------"

  b <- a

  putStrLn $ "--------- <<<< " ++ task ++ " >>>> ---------"
  putStrLn "\n"

  return b
