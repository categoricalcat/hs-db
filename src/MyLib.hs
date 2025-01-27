module MyLib where

import Control.Exception.Base (IOException, try)
import Log (logError)
import Task (Task (Task))

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
