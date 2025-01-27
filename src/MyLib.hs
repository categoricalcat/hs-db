module MyLib where

import Control.Exception.Base (IOException, try)

ping :: IO ()
ping = putStrLn "pong"

safeReadFile :: FilePath -> IO (Either IOException String)
safeReadFile path = try (readFile path)

withTaskLog :: (Show a) => String -> IO a -> IO a
withTaskLog task a = do
  putStrLn "\n"
  putStrLn $ "--------- >>>> " ++ task ++ " <<<< ---------"

  b <- a

  putStrLn $ "--------- <<<< " ++ task ++ " >>>> ---------"
  putStrLn "\n"

  return b
