module MyLib where

import Control.Exception.Base (catch)
import System.IO (readFile')

ping :: IO ()
ping = putStrLn "pong"

safeReadFile :: FilePath -> IO (Either IOError String)
safeReadFile path =
  catch
    (readFile' path >>= return . Right)
    (return . Left :: IOError -> IO (Either IOError String))

withTaskLog :: (Show a) => String -> IO a -> IO a
withTaskLog task a = do
  putStrLn "\n"
  putStrLn $ "--------- >>>> " ++ task ++ " <<<< ---------"

  b <- a

  putStrLn $ "--------- <<<< " ++ task ++ " >>>> ---------"
  putStrLn "\n"

  return b
