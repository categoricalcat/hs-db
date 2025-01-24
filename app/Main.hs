{-# LANGUAGE OverloadedStrings #-}

module Main where

import DB.Helpers (describeConnection)
import DB.Main (getConn, query, withQueryHandler)
import Data.Map qualified as Map
import Database.HDBC (IConnection (..), SqlValue (SqlInteger))
import MyLib (safeReadFile, withTaskLog)
import Parser.Main
  ( Parser (runParser),
    ParserLog (ParserLog),
    envKeyValues,
  )
import System.Environment (setEnv)

setEnvs :: IO ()
setEnvs =
  safeReadFile ".env"
    >>= ( \ma -> case ma of
            Right s -> return $ runParser envKeyValues s []
            Left _ -> return $ (Nothing, [], [ParserLog "no_env_file"])
        )
    >>= ( \ma -> case ma of
            (Just envs, _, _) -> do
              mapM_ (\(k, v) -> setEnv k v) envs
              putStrLn "Environment variables set successfully."
            _ -> putStrLn "No environment variables to set."
        )

run :: (IConnection conn) => conn -> IO ()
run conn = do
  withTaskLog "throw sql test" $
    safeReadFile "sql/throw.sql"
      >>= ( \ma -> case ma of
              Right sql -> withQueryHandler $ query conn sql []
              Left _ -> return $ Right [Map.empty]
          )
      >>= print

  withTaskLog "invalid file test" $
    safeReadFile "invalid file test"
      >>= print

  withTaskLog "query 1 + 1 test" $
    query conn "SELECT ($1::integer) + ($2::integer)" [SqlInteger 2, SqlInteger 2]
      >>= print

  withTaskLog "setting up loading envs from .env" $
    setEnvs

  return ()

main :: IO ()
main = withTaskLog "maine" $ do
  conn <- getConn

  describeConnection conn
    >>= putStrLn

  Main.run conn

  disconnect conn
