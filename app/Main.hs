{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (Exception (toException))
import DB.Helpers (describeConnection)
import DB.Main (dropTable, getConn, loadConfig, query, withQueryHandler)
import DB.QueryResult
import Data.Either (fromLeft)
import Database.HDBC (IConnection (..), SqlError (SqlError), SqlValue (SqlInteger), toSql)
import Log
import MyLib (safeReadFile, withTaskLog)
import Parser.Main
  ( Parsed,
    Parser (runParser),
    envKeyValues,
  )
import System.Environment (setEnv)

-- print $ toSql ("Haskell2" :: String)

main :: IO ()
main = withTaskLog "maine" $ do
  withTaskLog "setting up loading envs from .env" $
    setEnvs
      >>= putStrLn

  conn <-
    loadConfig
      >>= getConn

  withTaskLog "describing connection" $
    describeConnection conn
      >>= putStrLn

  Main.run conn

  disconnect conn

run :: (IConnection conn) => conn -> IO ()
run conn = do
  withTaskLog "throw sql test" $
    safeReadFile "sql/throw.sql"
      >>= \case
        Right sql -> query conn sql []
        Left _ -> return $ Left $ logError (SqlError "could not read sql/throw.sql" 1 "asd")
      >>= print

  withTaskLog "invalid file test" $
    safeReadFile "invalid file test"
      >>= print

  withTaskLog "query 1 + 1 test" $
    query conn "SELECT ($1::integer) + ($2::integer)" [SqlInteger 2, SqlInteger 2]
      >>= print

  withTaskLog "creating table user" $
    safeReadFile "sql/create-user.sql"
      >>= \case
        Right sql -> query conn sql []
        Left _ -> return $ Left $ logError (SqlError "could not create user" 1 "asd")
      >>= print

  withTaskLog "dropping table user" $
    dropTable conn "users"
      >>= print

  return ()

setEnvs :: IO String
setEnvs = do
  contents <- safeReadFile ".env"
  let parsed = parseEnvFile contents
  msg <- setParsedEnvs parsed
  return msg

parseEnvFile :: Either a String -> Parsed [(String, String)]
parseEnvFile = \case
  Right s -> runParser envKeyValues s (Trace [])
  Left _ -> (Nothing, [], Trace [logError "no_env_file"])

setParsedEnvs :: Parsed [(String, String)] -> IO String
setParsedEnvs = \case
  (Just envs, _, _) -> do
    mapM_ (\(k, v) -> setEnv k v) envs
    return "Environment variables set successfully."
  _ -> return "No environment variables to set."
