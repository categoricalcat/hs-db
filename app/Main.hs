{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Exception (try)
import DB.Helpers (describeConnection)
import DB.Main (dropTable, getConn, loadConfig, query)
import Database.HDBC (IConnection (..), SqlError (SqlError), SqlValue (SqlInteger), toSql)
import Log
import MyLib (safeReadFile, withTaskLog)
import Parser.Main
  ( Parsed,
    ParsedData (Parsed),
    Parser (runParser),
    alphaNum,
    envKeyValues,
    string,
  )
import System.Environment (setEnv)
import Task

pa :: Task e Integer
pa = Task [] (Just 1)

(<$$>) :: (Functor m, Functor n) => (a -> b) -> m (n a) -> m (n b)
(<$$>) = fmap . fmap

main :: IO ()
main = withTaskLog "main" $ do
  print $ runParser (alphaNum) "hello" (Trace [])
  print $ runParser (alphaNum) "hello" (Trace [])

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
        Task _ (Just sql) -> query conn sql []
        _ -> return $ Task [] Nothing
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

parseEnvFile :: Task e String -> Parsed [(String, String)]
parseEnvFile = \case
  Task _ (Just s) -> runParser envKeyValues s (Trace [])
  _ -> Task [logError "no_env_file"] Nothing

setParsedEnvs :: Parsed [(String, String)] -> IO String
setParsedEnvs = \case
  Task _ (Just (Parsed envs _)) -> do
    mapM_ (\(k, v) -> setEnv k v) envs
    return "Environment variables set successfully."
  _ -> return "No environment variables to set."
