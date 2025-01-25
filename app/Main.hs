{-# LANGUAGE OverloadedStrings #-}

module Main where

import DB.Helpers (describeConnection)
import DB.Main (QueryResult, dropTable, getConn, loadConfig, query, withQueryHandler)
import Data.Either (fromLeft)
import Database.HDBC (IConnection (..), SqlValue (SqlInteger), toSql)
import MyLib (safeReadFile, withTaskLog)
import Parser.Main
  ( Parsed,
    Parser (runParser),
    ParserLog (ParserLog),
    envKeyValues,
  )
import System.Environment (setEnv)

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
      >>= runQuery conn
      >>= print

  withTaskLog "invalid file test" $
    safeReadFile "invalid file test"
      >>= print

  withTaskLog "query 1 + 1 test" $
    query conn "SELECT ($1::integer) + ($2::integer)" [SqlInteger 2, SqlInteger 2]
      >>= print

  print $ toSql ("Haskell" :: String)

  withTaskLog "dropping table user" $
    dropTable conn "users"
      >>= \case
        Left e -> print e
        Right r -> print r

  return ()

setEnvs :: IO String
setEnvs = do
  contents <- safeReadFile ".env"
  let parsed = parseEnvFile contents
  msg <- setParsedEnvs parsed
  return msg

parseEnvFile :: Either a String -> Parsed [(String, String)]
parseEnvFile = \case
  Right s -> runParser envKeyValues s []
  Left _ -> (Nothing, [], [ParserLog "no_env_file"])

setParsedEnvs :: Parsed [(String, String)] -> IO String
setParsedEnvs = \case
  (Just envs, _, _) -> do
    mapM_ (\(k, v) -> setEnv k v) envs
    return "Environment variables set successfully."
  _ -> return "No environment variables to set."

runQuery :: (IConnection conn) => conn -> Either e String -> IO QueryResult
runQuery conn (Right sql) = withQueryHandler $ query conn sql []
runQuery _ _ = return $ Right []
