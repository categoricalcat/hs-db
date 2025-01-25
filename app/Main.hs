{-# LANGUAGE OverloadedStrings #-}

module Main where

import DB.Helpers (describeConnection)
import DB.Main (getConn, loadConfig, query, withQueryHandler)
import Data.Map qualified as Map
import Database.HDBC (IConnection (..), SqlValue (SqlInteger))
import MyLib (safeReadFile, withTaskLog)
import Parser.Main
  ( Parsed,
    Parser (runParser),
    ParserLog (ParserLog),
    envKeyValues,
  )
import System.Environment (setEnv)

parseEnvFile :: Either a String -> Parsed [(String, String)]
parseEnvFile = \case
  Right s -> runParser envKeyValues s []
  Left _ -> (Nothing, [], [ParserLog "no_env_file"])

applyEnvVars :: Parsed [(String, String)] -> IO String
applyEnvVars = \case
  (Just envs, _, _) -> do
    mapM_ (\(k, v) -> setEnv k v) envs
    return "Environment variables set successfully."
  _ -> return "No environment variables to set."

setEnvs :: IO String
setEnvs = do
  contents <- safeReadFile ".env"
  let parsed = parseEnvFile contents
  msg <- applyEnvVars parsed
  return msg

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

  return ()

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
