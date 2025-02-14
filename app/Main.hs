{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad (join)
import DB.Helpers (describeConnection)
import DB.Main (dropTable, getConn, loadConfig, prepareQuery, query)
import DB.QueryResult
import Data.Functor.Compose
import Database.HDBC
import Lib.Nested
import Log
import MyLib (safeReadFile, withTaskLog, (<<$>>), (=<<<), (=>>), (>>>=))
import Parser.Main
  ( Parsed,
    ParsedData (Parsed),
    Parser (runParser),
    alphaNum,
    digit,
    envKeyValues,
    string,
  )
import System.Environment (setEnv)
import Task

pa :: Task e Integer
pa = Task [] (Just 1)

taskQuery :: (Show e) => Task e String -> Task String ResultSet
taskQuery (Task logs Nothing) = Task (show <<$>> logs) Nothing
taskQuery (Task logs (Just sql)) = Task [] Nothing

test c =
  withTaskLog "testing" $ do
    let a = showTaskLogs <$> safeReadFile "sql/create-user.sql"
    let b = (\a' -> Task [] (Just [a'])) =>> a

    return ()
  where
    q s = showTaskLogs <$> query s [] c

main :: IO ()
main = withTaskLog "main" $
  do
    withTaskLog "setting up loading envs from .env" $
      setEnvs >>= putStrLn

    conn <-
      loadConfig
        >>= getConn

    withTaskLog "describing connection" $
      describeConnection conn
        >>= putStrLn

    Main.run conn

    print $ runParser (alphaNum) "hello" (Trace [])
    print $ runParser (some alphaNum) "hello" (Trace [])
    print $ runParser (some digit) "12345" (Trace [])

    withTaskLog "creating table user" $
      showTaskLogs <$> safeReadFile "sql/create-userr.sql"
        >>= \case
          Task logs (Just sql) ->
            showTaskLogs <$> query sql [] conn
              >>= \case
                Task _ (Just r) -> return $ Task (logInfo "table_created" : logs) (Just r)
                Task logs' _ -> return $ Task (logError "could_not_create_table" : logs') Nothing
          Task logs _ -> return $ Task (logError "file_not_found" : logs) Nothing
        >>= print

    -- _ <-
    --   withTaskLog "testing" $
    --     (\s -> query s [] conn)
    --       <<$>> mapShowLogs
    --       `fmap` safeReadFile "sql/create-user.sql"
    --       -- >>>= (\a -> return _)
    --       >>= print

    disconnect conn
    return ()

run :: (IConnection conn) => conn -> IO ()
run conn = do
  withTaskLog "throw sql test" $
    safeReadFile "sql/throw.sql"
      >>= print

  withTaskLog "invalid file test" $
    safeReadFile "invalid file test"
      >>= print

  withTaskLog "query 1 + 1 test" $
    query "SELECT ($1::integer) + ($2::integer)" [SqlInteger 2, SqlInteger 2] conn
      >>= print

  -- sql <- getNested $ (\s -> Nested $ query conn s []) =<< (Nested $ safeReadFile "sql/create-user.sql")

  r <-
    withTaskLog "creating table user" $
      ( do
          return "asd"
      )

  print r
  print result

  withTaskLog "dropping table user" $
    dropTable conn "users"
      >>= print

  return ()

setEnvs :: IO String
setEnvs = do
  contents <- safeReadFile ".env"
  asd <- id <<$>> safeReadFile ".env"
  print asd
  let parsed = parseEnvFile contents
  print parsed
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

-- composedValue :: Nested [] Maybe Int
composedValue :: Nested [] Maybe Int
composedValue =
  Nested [Just 1, Nothing, Just 3]
    >>= (\a -> Nested $ [Just a])

composedValue' :: Nested [] Maybe Int
composedValue' = id . (* 2) <$> Nested [Just 1, Nothing, Just 3]

result :: [Maybe Int]
result = getNested (fmap (* 2) composedValue) -- Result: [Just 2, Nothing, Just 6]
