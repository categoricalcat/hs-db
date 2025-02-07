{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Exception (try)
import Control.Monad (join)
import DB.Helpers (describeConnection)
import DB.Main (PreparedQuery (PreparedQuery), dropTable, getConn, loadConfig, query)
import DB.QueryResult (ResultSet)
import Data.Functor.Compose
import Database.HDBC (IConnection (..), SqlError (SqlError), SqlValue (SqlInteger), toSql)
import Log
import MyLib (safeReadFile, withTaskLog, (<<$>>))
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

main :: IO ()
main = withTaskLog "main" $ do
  withTaskLog "setting up loading envs from .env" $
    setEnvs
      >>= putStrLn

  conn <-
    loadConfig
      >>= getConn

  withTaskLog "describing connection" $
    describeConnection conn
      >>= putStrLn

  -- Main.run conn

  print $ runParser (alphaNum) "hello" (Trace [])
  print $ runParser (some alphaNum) "hello" (Trace [])
  print $ runParser (some digit) "12345" (Trace [])

  withTaskLog "creating table user" $
    mapShowLogs <$> safeReadFile "sql/create-userr.sql"
      >>= \case
        Task logs (Just sql) ->
          mapShowLogs <$> query (PreparedQuery sql [] conn)
            >>= \case
              Task _ (Just r) -> return $ Task (logInfo "table_created" : logs) (Just r)
              Task logs' _ -> return $ Task (logError "could_not_create_table" : logs') Nothing
        Task logs _ -> return $ Task (logError "file_not_found" : logs) Nothing
      >>= print

  _ <-
    withTaskLog "testing" $
      (\a -> a)
        `fmap` (\s -> PreparedQuery s [] conn)
        <<$>> mapShowLogs
        `fmap` safeReadFile "sql/create-user.sql"
        >>= (\a -> return a)
  -- >>= print

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
    query (PreparedQuery "SELECT ($1::integer) + ($2::integer)" [SqlInteger 2, SqlInteger 2] conn)
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

newtype Nested f g a = Nested
  { getNested :: f (g a)
  }

instance (Functor f, Functor g) => Functor (Nested f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Nested f g a -> Nested f g b
  fmap f (Nested fg) = Nested $ fmap (fmap f) fg

instance (Applicative f, Applicative g) => Applicative (Nested f g) where
  -- Inject a value into the Nestedd Applicative
  pure x = Nested (pure (pure x)) -- Uses `pure` for both `f` and `g`

  -- Apply a wrapped function to a wrapped value
  Nested fgab <*> Nested fga = Nested $ liftA2 (<*>) fgab fga

instance (Monad f, Monad g, Traversable f, Traversable g) => Monad (Nested f g) where
  return = pure -- Inherited from Applicative

  -- The bind operation: Nested f g a >>= (a -> Nested f g b) -> Nested f g b
  Nested fga >>= k = Nested $ do
    ga <- fga -- Extract `g a` from `f (g a)` (using `f`'s monad)
    ggb <- sequenceA (fmap (getNested . k) ga) -- Swap layers: `g (f (g b))` → `f (g (g b))`
    pure (join ggb)
