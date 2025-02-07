module DB.Main where

import Control.Exception (catch)
import DB.Helpers (execute)
import DB.QueryResult
import Data.Map (Map)
import Database.HDBC (IConnection (..), SqlError, SqlValue (SqlInteger, SqlString), fetchAllRowsMap', handleSql, toSql)
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL')
import Log (logError)
import System.Environment (getEnv)
import Task (Task (Task))

data ConnectionConfig = ConnectionConfig
  { dbUser :: String,
    dbPassword :: String,
    dbHost :: String,
    dbPort :: String,
    dbName :: String
  }

-- DROP TABLE $1 -- P
-- dropTable :: (IConnection c) => c -> String -> IO QueryResult
dropTable :: (IConnection conn) => conn -> String -> IO (Task SqlError [SqlResultMap])
dropTable c table = query (PreparedQuery ("DROP TABLE " <> table <> " CASCADE") [] c)

loadConfig :: IO ConnectionConfig
loadConfig = do
  user <- getEnv "DB_USER"
  password <- getEnv "DB_PASSWORD"
  host <- getEnv "DB_HOST"
  port <- getEnv "DB_PORT"
  name <- getEnv "DB_NAME"
  return $ ConnectionConfig user password host port name

devConfig :: ConnectionConfig
devConfig = ConnectionConfig "user1" "user1" "0.0.0.0" "5433" "user1"

getDevConn :: IO Connection
getDevConn = getConn devConfig

getConn :: ConnectionConfig -> IO Connection
getConn cfg = connectPostgreSQL' $ show cfg

data PreparedQuery c a = (IConnection c, Show a) => PreparedQuery a [SqlValue] c

instance (Show c) => Show (PreparedQuery c a) where
  show :: PreparedQuery c a -> String
  show (PreparedQuery sql values _) =
    "PreparedQuery Connection \n\t" <> (show sql) <> "\t-- " <> show values

query :: (IConnection c) => PreparedQuery c String -> IO QueryTaskResult
query (PreparedQuery sql values conn) =
  catch
    ( prepare conn sql
        >>= execute values
        >>= fetchAllRowsMap'
        >>= return . Task [] . Just
    )
    (\e -> return $ Task [logError e] Nothing)

emptyOnError :: SqlError -> IO QueryTaskResult
emptyOnError =
  ( \e -> do
      print e
      return $ Task [logError e] Nothing
  )

-- |
-- returns logs error and returns empty result
withQueryHandler :: IO QueryTaskResult -> IO QueryTaskResult
withQueryHandler = handleSql emptyOnError

instance Show ConnectionConfig where
  show :: ConnectionConfig -> String
  show (ConnectionConfig user password host port name) =
    "user="
      ++ user
      ++ " password="
      ++ password
      ++ " host="
      ++ host
      ++ " dbname="
      ++ name
      ++ " port="
      ++ port
