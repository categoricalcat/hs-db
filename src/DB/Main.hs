module DB.Main where

import Control.Exception (catch)
import DB.Helpers (execute)
import DB.QueryResult
import Data.Map (Map)
import Database.HDBC (IConnection (..), SqlError (SqlError), SqlValue (SqlInteger, SqlString), Statement (finish), fetchAllRowsMap', handleSql, toSql)
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
dropTable c table = query ("DROP TABLE " <> table <> " CASCADE") [] c

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

prepareQuery :: (IConnection c) => c -> String -> [SqlValue] -> PreparedQuery c String
prepareQuery conn sql values = PreparedQuery sql values (Just conn)

query :: (IConnection c) => String -> [SqlValue] -> c -> IO (Task SqlError [Map String SqlValue])
query sql values c =
  catch
    ( prepare c sql
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
