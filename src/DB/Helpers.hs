module DB.Helpers where

import Database.HDBC qualified as HDBC
import Database.HDBC.PostgreSQL qualified as HDBC

execute :: [HDBC.SqlValue] -> HDBC.Statement -> IO HDBC.Statement
execute xs stt = do
  _ <- HDBC.execute stt xs
  return stt

describeConnection :: HDBC.Connection -> IO String
describeConnection conn = do
  ts <- HDBC.getTables conn
  descriptions <- mapM describe ts

  return $
    unlines
      [ "driverName: " ++ driverName,
        "clientVer: " ++ clientVer,
        "proxiedClientName: " ++ proxiedClientName,
        "proxiedClientVer: " ++ proxiedClientVer,
        "dbServerVer: " ++ dbServerVer,
        "dbTransactionSupport: " ++ show dbTransactionSupport,
        "tables: " ++ show ts,
        "descriptions: " ++ show descriptions
      ]
  where
    driverName = HDBC.hdbcDriverName conn
    clientVer = HDBC.hdbcClientVer conn
    proxiedClientName = HDBC.proxiedClientName conn
    proxiedClientVer = HDBC.proxiedClientVer conn
    dbServerVer = HDBC.dbServerVer conn
    dbTransactionSupport = HDBC.dbTransactionSupport conn
    describe = HDBC.describeTable conn
