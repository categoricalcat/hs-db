module DB.Helpers where

import Database.HDBC qualified as HDBC
import Database.HDBC.PostgreSQL qualified as HDBC

execute :: [HDBC.SqlValue] -> HDBC.Statement -> IO HDBC.Statement
execute xs stt = do
  _ <- HDBC.execute stt xs
  return stt

unlines' :: [String] -> String
unlines' [] = ""
unlines' [x] = x
unlines' (x : xs) = x ++ "\n" ++ unlines' xs

describeConnection :: HDBC.Connection -> IO String
describeConnection conn = do
  ts <- HDBC.getTables conn
  descriptions <- mapM (HDBC.describeTable conn) ts
  -- to do: prettify descriptions
  return $ (show conn) <> "\n" <> unlines' (show <$> descriptions)

instance Show HDBC.Connection where
  show :: HDBC.Connection -> String
  show conn =
    unlines'
      [ "driverName: " ++ driverName,
        "clientVer: " ++ clientVer,
        "proxiedClientName: " ++ proxiedClientName,
        "proxiedClientVer: " ++ proxiedClientVer,
        "dbServerVer: " ++ dbServerVer,
        "dbTransactionSupport: " ++ show dbTransactionSupport
      ]
    where
      driverName = HDBC.hdbcDriverName conn
      clientVer = HDBC.hdbcClientVer conn
      proxiedClientName = HDBC.proxiedClientName conn
      proxiedClientVer = HDBC.proxiedClientVer conn
      dbServerVer = HDBC.dbServerVer conn
      dbTransactionSupport = HDBC.dbTransactionSupport conn
