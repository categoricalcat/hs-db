{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (Alternative (..))
import DB.Helpers (describeConnection)
import DB.Main (getConn, query, withQueryHandler)
import Data.Char (isUpperCase)
import Data.Map qualified as Map
import Database.HDBC (IConnection (..), SqlValue (SqlInteger))
import MyLib (safeReadFile, withTaskLog)
import Parser.Main

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
main = withTaskLog "main" $ do
  conn <- getConn

  describeConnection conn
    >>= putStrLn

  -- Main.run conn

  print $ runParser envName "ABC" []

  disconnect conn
