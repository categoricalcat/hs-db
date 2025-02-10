module DB.QueryResult where

import Data.Map (Map)
import Database.HDBC
import Database.HDBC.Statement
import Task (Task)

type SqlResultMap = Map String SqlValue

type ResultSet = [SqlResultMap]

type QueryTaskResult = Task SqlError ResultSet

data NoConnection = IConnection NoConnection deriving (Show)

data PreparedQuery c a = (IConnection c) => PreparedQuery a [SqlValue] (Maybe c)

setConnection :: (IConnection c) => PreparedQuery c a -> c -> PreparedQuery c a
setConnection (PreparedQuery sql values _) conn = PreparedQuery sql values (Just conn)

instance (IConnection c, Show a) => Show (PreparedQuery c a) where
  show :: (IConnection c, Show a) => PreparedQuery c a -> String
  show (PreparedQuery sql values _) =
    "PreparedQuery Connection \n\t" <> show sql <> "\t-- " <> show values

instance Functor (PreparedQuery c) where
  fmap :: (a -> b) -> PreparedQuery c a -> PreparedQuery c b
  fmap f (PreparedQuery sql values conn) = PreparedQuery (f sql) values conn

instance (IConnection c) => Applicative (PreparedQuery c) where
  pure :: a -> PreparedQuery c a
  pure a = PreparedQuery a [] Nothing

  (<*>) :: PreparedQuery c (a -> b) -> PreparedQuery c a -> PreparedQuery c b
  (PreparedQuery f values _) <*> (PreparedQuery x values' c) = PreparedQuery (f x) (values <> values') c

instance (IConnection c) => Monad (PreparedQuery c) where
  return = pure

  (>>=) :: (IConnection c) => PreparedQuery c a -> (a -> PreparedQuery c b) -> PreparedQuery c b
  (PreparedQuery x _ _) >>= f = f x
