module DB.QueryResult where

import Data.Map (Map)
import Database.HDBC (SqlError, SqlValue)
import Task (Task)

type SqlResultMap = Map String SqlValue

type ResultSet = [SqlResultMap]

type QueryResult = Task SqlError ResultSet
