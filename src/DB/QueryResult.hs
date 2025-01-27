module DB.QueryResult where

import Data.Map (Map)
import Database.HDBC (SqlError, SqlValue)
import Log

type SqlResultMap = Map String SqlValue

type QueryResult = Either (Log SqlError) [SqlResultMap]
