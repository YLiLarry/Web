{-# LANGUAGE FlexibleContexts #-}

module DB.Internal (
      module Database.HDBC
    , module Data.Convertible
    , IOMaybe
    , connectDB
    , getBy
    , getByID
    , getUserIDBy
    , ID
    , ColumnName
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Convertible

type TableName = String
type ColumnName = String
type IOMaybe a = IO (Maybe a)
type ID = Integer

dbLocation :: FilePath
dbLocation = "../../db"

-- | Run the statement and fetch a column, if the column doesn't exist, then returns Nothing
column :: (Convertible SqlValue a) => String -> Statement -> IOMaybe a
column colName = (fmap (maybe Nothing fromSql . lookup colName =<<)) . fetchRowAL

getBy :: (IConnection c, Convertible a SqlValue, Convertible SqlValue b) => (ColumnName, a) -> TableName -> ColumnName -> c -> IOMaybe b
getBy (query, qval) tb cl conn = do
    stmt <- prepare conn $ "SELECT " ++ show cl ++ " FROM " ++ show tb ++ " WHERE " ++ query ++ " ?"
    execute stmt [toSql qval]
    column cl stmt 
    
getByID :: (IConnection c, Convertible SqlValue b) => Integer -> TableName -> ColumnName -> c -> IOMaybe b
getByID id = getBy ("id =", id)    

getIDBy :: (IConnection c, Convertible a SqlValue, Convertible SqlValue b) => (ColumnName, a) -> TableName -> c -> IOMaybe b
getIDBy q table = getBy q table "id" 
    
connectDB :: IO Connection
connectDB = connectSqlite3 dbLocation

getUserIDBy :: (Convertible a SqlValue, IConnection c) => (ColumnName, a) -> c -> IOMaybe ID
getUserIDBy q = getIDBy q "User"

