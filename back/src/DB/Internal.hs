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
    , column -- to be replaced by a general new function
    , Pagination(..)
    , getAll
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Convertible
import Data.List (intercalate)

type TableName = String
type ColumnName = String
type IOMaybe a = IO (Maybe a)
type ID = Integer


data Pagination = Pagination {
      current :: Int
    --, total   :: Int
    , perPage :: Int
}

dbLocation :: FilePath
dbLocation = "../db.sqlite"

-- | Run the statement and fetch a column, if the column doesn't exist, then returns Nothing
column :: (Convertible SqlValue a) => String -> Statement -> IOMaybe a
column colName = (fmap (maybe Nothing fromSql . lookup colName =<<)) . fetchRowAL

getBy :: (IConnection c, Convertible a SqlValue, Convertible SqlValue b) => (ColumnName, a) -> TableName -> ColumnName -> c -> IOMaybe b
getBy (query, qval) tb cl conn = do
    stmt <- prepare conn $ "SELECT " ++ cl ++ " FROM " ++ tb ++ " WHERE " ++ query ++ " ?"
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

--getColsABy :: (IConnection c, Convertible a SqlValue, Convertible SqlValue b) => (ColumnName, a) -> TableName -> [ColumnName] -> c -> IOMaybe [(String, b)]

getAll :: (IConnection c) => TableName -> [ColumnName] -> Pagination -> c -> IO [[(ColumnName, String)]]
getAll tb cols pag conn = do
    let offset = show $ ((current pag) - 1) * (perPage pag)
    let top    = show $ perPage pag
    stmt <- prepare conn $ "SELECT " ++ intercalate "," cols ++ " FROM " ++ tb ++ " LIMIT " ++ offset ++ ", " ++ top
    execute stmt []
    (fmap.fmap.fmap) (\(colName, sqlVal) -> (colName, fromSql sqlVal)) $ fetchAllRowsAL stmt
