{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module DB.Internal.Aeson where

import DB.Internal.Class
import DB.Internal.Functions
import Data.Convertible (Convertible(..))
import Data.Maybe
import Data.Aeson
import Data.Either
import Database.HDBC

type Query = String

byID :: Query
byID = "id = "

instance FromJSON Pagination
instance ToJSON Pagination
instance DB Pagination

class (FromJSON a, ToJSON a, Show a) => DB a where
    
    -- | Columns (Property Names of `a`) affected when inserting to the database,
    -- other properties will be ignored.
    insertCols :: a -> [ColumnName]
    insertCols = replaceCols
    
    -- | Columns (Property Names of `a`) affected when replacing to the database
    -- other properties will be ignored.
    replaceCols :: a -> [ColumnName]
    replaceCols = insertCols
    
    toInsert :: a -> [(ColumnName, String)]
    toInsert a = filter f $ objToAL a
        where
            f (colnm, _) = elem colnm $ insertCols a
    
    toReplace :: a -> [(ColumnName, String)]
    toReplace a = filter f $ objToAL a
        where
            f (colnm, _) = elem colnm $ replaceCols a
            
    -- | Columns (Property Names of `a`) when getting one from the database,
    -- other properties will be the default value.
    oneCols :: a -> [ColumnName]
    oneCols = allCols
    
    -- | Columns (Property Names of `a`) when getting many from the database
    -- other properties will be the default value.
    allCols :: a -> [ColumnName]
    allCols = oneCols
    
    fromOne :: [(ColumnName, String)] -> a
    fromOne = alToObj
    
    fromAll :: [(ColumnName, String)] -> a
    fromAll = alToObj
    
    table :: a -> String
    table = head . words . show
    
    dbInsert :: a -> Conn ID
    dbInsert b = let (cols,vals) = unzip $ toInsert b in new (table b) cols $ map toSql vals

    dbReplace :: a -> Conn ID
    dbReplace b = let (cols,vals) = unzip $ toReplace b in new (table b) cols $ map toSql vals


    -- | Get data from the database extending the provided one
    dbOne :: (Convertible qval SqlValue) => a -> Query -> qval -> ConnMaybe a
    dbOne a qr qv = do
        v <- getOneBy (qr, qv) (table a) $ oneCols a
        return $ fromOne v
        
    -- | Get data from the database extending the provided one
    dbAll :: a -> Conn [a]
    dbAll a = do
        ls <- getAll (table a) (allCols a)
        return $ map alToObj ls
        
    dbAllPag :: a -> Pagination -> Conn ([a], Pagination)
    dbAllPag a p = do
        (ls, pag) <- getAllPaginated (table a) (allCols a) p
        return (map alToObj ls, pag)
    

alToObjEither :: FromJSON a => [(ColumnName, String)] -> Either String a
alToObjEither ls = 
    case fromJSON $ toJSON ls of
        Error msg -> Left msg
        Success v -> Right v

alToObjMaybe :: FromJSON a => [(ColumnName, String)] -> Maybe a
alToObjMaybe ls = 
    case fromJSON $ toJSON ls of
        Error   _ -> Nothing
        Success v -> Just v
    

alToObj :: FromJSON a => [(ColumnName, String)] -> a
alToObj ls = 
    case fromJSON $ toJSON ls of
        Error msg -> error msg
        Success v -> v

objToAL :: ToJSON a => a -> [(ColumnName, String)]
objToAL obj = 
    case fromJSON $ toJSON obj of
        Error msg -> error msg
        Success v -> v


getObj :: FromJSON a => ID -> TableName -> [ColumnName] -> ConnMaybe a
getObj id tb cols = fmap alToObj (getOne id tb cols)

--newObj :: ToJSON a => TableName -> a -> Conn ID
--newObj 
