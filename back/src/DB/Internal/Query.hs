{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module DB.Internal.Query where

import DB.Internal.Class
import Data.List
import Data.Maybe
import Database.HDBC    
import Control.Monad.Reader
import Control.Monad.Except

type WhereClause = String

data SaveQuery = SaveQuery {
    saveQuery :: String
} deriving (Show)
    
data GetQuery = GetQuery {
    getQuery :: String
} deriving (Show)

data DelQuery = DelQuery {
    delQuery :: String
} deriving (Show)

class SaveQueryC a where
    newSaveQuery :: String -> a
    unSaveQuery :: a -> String
    
    runSaveQuery :: FromConnEither m => a -> [SqlValue] -> m ID
    runSaveQuery a sqlvals =
        let f conn = ExceptT $ handleSql (return . Left . seErrorMsg) $ do 
                stmt <- prepare conn $ unSaveQuery a
                execute stmt sqlvals
                commit conn
                (Right . fromSql . head . head) <$> quickQuery conn "SELECT last_insert_rowid();" []
        in fromConnEither $ ReaderT f
        
    insertInto :: TableName -> [ColumnName] -> a
    insertInto tb cols = newSaveQuery $ "INSERT INTO " ++ tb ++ " (" ++ intercalate "," cols ++ ") VALUES (" ++ intercalate "," (map (const "?") cols) ++ ")"
    
    replaceInto :: TableName -> [ColumnName] -> a
    replaceInto tb cols = newSaveQuery $ "REPLACE INTO " ++ tb ++ " (" ++ intercalate "," cols ++ ") VALUES (" ++ intercalate "," (map (const "?") cols) ++ ")"
    
    
instance SaveQueryC SaveQuery where
    newSaveQuery = SaveQuery
    unSaveQuery = saveQuery
    
class GetQueryC a where
    newGetQuery :: String -> a
    unGetQuery :: a -> String
    
    runGetQuery :: FromConnEither m => a -> [SqlValue] -> m [[SqlValue]]
    runGetQuery a sqlVs =
        let f conn = ExceptT $ handleSql (return . Left . seErrorMsg) $ do 
                r <- quickQuery conn (unGetQuery a) sqlVs
                return $ Right r
        in fromConnEither $ ReaderT f
        
        
    selectID :: TableName -> [ColumnName] -> a
    selectID = selectWhere "idx = ?"
    
    selectPrev :: TableName -> [ColumnName] -> a
    selectPrev = selectWhere "idx < ? ORDER BY idx DESC LIMIT 1"
    
    selectNext :: TableName -> [ColumnName] -> a
    selectNext = selectWhere "idx > ? ORDER BY idx ASC LIMIT 1"
    
    selectWhere :: WhereClause -> TableName -> [ColumnName] -> a
    selectWhere clause = select $ " WHERE " ++ clause
    
    select :: String -> TableName -> [ColumnName] -> a
    select rst tb cols = newGetQuery $ "SELECT " ++ intercalate "," cols ++ " FROM " ++ tb ++ " " ++ rst
        
    selectP :: Pagination -> TableName -> [ColumnName] -> a
    selectP pag = select $ "LIMIT " ++ offset ++ ", " ++ top
        where
            curr   = pag # current
            per    = pag # perPage
            offset = show $ (curr - 1) * per
            top    = show per
        
    
instance GetQueryC GetQuery where
    newGetQuery = GetQuery
    unGetQuery = getQuery
    
class DelQueryC a where
    newDelQuery :: String -> a
    unDelQuery :: a -> String
    
    runDelQuery :: FromConnEither m => a -> [SqlValue] -> m ()
    runDelQuery a sqlVs =
        let f conn = ExceptT $ handleSql (return . Left . seErrorMsg) $ do 
                r <- quickQuery conn (unDelQuery a) sqlVs
                return $ Right ()
        in fromConnEither $ ReaderT f
    
    deleteWhere :: WhereClause -> TableName -> a
    deleteWhere clause tb = newDelQuery $ "DELETE FROM " ++ tb ++ " WHERE " ++ clause
    
    deleteID :: TableName -> a
    deleteID = deleteWhere "id = ?"
    
instance DelQueryC DelQuery where
    newDelQuery = DelQuery
    unDelQuery (DelQuery v) = v
