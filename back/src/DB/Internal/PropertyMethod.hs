
module DB.Internal.PropertyMethod where
    
import Database.HDBC
import DB.Internal.Query as Q
import DB.Internal.Class
import Data.Maybe (fromJust)
import Helper (_p)

type PropertyName = String

data SaveMethod = SaveMethod ([PropertyName], SaveQuery) deriving (Show)
class SaveMethodC m where
    newSaveMethod :: [PropertyName] -> SaveQuery -> m
    unSaveMethod :: m -> ([PropertyName], SaveQuery) 
    
    runSaveMethod :: FromConnEither n => m -> [(PropertyName, SqlValue)] -> n ID
    runSaveMethod ms sqlVs = runSaveQuery saveQ [ fromJust $ lookup prop sqlVs | prop <- props ]
        where
            (props, saveQ) = unSaveMethod ms

    insertInto :: TableName -> [ColumnName] -> m
    insertInto tb cols = newSaveMethod cols $ Q.insertInto tb cols
    
    replaceInto :: TableName -> [ColumnName] -> m
    replaceInto tb cols = newSaveMethod cols $ Q.replaceInto tb cols
    
data GetMethod = GetMethod ([PropertyName], GetQuery, [SqlValue]) deriving (Show)
class GetMethodC m where
    newGetMethod :: [PropertyName] -> GetQuery -> [SqlValue] -> m
    unGetMethod :: m -> ([PropertyName], GetQuery, [SqlValue]) 

    runGetMethod :: FromConnEither n => m -> n [[(PropertyName, SqlValue)]]
    runGetMethod ms = do
        result <- runGetQuery query sqlVs
        return [ zip props r | r <- result ]
        where
            (props, query, sqlVs) = unGetMethod ms

    selectID :: ID -> TableName -> [ColumnName] -> m
    selectID id tb cols = newGetMethod cols (Q.selectID tb cols) [toSql id]
    
    prevID :: ID -> TableName -> [ColumnName] -> m
    prevID id tb cols = newGetMethod cols (Q.selectPrev tb ["idx"]) [toSql id]
    
    nextID :: ID -> TableName -> [ColumnName] -> m
    nextID id tb cols = newGetMethod cols (Q.selectNext tb ["idx"]) [toSql id]
        
    selectP :: Pagination -> TableName -> [ColumnName] -> m
    selectP pag tb cols = newGetMethod cols (Q.selectP pag tb cols) []
    
    selectWhere :: WhereClause -> TableName -> [ColumnName] -> [SqlValue] -> m
    selectWhere w tb cols = newGetMethod cols (Q.selectWhere w tb cols) 
        
instance SaveMethodC SaveMethod where
    newSaveMethod a b = SaveMethod (a,b)
    unSaveMethod (SaveMethod v) = v

instance GetMethodC GetMethod where
    newGetMethod a b c = GetMethod (a,b,c)
    unGetMethod (GetMethod v) = v
    
data DelMethod = DelMethod (DelQuery, [SqlValue]) deriving (Show)
class DelMethodC m where
    newDelMethod :: DelQuery -> [SqlValue] -> m
    unDelMethod :: m -> (DelQuery, [SqlValue]) 
    
    runDelMethod :: FromConnEither n => m -> n ()
    runDelMethod m = runDelQuery delQuery sqlVs
        where
            (delQuery, sqlVs) = unDelMethod m
        
    
instance DelMethodC DelMethod where
    newDelMethod query vals = DelMethod (query, vals)
    unDelMethod (DelMethod x) = x
    