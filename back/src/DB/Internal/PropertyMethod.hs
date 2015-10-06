
module DB.Internal.PropertyMethod where
    
import Database.HDBC
import DB.Internal.Query as Q
import DB.Internal.Class

type PropertyName = String

data SaveMethod = SaveMethod ([PropertyName], SaveQuery) deriving (Show)
class SaveMethodC m where
    newSaveMethod :: [PropertyName] -> SaveQuery -> m
    getSaveMethod :: m -> ([PropertyName], SaveQuery) 

    insertInto :: TableName -> [ColumnName] -> m
    insertInto tb cols = newSaveMethod cols $ Q.insertInto tb cols
    
    replaceInto :: TableName -> [ColumnName] -> m
    replaceInto tb cols = newSaveMethod cols $ Q.replaceInto tb cols
    
    
data GetMethod = GetMethod ([PropertyName], GetQuery, [SqlValue]) deriving (Show)
class GetMethodC m where
    newGetMethod :: [PropertyName] -> GetQuery -> [SqlValue] -> m
    getGetMethod :: m -> ([PropertyName], GetQuery, [SqlValue]) 

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
    getSaveMethod (SaveMethod v) = v

instance GetMethodC GetMethod where
    newGetMethod a b c = GetMethod (a,b,c)
    getGetMethod (GetMethod v) = v
    
