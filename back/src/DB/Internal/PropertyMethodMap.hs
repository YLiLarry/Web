{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Internal.PropertyMethodMap where
    
import DB.Internal.PropertyMethod as M
import DB.Internal.Query as Q
import DB.Internal.Class
import Database.HDBC (SqlValue)

data GetMethods a = GetMethods [GetMethod] deriving (Show)
data SaveMethods a = SaveMethods [SaveMethod] deriving (Show)

class GetMethodsC m where
    newGetMethods :: [GetMethod] -> m a
    getGetMethods :: m a -> [GetMethod]

    selectWhere :: WhereClause -> TableName -> [PropertyName] -> [SqlValue] -> m a
    selectWhere w tb ls sqlvs = newGetMethods [M.selectWhere w tb ls sqlvs]

    selectID :: ID -> TableName -> [PropertyName] -> m a
    selectID id tb ls = newGetMethods [M.selectID id tb ls]

class SaveMethodsC m where
    newSaveMethods :: [SaveMethod] -> m a
    getSaveMethods :: m a -> [SaveMethod]

    insertInto :: TableName -> [PropertyName] -> m a
    insertInto tb ls = newSaveMethods [M.insertInto tb ls]
    
    replaceInto :: TableName -> [PropertyName] -> m a
    replaceInto tb ls = newSaveMethods [M.replaceInto tb ls]
    
instance SaveMethodsC SaveMethods where
    newSaveMethods = SaveMethods
    getSaveMethods (SaveMethods v) = v
    
        
instance GetMethodsC GetMethods where
    newGetMethods = GetMethods 
    getGetMethods (GetMethods v) = v
