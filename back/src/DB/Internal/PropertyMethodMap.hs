{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Internal.PropertyMethodMap where
    
import DB.Internal.PropertyMethod as M
import DB.Internal.Query as Q
import DB.Internal.Class
import Database.HDBC (SqlValue)

data GetMethods a = GetMethods [GetMethod]
data SaveMethods a = SaveMethods [SaveMethod]

class GetMethodsC m where
    newGetMethods :: [GetMethod] -> m a
    getGetMethods :: m a -> [GetMethod]

class SaveMethodsC m where
    newSaveMethods :: [SaveMethod] -> m a
    getSaveMethods :: m a -> [SaveMethod]

    insertInto :: TableName -> [PropertyName] -> m a
    insertInto tb ls = newSaveMethods [M.insertInto tb ls]
    
instance SaveMethodsC SaveMethods where
    newSaveMethods = SaveMethods
    getSaveMethods (SaveMethods v) = v
    
        
instance GetMethodsC GetMethods where
    newGetMethods = GetMethods 
    getGetMethods (GetMethods v) = v
