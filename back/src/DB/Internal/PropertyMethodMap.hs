{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Internal.PropertyMethodMap where
    
import DB.Internal.PropertyMethod as M
import DB.Internal.Query as Q
import DB.Internal.Class
import Database.HDBC (SqlValue)
import Control.Monad (void)
import Data.List (transpose)
import Helper (_p)

newtype GetMethods a = GetMethods [GetMethod] deriving (Show)
newtype SaveMethods a = SaveMethods [SaveMethod] deriving (Show)
newtype DelMethods a = DelMethods [DelMethod] deriving (Show)

class GetMethodsC m where
    newGetMethods :: [GetMethod] -> m a
    unGetMethods :: m a -> [GetMethod]

    runGetMethods :: FromConnEither n => m a -> n [[(PropertyName, SqlValue)]]
    runGetMethods ms = do
        ls <- sequence [ runGetMethod method | method <- unGetMethods ms ]
        return $ map concat $ transpose ls
    
    selectWhere :: WhereClause -> TableName -> [PropertyName] -> [SqlValue] -> m a
    selectWhere w tb ls sqlvs = newGetMethods [M.selectWhere w tb ls sqlvs]

    selectID :: ID -> TableName -> [PropertyName] -> m a
    selectID id tb ls = newGetMethods [M.selectID id tb ls]

class SaveMethodsC m where
    newSaveMethods :: [SaveMethod] -> m a
    unSaveMethods :: m a -> [SaveMethod]

    runSaveMethods :: FromConnEither n => m a -> [(PropertyName, SqlValue)] -> n [(PropertyName, ID)]
    runSaveMethods ms vls = fmap concat $ mapM (\x -> runSaveMethod x vls) $ unSaveMethods ms

    insertInto :: TableName -> [PropertyName] -> m a
    insertInto tb ls = newSaveMethods [M.insertInto tb ls]
    
    replaceInto :: TableName -> [PropertyName] -> m a
    replaceInto tb ls = newSaveMethods [M.replaceInto tb ls]
    
instance SaveMethodsC SaveMethods where
    newSaveMethods = SaveMethods
    unSaveMethods (SaveMethods v) = v
    
        
instance GetMethodsC GetMethods where
    newGetMethods = GetMethods 
    unGetMethods (GetMethods v) = v

class DelMethodsC m where
    newDelMethods :: [DelMethod] -> m a
    unDelMethods :: m a -> [DelMethod]
    
    runDelMethods :: FromConnEither n => m a -> n ()
    runDelMethods mls = void $ mapM runDelMethod $ unDelMethods mls
    
    
instance DelMethodsC DelMethods where
    newDelMethods = DelMethods
    unDelMethods (DelMethods v) = v
    