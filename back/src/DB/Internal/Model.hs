{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.Internal.Model where
    
import DB.Internal.Class
import DB.Internal.Query as Query
import Data.Aeson
import Data.Aeson.Encode (encodeToTextBuilder)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM (map, toList)
import Prelude hiding (id)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Either
import Data.Maybe
import GHC.Generics
import Helper
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Data.Convertible
import DB.Internal.PropertyMethodMap
import DB.Internal.PropertyMethod
import DB.Internal.Typecast

fromResult :: Result a -> a
fromResult (Success a) = a
fromResult (Error a) = error a

class FromModel a where
    fromModel :: (Model b) => b -> a
    fromModel = fromValue . toJSON
    
    fromValue :: Value -> a

instance FromModel (M.Map PropertyName SqlValue) where
    fromValue (Object hmap) = 
        M.fromList $ 
        map (\(k, v) -> (T.unpack k, v)) $
        HM.toList $ 
        HM.map convert hmap
            
instance FromModel (M.Map PropertyName Value) where
    fromValue = fromResult . fromJSON 
    
instance ToModel (M.Map PropertyName Value) where
    toValue = toJSON
    
instance FromModel String where
    fromValue = TL.unpack . TL.toLazyText . encodeToTextBuilder

class ToModel a where
    toValue :: a -> Value
    
    toModel :: (Model b) => a -> b
    toModel = fromResult . fromJSON . toValue
    
instance ToModel [(PropertyName, SqlValue)] where
    toValue al = object $ [ (T.pack x) .= toJSON (convert y :: Value) | (x,y) <- al ]
    
class (FromJSON a, ToJSON a, Show a) => Model a where
    
    decroate :: a -> (b -> ConnEither a) -> b -> ConnEither a
    decroate o f b = do
        e <- f b
        return $ extend o e
    
    extend :: a -> a -> a
    extend a b = toModel $ (u :: M.Map PropertyName Value)
        where
            nonNull x Null = x
            nonNull Null y = y
            u = M.unionWith nonNull (fromModel a) (fromModel b) 
    
    save :: (SaveResult r) => a -> SaveMethods a -> ConnEither r
    save obj ls = do
        v <- return $ fromModel obj
        xs <- mapM (f v) $ getSaveMethods ls
        return $ fromList $ concat xs
        where
            f :: M.Map PropertyName SqlValue -> SaveMethod -> ConnEither [(PropertyName, ID)]
            f al sm = do
                let (properties, method) = getSaveMethod sm
                id <- runSaveQuery method [ fromJust $ M.lookup x al | x <- properties ]
                return [ (x, id) | x <- properties ]
    
    get :: GetMethods a -> ConnEither [a]
    get ls = do 
        allAL <- foldM f (repeat []) $ getGetMethods ls
        return $ map toModel allAL
        where
            f :: [[(PropertyName, SqlValue)]] -> GetMethod -> ConnEither [[(PropertyName, SqlValue)]]
            f acc gm = do
                let (properties, method, sqlVs) = getGetMethod gm
                vals <- runGetQuery method sqlVs
                return $ zipWith (++) acc [ zip properties x | x <- vals ]
                
    --delete :: DelMethod l => a -> ConnEither ID
            
class SaveResult a where
    fromList :: [(PropertyName, ID)] -> a
    
instance SaveResult (M.Map PropertyName ID) where
    fromList = M.fromList 

instance SaveResult ID where
    fromList = snd . head

instance Model Pagination 

class (Model a) => PaginationC a where
    getTotal :: a -> TableName -> ConnEither a
    getTotal pag tb = do
        total <- get m
        return $ extend pag $ head total
        where
            m :: GetMethods a
            m = newGetMethods [ newGetMethod ["total"] (newGetQuery $ "SELECT COUNT(*) FROM " ++ tb) [] ]
        
    getCurrent :: a -> Int
    getPerPage :: a -> Int
    
instance PaginationC Pagination where
    getCurrent = fromJust . current
    getPerPage = fromJust . perPage
    
