{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module DB.Internal.Class where

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import GHC.Generics
import Data.Typeable
import Data.Aeson
-- import Database.HDBC
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Elevator

type TableName = String
type ColumnName = String
type IOMaybe a = IO (Maybe a)
type ID = Integer
type ConnT = ReaderT Connection
type Conn = ReaderT Connection IO 
type ConnEither = ConnEitherT IO

type ConnEitherT m = ConnT (ExceptT String m)

runConnEitherT :: ConnEitherT m v -> Connection -> m (Either String v)
runConnEitherT r x = runExceptT $ runReaderT r x

-- mapConnT :: (Monad m, Monad n) => (m x -> n y) -> ConnT m x -> ConnT n y
mapConnT = mapReaderT

class (Monad t) => FromConnEitherT m t where
    fromConnEitherT :: ConnEitherT m a -> t a

class (Monad t, FromConnEitherT IO t) => FromConnEither t where
    fromConnEither :: ConnEitherT IO a -> t a
    fromConnEither = fromConnEitherT

instance (FromConnEitherT IO m) => FromConnEither m where
    fromConnEither = fromConnEitherT

instance FromConnEitherT IO (ConnEitherT IO) where
    fromConnEitherT = id

class ToConnEitherT t where
    toConnEitherT :: (Monad m) => t m a -> ConnEitherT m a

-- mapConnEitherT :: (Monad m, Monad n) => (m (Either String x) -> n (Either String y)) -> ConnEitherT m x -> ConnEitherT n y
mapConnEitherT = mapConnT . mapExceptT

--mapConnEither :: (Either String x -> Either String y) -> ConnEither x -> ConnEither y
--mapConnEither = mapConn . mapExcept
                
data Pagination = Pagination {
      current :: Maybe Int
    , perPage :: Maybe Int
    , total   :: Maybe Int
} deriving (Generic, Show, Typeable)

instance FromJSON Pagination 
instance ToJSON Pagination 

dbLocation :: FilePath
dbLocation = "../db.sqlite"

connectDB :: IO Connection
connectDB = connectSqlite3 dbLocation


