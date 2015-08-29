{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module DB.Internal.Class where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Database.HDBC (IConnection(..))
import GHC.Generics
import Data.Aeson
        
type TableName = String
type ColumnName = String
type IOMaybe a = IO (Maybe a)
type ID = Integer
type Conn a = forall c. IConnection c => ReaderT c IO a
type ConnT m v = forall c. IConnection c => ReaderT c m v
type MaybeIO = MaybeT IO
type ConnMaybe v = ConnT MaybeIO v
type ConnEither v = ConnT (ExceptT String IO) v

data Pagination = Pagination {
      current :: Maybe Int
    , perPage :: Maybe Int
    , total   :: Maybe Int
} deriving (Generic, Show)

instance FromJSON Pagination 
instance ToJSON Pagination 

