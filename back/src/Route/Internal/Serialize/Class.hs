{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Route.Internal.Serialize.Class where

import Data.Aeson as A
import Data.Aeson.Encode as A
import Helper
import Happstack.Server
import Happstack.Server.Monads
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import GHC.Generics
import DB hiding (Meta)
import qualified DB as DB (Meta(..))
import Data.Typeable
import Route.Internal.Class
import qualified DB.User as U (User(..))
import DB.Problem (Problem)

data Serialize = Serialize {
      name   :: String
    , value  :: Value
    , meta   :: Value
} deriving (Show, Generic, Typeable)

instance ToJSON Serialize
instance FromJSON Serialize
instance Model Serialize

deriving instance Typeable Meta 

class (ToJSON a, Typeable a) => ToSerialize a where
    
    getName :: a -> String
    getName = show . typeOf
    
    getValue :: a -> Value
    getValue = toJSON
    
    getMeta  :: a -> Value
    getMeta = const Null
    
    toSerialize :: a -> Serialize
    toSerialize a = Serialize {
          name  = getName  a
        , value = getValue a
        , meta  = getMeta  a
    }

    sanitize :: a -> a
    sanitize = id

-- instance (ToSerialize a) => ToSerialize (a, Meta) where
    -- getName (x,_) = getName x
    -- getValue = toJSON . fst
    -- getMeta (_,NoMeta) = [("pagination",toJSON x)] 
    -- getMeta (_,DB.Meta x) = [("pagination",toJSON x)] 
    
instance ToMessage Serialize where
    toResponse x = toResponse 
        $ T.toLazyText 
        $ A.encodeToTextBuilder 
        $ toJSON 
        $ object [(T.toLower $ T.pack $ name x, value x), ("meta", meta x)]
    
ready :: (ToSerialize a) => ConnServer a -> Ready
ready x = mapConnT f x
    where
        f :: (ToSerialize a) => ExceptT String (WriterT Meta (ServerPartT IO)) a -> ServerPart Response
        f x = fmap g $ runWriterT $ runExceptT x
        g (Left msg, _) = (toResponse msg) { rsCode = 400 }
        g (Right vl, m) = toResponse $ (toSerialize $ sanitize vl) { meta = toJSON m }

instance (ToSerialize a) => ToSerialize [a] where
    getName = show . typeOf . head 
        
instance ToSerialize Char
instance ToSerialize U.User where
    sanitize a = a { 
          U.auth = Nothing 
        , U.password = Nothing
    }
instance ToSerialize Problem
