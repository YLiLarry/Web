{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module DB.User where

import Prelude hiding (id)
import DB.Internal as DB
import GHC.Generics
import DB.Internal.PropertyMethodMap as PM
import Data.Typeable
    
type Name     = String
type Email    = String
type Password = String
type Token    = String

data User = User {
      idx      :: Maybe ID
    , name     :: Maybe Name
    , email    :: Maybe Email
    , password :: Maybe Password
    , auth     :: Maybe Auth
} deriving (Generic, Show, Typeable)
    
data Auth = Auth {
      token  :: Maybe Token
    , userID :: Maybe ID
} deriving (Generic, Show, Typeable)

instance FromJSON User
instance ToJSON User
instance Model User

instance FromJSON Auth
instance ToJSON Auth
instance Model Auth

setAuth :: User -> ConnEither User
setAuth user = do
    a <- getOne $ PM.selectWhere "userID = ?" "Auth" ["token", "userID"] [toSql $ idx user]
    return $ user { auth = Just a }

getUserByEmail :: FromConnEitherT IO m => Email -> m User
getUserByEmail email = getOne $ PM.selectWhere "email = ?" "User" ["idx", "name", "email", "password"] [toSql email]

getUserByID :: FromConnEitherT IO m => ID -> m User
getUserByID uid = getOne $ PM.selectID uid "User" ["idx", "name", "email", "password"]

newUser :: FromConnEitherT IO m => User -> m User
newUser user = do
    uid <- save user $ PM.insertInto "User" ["name", "email", "password"]
    return $ user { idx = Just uid }

