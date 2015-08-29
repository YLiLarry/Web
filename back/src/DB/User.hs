{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module DB.User where

import DB.Internal
import Data.Convertible
import Data.Either
import Data.Maybe
import Prelude hiding (id)
    
type UserID   = Integer
type Username = String
type Email    = String
type Password = String

data User = User {
      id :: Integer
    , name :: String
    , email :: String
    , password :: String
    , token :: String
} deriving (Generic, Show)
    
getByUserID :: (Convertible SqlValue b) => ColumnName -> UserID -> ConnMaybe b
getByUserID col id = getByID id "User" col

getNameByID :: UserID -> ConnMaybe Username
getNameByID = getByUserID "username"  
    
getIDByEmail :: Email -> ConnMaybe UserID
getIDByEmail name = getUserIDBy ("email=", name)

newUser :: Username -> Email -> Password -> ConnEither UserID
newUser name email password = newEither "User" ["username", "email", "password"] [toSql name, toSql email, toSql password]

getPasswordByID :: UserID -> ConnMaybe Password
getPasswordByID = getByUserID "password"
    
getEmailByID :: UserID -> ConnMaybe Email
getEmailByID = getByUserID "email"

