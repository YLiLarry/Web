{-# LANGUAGE FlexibleContexts #-}

module DB.User where

import DB.Internal
import Data.Convertible
    
type UserID   = Integer
type Username = String
type Email    = String
type Password = String

    
getByUserID :: (Convertible SqlValue b, IConnection c) => UserID -> ColumnName -> c -> IOMaybe b
getByUserID id = getByID id "User"

getUsername :: IConnection c => UserID -> c -> IOMaybe Username
getUsername id = getByUserID id "username"  
    
getUserByEmail :: IConnection c => Email -> c -> IOMaybe UserID
getUserByEmail name = getUserIDBy ("email=", name)

newUser :: IConnection c => Username -> Email -> Password -> c -> IOMaybe UserID
newUser name email password conn = do
    stmt <- prepare conn "INSERT INTO User (username, email, password) VALUES (?,?,?)"
    execute stmt [ toSql name, toSql email, toSql password ]
    commit conn
    getUserByEmail email conn

getUserPassword :: IConnection c => UserID -> c -> IOMaybe Password
getUserPassword id = getByUserID id "password"
    

    