{-# LANGUAGE FlexibleContexts #-}

module Route.Auth where
    
import Happstack.Server
import DB
import DB.Auth as A
import DB.User as U
import Route.Internal
import Control.Monad.Trans
import Control.Monad
import Text.JSON
import Prelude hiding (id)
import Data.Maybe
import Helper

setUserCookies :: User -> ConnServer ()
setUserCookies usr = do
    addCookie Session $ mkCookie "uid" $ show $ fromJust $ U.idx usr
    addCookie Session $ mkCookie "token" $ fromJust $ U.token $ fromJust $ U.auth usr

expireUserCookies :: ConnServer ()
expireUserCookies = do
    expireCookie "uid" 
    expireCookie "token"
    

loginResponse :: ConnServer User
loginResponse = do
    email <- look "email"   
    pwd   <- look "password"
    usr   <- loginByEmail email pwd
    setUserCookies usr
    return usr
    
guardLogin :: Connection -> ServerPart User
guardLogin conn = do
    uid <- readCookieValue "uid"
    eitherUser <- runConnServerT (U.getUserByID uid) conn
    case fst eitherUser of
        Left msg -> mzero
        Right vl -> return vl


register :: ConnServer User
register = do
    email <- look "email"
    pwd   <- look "password"
    name  <- look "username"
    let usr = User {
              U.idx      = Nothing
            , U.name     = Just name
            , U.email    = Just email
            , U.password = Just pwd
            , U.auth     = Nothing
        }
    usr <- newUser usr
    setUserCookies usr
    loginByID (fromJust $ idx usr) pwd
    
logout :: ConnServer Bool
logout = do
    expireUserCookies
    return True
    
