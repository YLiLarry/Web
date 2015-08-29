{-# LANGUAGE RankNTypes #-}

module DB.Auth where

import DB.Internal
import System.Random (randomIO)
import DB.User
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Char8 as BS (pack, unpack)
import Helper.ByteString (toHex)
import Data.Either
import Data.Maybe
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT, mapReaderT)

type Token = String

getUserIDByToken :: Token -> ConnMaybe ID
getUserIDByToken token = getBy ("token=", token) "Auth" "userID"
    
-- | Takes a uid and a salt (normally the user email), assign a token for the uid in the database, and returns the token
newTokenByID :: ID -> String -> Conn Token
newTokenByID uid salt = do
    token <- lift $ newToken salt
    replace "Auth" ["userID", "token"] [toSql uid, toSql token]
    return token
    
-- | Takes a salt (normally the user email) and returns a token
newToken :: String -> IO Token
newToken salt = do
    rnd <- randomIO 
    let token = toHex $ hash $ BS.pack $ salt ++ show (rnd :: Double)
    return $ token

loginByEmail :: Email -> Password -> ConnEither (ID, Username, Email, Token)
loginByEmail email pwd = do
    uid <- connMaybeToEither "User does not exist." $ getIDByEmail email
    loginByID uid pwd
    
loginByID :: ID -> Password -> ConnEither (ID, Username, Email, Token)
loginByID uid pwd = do
    (pwd',name,email,token) <- connMaybeToEither "User does not exist." getThings
    lift $ ExceptT $ return $ if (pwd /= pwd') then Left "Wrong password." else Right ()
    return (uid,name,email,token)
    where
        getThings :: ConnMaybe (Password, Username, Email, Token)
        getThings = do
            pwd   <- getPasswordByID uid
            name  <- getNameByID     uid
            email <- getEmailByID    uid
            token <- mapReaderT lift $ newTokenByID uid email
            return (pwd, name, email, token)
            
