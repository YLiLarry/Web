{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module DB.Auth where

import Prelude hiding (id)
import qualified Prelude as P (id)
import DB.Internal
import System.Random (randomIO)
import DB.User as U
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Char8 as BS (pack, unpack)
import Helper.ByteString (toHex)
import Data.Either
import Data.Maybe
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe 
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Elevator
import DB.Internal.PropertyMethodMap as PM

defaultSaveAuth :: SaveMethods Auth
defaultSaveAuth = PM.replaceInto "Auth" ["userID", "token"] 

newUserAuth :: FromConnEither m => User -> m User
newUserAuth usr = fromConnEither $ do
    let i = idx usr
    let e = fromJust $ email usr
    t <- liftIO $ newToken e 
    let a = Auth { userID = i, token = Just t } 
    save a defaultSaveAuth :: ConnEither ID
    return $ usr { auth = Just a }
    
-- | Takes a salt (normally the user email) and returns a token
newToken :: String -> IO Token
newToken salt = do
    rnd <- randomIO 
    let token = toHex $ hash $ BS.pack $ salt ++ show (rnd :: Double)
    return $ token

loginByEmail :: FromConnEither m => Email -> Password -> m User
loginByEmail email pwd = fromConnEither $ do
    usr <- getUserByEmail email 
    loginByUser usr pwd
    
loginByID :: FromConnEither m => ID -> Password -> m User
loginByID uid pwd = fromConnEither $ do
    usr <- getUserByID uid
    loginByUser usr pwd
    
loginByUser :: FromConnEither m => User -> Password -> m User
loginByUser usr pwd = fromConnEither $ do
    let pwd' = fromJust $ password usr
    if pwd /= pwd' then throwError "Wrong password." else newUserAuth usr
    
-- logoutUser :: FromConnEither m => User -> m Bool
-- logoutUser usr = fromConnEither $ do
    