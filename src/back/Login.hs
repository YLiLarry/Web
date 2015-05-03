module Login where
    
import Happstack.Server
import DB
import Control.Monad.Trans
import Control.Monad

import Debug.Trace

_p x = trace (show x) x

loginResponse :: IConnection c => c -> ServerPart Response
loginResponse conn = do
    email <- look "email"
    pwd <- look "password"
    result <- lift $ login email pwd conn
    case result of
        (Left msg) -> unauthorized $ toResponse msg
        (Right token) -> do
            addCookie Session $ mkCookie "session" token 
            ok $ toResponse token

login :: IConnection c => Email -> Password -> c -> IO (Either String Token)
login email pwd conn = do
    maybeUID <- getUserByEmail email conn
    case maybeUID of
        Nothing    -> return $ Left "User Does not exist"
        (Just uid) -> do
            maybePwd <- getUserPassword uid conn
            if (maybePwd) /= (Just pwd)
            then return $ Left "Wrong password"
            else fmap Right $ newUserIDToken uid conn
                
