module Login where
    
import Happstack.Server
import DB
import Control.Monad.Trans
import Control.Monad
import Text.JSON

loginResponse :: IConnection c => c -> ServerPart Response
loginResponse conn = do
    email <- look "email"
    pwd <- look "password"
    result <- lift $ login email pwd conn
    case result of
        (Left msg) -> unauthorized $ toResponse msg
        (Right (uid, username, email, token)) -> do
            ok $ toResponse $ encode $ toJSObject [
                      ("uid", show uid)
                    , ("username", username)
                    , ("email", email)
                    , ("token", token)
                ]

login :: IConnection c => Email -> Password -> c -> IO (Either String (ID, Username, Email, Token))
login email pwd conn = do
    maybeUID <- getUserByEmail email conn
    case maybeUID of
        Nothing    -> return $ Left "User Does not exist"
        (Just uid) -> do
            maybePwd <- getUserPassword uid conn
            if (maybePwd) /= (Just pwd)
            then return $ Left "Wrong password"
            else do
                (Just usrname) <- getUsername uid conn
                token <- newUserIDToken uid conn
                return $ Right (uid, usrname, email, token)
                
