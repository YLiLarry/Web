module DB.Auth where

import DB.Internal
import Data.Unique
import DB.User

type Token = String

getUserIDByToken :: IConnection c => Token -> c -> IO (Maybe ID)
getUserIDByToken token = getBy ("token=", token) "Auth" "userID"
    
newUserIDToken :: IConnection c => ID -> c -> IO Token
newUserIDToken uid conn = do
    stmt <- prepare conn "REPLACE INTO Auth (userID, token) VALUES (?,?)"
    token <- fmap (show . hashUnique) newUnique
    execute stmt $ [ toSql uid, toSql token ]
    commit conn
    return token
    

login :: IConnection c => Email -> Password -> c -> IO (Either String (ID, Username, Email, Token))
login email pwd conn = do
    maybeID <- getUserByEmail email conn
    case maybeID of
        Nothing    -> return $ Left "User Does not exist"
        (Just uid) -> do
            maybePwd <- getUserPassword uid conn
            if (maybePwd) /= (Just pwd)
            then return $ Left "Wrong password"
            else do
                (Just usrname) <- getUsername uid conn
                token <- newUserIDToken uid conn
                return $ Right (uid, usrname, email, token)
