module DB.Auth where

import DB.Internal
import Data.Unique
import System.Random (randomIO)
import DB.User
import Crypto.Hash.SHA256 (hash)
--import Data.ByteString as BS (ByteString)
import Data.ByteString.Char8 as BS (pack, unpack)
import Helper.ByteString (toHex)

type Token = String

getUserIDByToken :: IConnection c => Token -> c -> IO (Maybe ID)
getUserIDByToken token = getBy ("token=", token) "Auth" "userID"
    
-- | Takes a uid and a salt (normally the user email), assign a token for the uid in the database, and returns the token
newUserIDToken :: IConnection c => ID -> String -> c -> IO Token
newUserIDToken uid salt conn = do
    stmt <- prepare conn "REPLACE INTO Auth (userID, token) VALUES (?,?)"
    token <- newToken salt
    execute stmt $ [ toSql uid, toSql token ]
    commit conn
    return token
    
-- | Takes a salt (normally the user email) and returns a token
newToken :: String -> IO Token
newToken salt = do
    rnd <- randomIO 
    let token = toHex $ hash $ BS.pack $ salt ++ show (rnd :: Double)
    print token
    return $ token

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
                token <- newUserIDToken uid email conn
                return $ Right (uid, usrname, email, token)
