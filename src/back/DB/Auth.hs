module DB.Auth where

import DB.Internal
import Data.Unique

type UID = Integer
type ID = Integer
type Username = String
type Token = String

getUserIDByToken :: IConnection c => Token -> c -> IO (Maybe UID)
getUserIDByToken token = getBy ("token=", token) "Auth" "userID"
    
newUserIDToken :: IConnection c => UID -> c -> IO Token
newUserIDToken uid conn = do
    stmt <- prepare conn "REPLACE INTO Auth (userID, token) VALUES (?,?)"
    token <- fmap (show . hashUnique) newUnique
    execute stmt $ [ toSql uid, toSql token ]
    commit conn
    return token
    
