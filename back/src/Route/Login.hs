module Route.Login where
    
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
            addCookie Session $ mkCookie "uid" $ show uid
            addCookie Session $ mkCookie "token" token
            ok $ toResponse $ encode $ toJSObject [
                      ("uid", show uid)
                    , ("username", username)
                    , ("email", email)
                    , ("token", token)
                ]

