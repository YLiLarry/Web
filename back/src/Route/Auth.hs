module Route.Auth where

import DB
import Happstack.Server
import Text.JSON
import Control.Monad.Trans (lift)

register :: IConnection c => c -> ServerPart Response
register conn = do
    email <- look "email"
    password <- look "password"
    username <- look "username"
    lift $ newUser username email password conn
    result <- lift $ login email password conn
    case result of
        (Left msg) -> unauthorized $ toResponse msg
        (Right (uid, username, email, token)) -> do
            addCookie Session $ mkCookie "uid" (show uid)
            addCookie Session $ mkCookie "token" (show token)
            ok $ toResponse $ encode $ toJSObject [
                      ("uid", show uid)
                    , ("username", username)
                    , ("email", email)
                    , ("token", token)
                ]
