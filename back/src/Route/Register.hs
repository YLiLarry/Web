module Route.Register where

import DB
import Happstack.Server
import Text.JSON
import Control.Monad.Trans (lift)
import Prelude hiding (id)
import DB.Auth
import DB.User
import Route.Internal (ConnServer)

register :: ConnServer Response
register = do
    email <- lift $ look "email"
    password <- lift $ look "password"
    username <- lift $ look "username"
    result <- lift $ newUser username email password
    case result of
        (Left msg) -> unauthorized $ toResponse msg
        (Right uid) -> do
            loginByID uid password
            addCookie Session $ mkCookie "uid" (show uid)
            addCookie Session $ mkCookie "token" (show token)
            ok $ toResponse $ encode $ toJSObject [
                      ("uid", show uid)
                    , ("username", username)
                    , ("email", email)
                    , ("token", token)
                ]
                
        
