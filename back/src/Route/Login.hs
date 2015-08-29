module Route.Login where
    
import Happstack.Server
import DB
import DB.Auth
import Route.Internal
import Control.Monad.Trans
import Control.Monad
import Text.JSON

loginResponse :: ConnServer Response
loginResponse = do
    email <- lift $ look "email"
    pwd <- lift $ look "password"
    (uid, username, email, token) <- connEitherToServer $ loginByEmail email pwd
    addCookie Session $ mkCookie "uid" $ show uid
    addCookie Session $ mkCookie "token" token
    ok $ toResponse $ encode $ toJSObject [
              ("uid", show uid)
            , ("username", username)
            , ("email", email)
            , ("token", token)
        ]
