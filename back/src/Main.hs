module Main where

import Text.Blaze.Html (preEscapedToHtml)
-- import Data.Text
import Happstack.Server
import Happstack.Server.FileServe.BuildingBlocks (guessContentType)
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.Trans (lift)
import DB
import Route
import Helper
import Route.Internal
import Control.Exception

main :: IO ()
main = do
    conn <- connectDB
    let withConn f = runReaderT (ready f) conn
    simpleHTTP nullConf $ do
        decodeBody $ defaultBodyPolicy "/tmp/" 1024 1024 1024
        handleInternalError $ msum [
                dirs "api/v1" $ msum [
                          dir "login" $ withConn loginResponse
                        , protectedRoutes conn
                        , dir "register" $ withConn register
                        , dir "problems" $ path (withConn . problem . read)
                        , dir "problems" $ withConn problems
                        , badRequest $ toResponse "Your request is illegal."
                    ]
                , msum [
                          loadResource
                        , homePage
                    ]
            ]

-- protectedRoutes :: IConnection c => c -> ServerPart Response
protectedRoutes conn = do 
    let withConn f = runReaderT (ready f) conn
    usr <- guardLogin conn
    msum [
              dir "problems" $ path (\pid -> withConn $ problemWithUserSolution (read pid) usr)
            , dir "problems" $ withConn $ problemsWithUserSolution usr 
            , dir "compile"  $ withConn $ checkAnswer usr 
        ]

    
homePage :: ServerPart Response
homePage = load "index.html"

page404 :: ServerPart Response
page404 = load "404.html"

loadResource :: ServerPart Response
loadResource = load =<< fullpath 
    
-- | Get the full request path without quries
fullpath :: ServerMonad m => m String
fullpath = liftM rqUri askRq

frontend :: FilePath -> FilePath
frontend = ("../front/dist/" ++)

load :: String -> ServerPart Response
load str = serveFile (asContentType (fromMaybe "text/html" $ guessContentType mimeTypes uri)) uri
    where uri = frontend str ++ if '.' `elem` str then "" else "index.html"

handleInternalError :: ServerPart Response -> ServerPart Response
handleInternalError = mapServerPartT (handle f)
    where
        f :: SomeException -> UnWebT IO Response
        f e = return $ Just
            (Left $ toResponse $ show e, filterFun (\x -> x {rsCode = 500}))
            