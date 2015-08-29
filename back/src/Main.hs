module Main where

import Text.Blaze.Html (preEscapedToHtml)
import Data.Text
import Happstack.Server (askRq, ServerMonad(..), rqUri, dirs, badRequest, readCookieValue, anyPath)
import Happstack.Server.FileServe.BuildingBlocks (guessContentType)
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.Trans (lift)
import DB
import Route
import Helper

main :: IO ()
main = do
    conn <- connectDB
    serve Nothing $ msum [
              dirs "api/v1" $ msum [
                      dir "login" $ loginResponse conn
                    , protectedRoutes conn
                    , dir "register" $ register conn
                    , dir "problems" $ problemElement conn
                    , dir "problems" $ problemCollection conn
                    , badRequest $ toResponse "Your request is illegal."
                ]
            , msum [
                      loadResource
                    , homePage
                ]
        ]

protectedRoutes :: IConnection c => c -> ServerPart Response
protectedRoutes conn = do 
    uid <- readCookieValue "uid"
    --token <- readCookieValue "token"
    msum [
              dir "problems" $ path (\pid -> userSolution uid (read pid) conn)
            , dir "problems" $ userSolutions uid conn
            , dir "compile" $ checkAnswer uid conn
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

--newtype HtmlString = HtmlString {
--        getHtmlString :: String
--    } 
    
--instance ToMessage HtmlString where
--    toContentType _ = B.pack "text/html; charset=UTF-8" 
--    toMessage = toMessage . getHtmlString 

