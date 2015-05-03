module Main where

import Happstack.Lite
import Text.Blaze.Html (preEscapedToHtml)
import Data.Text
import Happstack.Server (askRq, ServerMonad(..), rqUri)
import Happstack.Server.FileServe.BuildingBlocks (guessContentType)
import Data.Maybe (fromMaybe)
import Control.Monad
import DB
import Login

main :: IO ()
main = do
    conn <- connectDB
    serve Nothing $ msum [
              dir "api" $ dir "login" $ loginResponse conn
            , loadAnyPage
            , nullDir >> homePage
            , homePage
            , page404  
        ]

homePage :: ServerPart Response
homePage = load "index.html"

page404 :: ServerPart Response
page404 = load "404.html"

loadAnyPage :: ServerPart Response
loadAnyPage = load =<< fullpath 
    
-- | Get the full request path without quries
fullpath :: ServerMonad m => m String
fullpath = liftM rqUri askRq

frontend :: FilePath -> FilePath
frontend = ("src/front/dist/" ++)

load :: String -> ServerPart Response
load str = serveFile (asContentType (fromMaybe "text/html" $ guessContentType mimeTypes uri)) uri
    where uri = frontend str ++ if '.' `elem` str then "" else "index.html"

--newtype HtmlString = HtmlString {
--        getHtmlString :: String
--    } 
    
--instance ToMessage HtmlString where
--    toContentType _ = B.pack "text/html; charset=UTF-8" 
--    toMessage = toMessage . getHtmlString 

