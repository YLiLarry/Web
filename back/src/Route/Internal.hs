module Route.Internal where

import Text.JSON (encode, toJSObject, showJSON, showJSONs)
import DB(Pagination(..))
import Helper


toJObjs :: String -> [[(String,String)]] -> String
toJObjs objType ls = encode $ toJSObject [(objType, map toJSObject ls)]

toJObj :: String -> Maybe [(String,String)] -> String
toJObj _       Nothing    = "{}"
toJObj objType (Just obj) = encode $ toJSObject [(objType, toJSObject obj)]

toJObjsPag :: String -> ([[(String,String)]], Pagination) -> String
toJObjsPag objType (ls,pag) = encode $ toJSObject $ 
    [("meta", showJSON $ toJSObject [("total", total pag)]), (objType, showJSONs $ map toJSObject ls)]

