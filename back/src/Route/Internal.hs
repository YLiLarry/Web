module Route.Internal where

import Text.JSON (encode, toJSObject)

toJObjs :: String -> [[(String,String)]] -> String
toJObjs objType ls = encode $ toJSObject [(objType, map toJSObject ls)]
