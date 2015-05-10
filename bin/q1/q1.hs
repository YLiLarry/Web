{-# LANGUAGE Safe #-}
----------------------
answer :: String
answer = "string"
----------------------
main :: IO ()
main = print $ userAnswer

userAnswer :: String
userAnswer = answer

