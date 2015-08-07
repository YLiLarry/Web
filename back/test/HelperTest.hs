{-# LANGUAGE OverloadedStrings #-}

import TestTools
import Helper
import Data.ByteString

main :: IO ()
main = do
    hspec $ do
        describe "Testing ByteString.toHex and ByteString.fromHex" $ do
            it "Should be equal" $ do                
                (fromHex $ toHex "test") `shouldBe` "test"
                