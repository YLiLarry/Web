import DB
import TestTools

main :: IO ()
main = do
    conn <- connectDB
    hspec $ do
        describe "Create a token for a uid and read token for that uid" $ do
            it "Should be equal" $ do
                t <- newUserIDToken 1412 conn
                uid <- getUserIDByToken t conn
                uid `shouldBe` (Just 1412)
                uid <- getUserIDByToken t conn
                uid `shouldBe` (Just 1412)
            
            