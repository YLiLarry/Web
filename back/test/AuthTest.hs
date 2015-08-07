import TestTools
import Helper
import DB

main :: IO ()
main = do
    conn <- connectDB
    hspec $ do
        describe "Create a token for a uid and read token for that uid" $ do
            it "Should be equal" $ do
                t <- newUserIDToken 1412 "email" conn
                uid <- getUserIDByToken t conn
                uid `shouldSatisfy` isJust
                uid <- getUserIDByToken t conn
                uid `shouldSatisfy` isJust
            
            