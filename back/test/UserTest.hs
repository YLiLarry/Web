import TestTools

main :: IO ()
main = do
    conn <- connectDB
    hspec $ do
        describe "User.hs" $ do
            it "Create a new user with username, email and password" $ do
                name <- generate arbitrary
                email <- generate arbitrary
                uid1 <- newUser name email "123" conn
                uid2 <- getUserByEmail email conn
                uid1 `shouldBe` uid2
    disconnect conn
    
