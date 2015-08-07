import Route.Compile as C
import TestTools
import Data.Either (isLeft)
import Data.Maybe (isJust)
import Data.Text.Lazy as T (empty)
import Control.Monad (msum, mplus)
import Control.Applicative ((<|>))
import DB

main :: IO ()
main = do
    conn <- connectDB
    hspec $ do
        describe "Compiles a user code with syntax error" $ do
            it "Should be equal" $ do
                1 `shouldBe` 1
                
        describe "Runs a user code of infinite loop." $ do
            it "Should time out." $ do
                e <- C.run "./test/CompileTest/loop/" T.empty
                e `shouldSatisfy` isLeft
                
        describe "Tests runForEachInput with user code infinite loop" $ do
            it "Should time out." $ do
                e <- C.runForEachInput "./test/CompileTest/loop/" "./test/CompileTest/loop/"
                e `shouldSatisfy` isJust
        
            