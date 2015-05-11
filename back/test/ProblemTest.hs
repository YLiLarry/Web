import TestTools

import Control.Applicative (liftA2)
import Test.QuickCheck.Property (morallyDubiousIOProperty, property, Result)
import Test.Hspec.QuickCheck (prop)
import Control.Monad


main :: IO ()
main = do
    conn <- connectDB
    hspec $ do
        describe "Problem.hs" $ do
            it "Create a new problem and read" $ do
                title <- generate arbitrary :: IO String
                content <- generate arbitrary :: IO String
                (Just newid) <- newProblem (Problem 0 title content 0) conn
                (Just problem) <- getProblemByID newid conn
                title `shouldBe` problemTitle problem
            it "Read all problems" $ do
                result <- quickCheckWithResult (stdArgs { maxSize = 5 }) 
                    $ (\x y -> ioProperty $ prop_getAllProblems conn x y)
                result `shouldSatisfy` isSuccess
    disconnect conn
    
    where 
        prop_getAllProblems :: (IConnection c) => c -> Positive Int -> Positive Int -> IO Bool
        prop_getAllProblems conn c p = do
            collection <- getAllProblems (Pagination (getPositive c) (getPositive p)) conn
            return $ length collection > 0
            
            