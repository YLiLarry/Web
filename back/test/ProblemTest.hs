import TestTools

import Control.Applicative (liftA2)
import Test.QuickCheck.Property (morallyDubiousIOProperty, property, Result)
import Test.Hspec.QuickCheck (prop)
import Control.Monad
import DB

main :: IO ()
main = do
    conn <- connectDB
    hspec $ do
        describe "Problem.hs" $ do
            it "Create a new problem and read" $ do
                result <- quickCheckResult
                    $ (\x y -> ioProperty $ prop_newProblem conn x y)
                result `shouldSatisfy` isSuccess
            it "Read all problems" $ do
                result <- quickCheckResult
                    $ (\x y -> (getPositive x * getPositive y) < 1000 ==> ioProperty $ prop_getAllProblems conn x y)
                result `shouldSatisfy` isSuccess
            it "Create solutions and read" $ do
                result <- quickCheckResult
                    $ (\x y -> ioProperty $ prop_newUserSolution conn x y)
                result `shouldSatisfy` isSuccess
    disconnect conn
    
    where 
        prop_newProblem :: (IConnection c) => c -> String -> String -> IO Bool
        prop_newProblem conn x y = do
            (Just newid) <- newProblem defaultProblem { problemTitle = x, problemContent = y } conn
            (Just problem) <- getProblemByID newid conn
            return $ x == problemTitle problem
        
        prop_getAllProblems :: (IConnection c) => c -> Positive Int -> Positive Int -> IO Bool
        prop_getAllProblems conn c p = do
            collection <- getAllProblems (Pagination (getPositive c) (getPositive p)) conn
            return $ length collection > 0
            
        prop_newUserSolution :: (IConnection c) => c -> Positive Integer -> Positive Integer -> IO Bool
        prop_newUserSolution conn uid' pid' = do
            let uid = getPositive uid'
            let pid = getPositive pid'
            id <- newUserSolution uid pid conn
            userSolvedProblem uid pid conn
            