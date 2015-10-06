{-# LANGUAGE RankNTypes #-}

import TestTools

import Control.Applicative (liftA2)
import Test.QuickCheck.Property (morallyDubiousIOProperty, property, Result)
import Test.Hspec.QuickCheck (prop)
import Control.Monad
import Control.Monad.Trans.Reader
import DB.Problem
import DB.Internal
import Data.Either

main :: IO ()
main = do
    conn <- connectDB
    hspec $ do
        describe "Problem.hs" $ do
            it "Create a new problem and read" $ do
                result <- quickCheckResult
                    $ (\x y -> ioProperty $ liftM isRight $ runConnEitherT (prop_newProblem x y) conn)
                result `shouldSatisfy` isSuccess
            it "Read all problems" $ do
                result <- quickCheckResult
                    $ (\x y -> (getPositive x * getPositive y) < 1000 ==> ioProperty $ liftM isRight $ runConnEitherT (prop_getProblems x y) conn)
                result `shouldSatisfy` isSuccess
            it "Create solutions and read" $ do
                result <- quickCheckResult
                    $ (\x y -> ioProperty $ liftM isRight $ runConnEitherT (prop_newUserSolution x y) conn)
                result `shouldSatisfy` isSuccess
    disconnect conn
    
    where 
        prop_newProblem :: String -> String -> ConnEither Bool
        prop_newProblem x y = do
            newid <- newProblem problem { title = x, content = y }
            p <- getProblemByID newid
            return $ x == title problem
        
        prop_getProblems :: Positive Int -> Positive Int -> ConnEither Bool
        prop_getProblems c p = do
            collection <- toConnEitherT $ getProblems $ Pagination { current = getPositive c, perPage = getPositive p, total = Nothing }
            return $ length collection > 0
            
        prop_newUserSolution :: Positive Integer -> Positive Integer -> ConnEither ID
        prop_newUserSolution uid' pid' = do
            let uid = getPositive uid'
            let pid = getPositive pid'
            newUserSolution $ UserSolution { userID = uid, problemID = pid }
            