{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module DB.Problem where

import Prelude hiding (id)

import DB.Internal
import Control.Monad
import Text.JSON as JSON (encode, JSON, toJSObject)
import DB.Internal.Query as Q
import DB.Internal.PropertyMethod as M
import DB.Internal.PropertyMethodMap as MP

data Problem = Problem {
      id           :: Maybe Integer
    , title        :: Maybe String
    , content      :: Maybe String
    , answerCount  :: Maybe Integer
    , solvedByUser :: Maybe ID
    , prev         :: Maybe Integer
    , next         :: Maybe Integer
    , userSolution :: Maybe String
} deriving (Generic, Show)

instance FromJSON Problem
instance ToJSON Problem
instance Model Problem
    
data UserSolution = UserSolution {
      problemID :: Integer
    , userID :: Integer
} deriving (Generic, Show)

instance FromJSON UserSolution
instance ToJSON UserSolution
instance Model UserSolution
    
problem :: Problem
problem = Problem {
      id = Nothing
    , title = Nothing
    , content = Nothing
    , answerCount = Nothing
    , solvedByUser = Nothing
    , prev = Nothing
    , next = Nothing
    , userSolution = Nothing 
}

defaultSaveProblem :: SaveMethods Problem
defaultSaveProblem = MP.insertInto "Problem" ["title", "content", "answerCount"]

defaultGetProblemByID :: ID -> GetMethods Problem
defaultGetProblemByID id = newGetMethods $ 
    [     M.selectID id "Problem" ["id","title","content","answerCount"] 
        , M.prevID id "Problem" ["prev"] 
        , M.nextID id "Problem" ["next"] 
    ]
    
newProblem :: Problem -> ConnEither ID
newProblem p = save p defaultSaveProblem

getProblemByID :: ID -> ConnEither Problem
getProblemByID id = liftM head $ get $ defaultGetProblemByID id
            
            
problemWithUserSolution :: ID -> ID -> ConnEither Problem
problemWithUserSolution usr pid = do 
    a <- get $ defaultGetProblemByID pid
    b <- get $ f [toSql usr, toSql pid]
    return $ extend (head a) (head b)
    where
        f p = newGetMethods [ newGetMethod ["solvedByUser"] (Q.selectWhere "userID = ? AND problemID = ?" "UserSolution" ["id"]) p ]

main = do
    conn <- connectSqlite3 "../../db.sqlite"
    t <- runExceptT $ runReaderT (getProblems (Pagination (Just 1) (Just 10) Nothing)) conn
    print (fmap (length.fst) t)
    print t
    
--getSolvedProblems :: ID -> Pagination -> Conn ([Problem], Pagination)
--getSolvedProblems uid pag = dbAll ByUser
    
--newUserSolution :: ID -> ID -> Conn ID
--newUserSolution uid pid = dbReplace (UserSolution uid pid)

--userSolvedProblem :: ID -> ID -> Conn Bool    
--userSolvedProblem uid pid = hasPair "UserSolution" ("userID", "problemID") (uid, pid)

defaultGetProblems :: Pagination -> GetMethods Problem
defaultGetProblems pag = newGetMethods [
        M.selectP pag "Problem" ["id","title","content","answerCount"] 
    ]

getProblems :: Pagination -> ConnEither ([Problem], Pagination)
getProblems pag = do 
    problems <- get $ defaultGetProblems pag
    pagi <- getTotal pag "Problem"
    return (problems, pagi)
    

