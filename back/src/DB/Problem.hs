{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module DB.Problem where

import Prelude hiding (id)

import DB.Internal
import Control.Monad
import Text.JSON as JSON (encode, JSON, toJSObject)
import DB.Internal.Query as Q
import DB.Internal.PropertyMethod as M
import DB.Internal.PropertyMethodMap as MP
import DB.User (User)
import qualified DB.User as U

data Problem = Problem {
      idx          :: Maybe ID
    , title        :: Maybe String
    , content      :: Maybe String
    , answerCount  :: Maybe Integer
    , solvedByUser :: Maybe ID
    , prev         :: Maybe Integer
    , next         :: Maybe Integer
    , userSolution :: Maybe String
} deriving (Generic, Show, Typeable)

instance FromJSON Problem
instance ToJSON Problem
instance Model Problem
    
data UserSolution = UserSolution {
      problemID :: ID
    , userID    :: ID
} deriving (Generic, Show, Typeable)

instance FromJSON UserSolution
instance ToJSON UserSolution
instance Model UserSolution
    
problem :: Problem
problem = Problem {
      idx = Nothing
    , title = Nothing
    , content = Nothing
    , answerCount = Nothing
    , solvedByUser = Nothing
    , prev = Nothing
    , next = Nothing
    , userSolution = Nothing 
}
    
newProblem :: (FromConnEither m) => Problem -> m ID
newProblem p = save p $ MP.insertInto "Problem" ["title", "content", "answerCount"]

getProblemByID :: (FromConnEither m) => ID -> m Problem
getProblemByID id = getOne $ newGetMethods
    [     M.selectID id "Problem" ["idx","title","content","answerCount"] 
        , M.prevID id "Problem" ["prev"] 
        , M.nextID id "Problem" ["next"] 
    ]
    
setSolvedByUser :: User -> Problem -> GetMethods Problem
setSolvedByUser usr prb = newGetMethods 
    [ newGetMethod 
        ["solvedByUser"] 
        (Q.selectWhere "userID = ? AND problemID = ?" "UserSolution" ["idx"]) 
        [ toSql (U.idx usr), toSql (idx prb) ] 
    ]
        

setPrevNextID :: Problem -> GetMethods Problem
setPrevNextID p = newGetMethods 
        [     M.nextID (p # idx) "Problem" ["next"] 
            , M.prevID (p # idx) "Problem" ["prev"] 
        ]

saveUserSolution :: (FromConnEither m) => UserSolution -> m ID
saveUserSolution sol = save sol $ MP.insertInto "UserSolution" ["userID", "problemID"]

getProblems :: (FromResultPagination m) => Pagination -> m [Problem]
getProblems pag =
    getP "Problem" pag (newGetMethods [
            M.selectP pag "Problem" ["idx","title","content","answerCount"] 
        ])
    >>= fromResultPagination . decroMapGetS [ setPrevNextID ]

getProblemWithUserSolution :: (FromConnEither m) => ID -> User -> m Problem
getProblemWithUserSolution pid usr = do
    p <- getProblemByID pid
    decroGet (setSolvedByUser usr p) p

getProblemsWithUserSolution :: (FromResultPagination m) => Pagination -> User -> m [Problem]
getProblemsWithUserSolution pag usr = 
    getProblems pag
    >>= fromResultPagination . decroMapGetS [ setSolvedByUser usr ]
    
-- deleteProblem :: Problem -> 