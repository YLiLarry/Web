{-# LANGUAGE FlexibleContexts #-}

module DB.Problem where

import DB.Internal
--import Data.Text
import Control.Monad (join)
import Text.JSON as JSON (encode, JSON, toJSObject)

data Problem = Problem {
      problemID :: Maybe Integer
    , problemTitle :: String
    , problemContent :: String
    , problemAnswerCount :: Integer
    , problemSolvedByUser :: Bool
}

defaultProblem :: Problem
defaultProblem = Problem {
      problemID = Nothing
    , problemTitle = "Empty"
    , problemContent = "Empty"
    , problemAnswerCount = 0
    , problemSolvedByUser = False
}

instance JSON Problem

newProblem :: IConnection c => Problem -> c -> IOMaybe ID
newProblem problem conn = do
    stmt <- prepare conn "INSERT INTO Problem (title, content, answerCount) VALUES (?,?,?)"
    execute stmt [ 
              toSql $ problemTitle problem
            , toSql $ problemContent problem
            , toSql $ problemAnswerCount problem 
        ]
    commit conn
    fmap (fromSql.head.head) $ quickQuery conn "SELECT last_insert_rowid();" []

getByProblemID :: (Convertible SqlValue b, IConnection c) => ID -> ColumnName -> c -> IOMaybe b
getByProblemID id = getByID id "Problem"

getProblemByID :: IConnection c => ID -> c -> IOMaybe Problem
getProblemByID id conn = do
    maybeTitle <- getByProblemID id "title" conn
    if (maybeTitle == Nothing) then return Nothing
    else do
        (Just title) <- return maybeTitle
        (Just content) <- getByProblemID id "content" conn
        (Just answerCount) <- getByProblemID id "answerCount" conn
        return $ Just $ defaultProblem {
                  problemID = Just id
                , problemTitle = title
                , problemContent = content
                , problemAnswerCount = answerCount
            }
            
getProblem :: IConnection c => ID -> c -> IO (Maybe [(ColumnName,String)])
getProblem pid conn = do
    prev <- prevID pid "Problem" conn
    next <- nextID pid "Problem" conn
    prb <- getOne pid "Problem" ["id","title","content","answerCount"] conn
    return $ maybe Nothing
        (Just . ([("next", maybe "" show next), ("prev", maybe "" show prev)] ++)) 
        prb
            
getUserSolutions :: IConnection c => ID -> Pagination -> c -> IO [[(ColumnName,String)]]
getUserSolutions uid pag conn = join $ fmap sequence $ (fmap.fmap) lambda $ getAllProblems pag conn
    where
        lambda assoc = do
            let (Just pid) = lookup "id" assoc
            bool <- userSolvedProblem uid (read pid) conn
            return $ ("solvedByUser", JSON.encode bool) : assoc

getUserSolution :: IConnection c => ID -> ID -> c -> IOMaybe [(ColumnName,String)]
getUserSolution uid pid conn = do
    bool <- userSolvedProblem uid pid conn
    prob <- getProblem pid conn
    return $ fmap (("solvedByUser", JSON.encode bool):) prob
    
newUserSolution :: IConnection c => ID -> ID -> c -> IO ID
newUserSolution uid pid = new "UserSolution" ["userID", "problemID"] [toSql uid, toSql pid]

userSolvedProblem :: IConnection c => ID -> ID -> c -> IO Bool    
userSolvedProblem uid pid conn = hasPair "UserSolution" ("userID", "problemID") (uid, pid) conn

getAllProblems :: IConnection c => Pagination -> c -> IO [[(ColumnName,String)]]
getAllProblems pag conn = getAll "Problem" ["id","title","content","answerCount"] pag conn 

