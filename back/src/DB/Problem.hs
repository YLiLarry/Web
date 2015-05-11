{-# LANGUAGE FlexibleContexts #-}

module DB.Problem where

import DB.Internal
--import Data.Text

data Problem = Problem {
      problemID :: Integer
    , problemTitle :: String
    , problemContent :: String
    , problemAnswerCount :: Integer
}


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
        return $ Just $ Problem {
                  problemID = id
                , problemTitle = title
                , problemContent = content
                , problemAnswerCount = answerCount
            }

getAllProblems :: IConnection c => Pagination -> c -> IO [[(ColumnName,String)]]
getAllProblems pag conn = getAll "Problem" ["title","content","answerCount"] pag conn 
