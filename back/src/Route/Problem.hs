{-# LANGUAGE RankNTypes #-}

module Route.Problem where

import Happstack.Server
import Text.JSON as JSON (encode)
import DB
import DB.Problem
import Route.Internal
import Control.Monad.Trans (lift)
import Route.Internal (toJObjs, toJObj, toJObjsPag)
import Helper

problemCollection :: ConnServer Response
problemCollection conn = do
    current <- lookRead "current"
    perPage <- lookRead "perpage"
    result <- lift $ getAllProblems (newPagination current perPage) conn
    ok $ toResponse $ toJObjsPag "problems" result

problemElement :: ConnServer Response
problemElement conn = path handler
    where handler pid = do
            result <- lift $ getProblemByID (read pid) conn
            (if (result == Nothing) then notFound else ok) $ toResponse $ toJObj "problems" result

userSolutions :: ID -> ConnServer Response
userSolutions uid conn = do
    current <- lookRead "current"
    perPage <- lookRead "perpage"
    result <- lift $ getUserSolutions uid (newPagination current perPage) conn
    ok $ toResponse $ toJObjs "problems" result

userSolution :: ID -> ID -> ConnServer Response
userSolution uid pid conn = do
    result <- lift $ getUserSolution uid pid conn
    (if (result == Nothing) then notFound else ok) $ toResponse $ toJObj "problems" result
