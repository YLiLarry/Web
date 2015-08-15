module Route.Problem where

import Happstack.Server
import Text.JSON as JSON (encode)
import DB
import Control.Monad.Trans (lift)
import Route.Internal (toJObjs, toJObj, toJObjsPag)
import Helper

problemCollection :: IConnection c => c -> ServerPart Response
problemCollection conn = do
    current <- lookRead "current"
    perPage <- lookRead "perpage"
    result <- lift $ getAllProblems (newPagination current perPage) conn
    ok $ toResponse $ toJObjsPag "problems" result

problemElement :: IConnection c => c -> ServerPart Response
problemElement conn = path handler
    where handler pid = do
            result <- lift $ getProblem (read pid) conn
            (if (result == Nothing) then notFound else ok) $ toResponse $ toJObj "problems" result

userSolutions :: IConnection c => ID -> c -> ServerPart Response
userSolutions uid conn = do
    current <- lookRead "current"
    perPage <- lookRead "perpage"
    result <- lift $ getUserSolutions uid (newPagination current perPage) conn
    ok $ toResponse $ toJObjs "problems" result

userSolution :: IConnection c => ID -> ID -> c -> ServerPart Response
userSolution uid pid conn = do
    result <- lift $ getUserSolution uid pid conn
    (if (result == Nothing) then notFound else ok) $ toResponse $ toJObj "problems" result
