module Route.Problem where

import Happstack.Server
import Text.JSON as JSON (encode)
import DB
import Control.Monad.Trans (lift)
import Route.Internal (toJObjs, toJObj)

problemCollection :: IConnection c => c -> ServerPart Response
problemCollection conn = do
    current <- lookRead "current"
    perPage <- lookRead "perpage"
    result <- lift $ getAllProblems (Pagination current perPage) conn
    ok $ toResponse $ toJObjs "problems" result

problemElement :: IConnection c => ID -> c -> ServerPart Response
problemElement pid conn = do
    result <- lift $ getProblem pid conn
    (if (result == Nothing) then notFound else ok) $ toResponse $ toJObj "problem" result

userSolutions :: IConnection c => c -> ServerPart Response
userSolutions conn = do
    uid <- lookRead "uid"
    current <- lookRead "current"
    perPage <- lookRead "perpage"
    result <- lift $ getUserSolutions uid (Pagination current perPage) conn
    ok $ toResponse $ JSON.encode result

userSolution :: IConnection c => ID -> ID -> c -> ServerPart Response
userSolution uid pid conn = do
    result <- lift $ getUserSolution uid pid conn
    (if (result == Nothing) then notFound else ok) $ toResponse $ toJObj "problem" result
