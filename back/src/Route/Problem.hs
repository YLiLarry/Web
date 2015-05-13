module Route.Problem where

import Happstack.Server
import Text.JSON as JSON (encode)
import DB
import Control.Monad.Trans (lift)
import Route.Internal (toJObjs)

problemCollection :: IConnection c => c -> ServerPart Response
problemCollection conn = do
    current <- lookRead "current"
    perPage <- lookRead "perpage"
    result <- lift $ getAllProblems (Pagination current perPage) conn
    ok $ toResponse $ toJObjs "problems" result

problemElement :: ServerPart Response
problemElement = do
    ok $ toResponse $ JSON.encode "test2"

userSolutions :: IConnection c => c -> ServerPart Response
userSolutions conn = do
    uid <- lookRead "uid"
    current <- lookRead "current"
    perPage <- lookRead "perpage"
    result <- lift $ getUserSolutions uid (Pagination current perPage) conn
    ok $ toResponse $ JSON.encode result
    

userSolution :: IConnection c => c -> ServerPart Response
userSolution conn = do
    uid <- lookRead "uid"
    pid <- lookRead "pid"
    result <- lift $ getUserSolution uid pid conn
    ok $ toResponse $ JSON.encode result
    
