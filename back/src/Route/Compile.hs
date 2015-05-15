module Route.Compile where

import Text.JSON as JSON (encode)
import Happstack.Server
import System.Process (readCreateProcessWithExitCode, proc, CreateProcess(..))
--import Data.Unique
import Control.Monad.Trans (lift)
import Control.Monad (forM_)
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import Data.Text.Lazy as Text (splitOn, pack, append)
import Data.Text.Lazy.IO as TextIO (readFile, writeFile)
import DB (newUserSolution, IConnection, ID)

import Helper

compile :: IConnection c => ID -> c -> ServerPart Response
compile uid conn = do
    (filepath,filename,_) <- lookFile "file"
    pid <- look "problem"
    language <- look "language"
    wd <- lift getCurrentDirectory
    let binPath = wd ++ "/bin/" ++ language
    let problemPath = wd ++ "/bin/plain/q" ++ pid
    let compilePath = wd ++ "/tmp/q" ++ pid ++ "/user" ++ show uid
    lift $ createDirectoryIfMissing True compilePath
    lift $ splitUserAnswerTo compilePath filepath
    (exitCode, out, error) <- lift $ readCreateProcessWithExitCode 
        (proc (binPath ++ "/compile") 
              [filepath,problemPath,compilePath]) { cwd = Just binPath } 
        ""
    let result = JSON.encode [("stdout",out), ("stderr", error)]
    if (exitCode /= ExitSuccess) 
    then badRequest $ toResponse result
    else do
        lift $ newUserSolution uid (read pid) conn
        ok $ toResponse result

splitUserAnswerTo :: FilePath -> FilePath -> IO ()
splitUserAnswerTo destination filepath = do
    content <- TextIO.readFile filepath
    forM_ (zip [1..] $ tail $ Text.splitOn (pack "OUTPUT") content) (\(num, content) -> do
        TextIO.writeFile (destination ++ "/" ++ show num ++ ".out") $ (pack "OUTPUT") `append` content)
    
