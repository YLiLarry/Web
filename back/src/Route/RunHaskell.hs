module Route.RunHaskell where

import Text.JSON as JSON (encode)
import Happstack.Server
import System.Process (readCreateProcessWithExitCode, proc, CreateProcess(..))
--import Data.Unique
import Control.Monad.Trans (lift)
import Control.Monad (forM_)
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import Data.Text.Lazy as Text (splitOn, pack)
import Data.Text.Lazy.IO as TextIO (readFile, writeFile)
--import Data.Text.Lazy.IO as Text

import Helper

runHaskell :: ServerPart Response
runHaskell = do
    (filepath,filename,_) <- lookFile "hsfile"
    question <- look "question"
    uid <- look "uid"
    language <- look "language"
    wd <- lift getCurrentDirectory
    let binPath = wd ++ "/bin/" ++ language
    let questionPath = wd ++ "/bin/plain/q" ++ question
    let compilePath = wd ++ "/tmp/q" ++ question ++ "/user" ++ uid
    lift $ createDirectoryIfMissing True compilePath
    lift $ splitUserAnswerTo compilePath filepath
    (exitCode, out, error) <- lift $ readCreateProcessWithExitCode 
        (proc (binPath ++ "/compile") 
              [filepath,questionPath,compilePath]) { cwd = Just binPath } 
        ""
    let result = JSON.encode [("stdout",out), ("stderr", error)]
    if (exitCode /= ExitSuccess) 
        then badRequest $ toResponse result
        else ok $ toResponse result

splitUserAnswerTo :: FilePath -> FilePath -> IO ()
splitUserAnswerTo destination filepath = do
    content <- TextIO.readFile filepath
    forM_ (zip [1..] $ tail $ Text.splitOn (pack "OUTPUT") content) (\(num,content) -> do
        TextIO.writeFile (destination ++ "/" ++ show num ++ ".out") content)
    
