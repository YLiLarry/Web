module RunHaskell where

import Happstack.Server
import System.Process
import Data.Unique
import Control.Monad.Trans
import System.Directory
import System.Exit

import Helper

runHaskell :: ServerPart Response
runHaskell = do
    (filepath,filename,_) <- lookFile "hsfile"
    question <- look "question"
    wd <- lift getCurrentDirectory
    (exitCode, out, error) <- lift $ readCreateProcessWithExitCode (proc (wd ++ "/bin/compile") [question,filepath]) { cwd = Just (wd ++ "/bin/") } ""
    if (exitCode /= ExitSuccess) 
        then badRequest $ toResponse $ "Compilation failure:\n" ++ error ++ "\n" 
        else ok $ toResponse $ "stdout:\n" ++ out ++ "\nstderr:\n" ++ error ++ "\n" 
    --output <- lift $ runCode question path
    --ok $ toResponse output
    
--runCode :: String -> FilePath -> IO String
--runCode question filepath = do
    --suffix <- newUnique
    --newPath <- return $ filepath ++ suffix
    --renameFile filepath newPath
    