module Route.Compile where

import Text.JSON as JSON (encode)
import Happstack.Server
import System.Process (readCreateProcessWithExitCode, proc, shell, CreateProcess(..))
--import Data.Unique
import Control.Monad.Trans (lift)
import Control.Monad (forM_, msum)
--import Control.Exception (catch)
import System.Directory (getCurrentDirectory, createDirectoryIfMissing, getDirectoryContents)
import System.Exit (ExitCode(..))
import DB (IConnection, ID)
import DB.Problem (newUserSolution)
import System.FilePath (replaceExtension)
import Data.Text.Lazy as T (Text, pack, unpack)
import Data.Text.Lazy.IO as T (readFile, writeFile)
import Data.List (isSuffixOf)
import Route.Internal (ConnServer)

import Helper

type Error         = String
type DirectoryPath = String

checkAnswer :: ID -> ConnServer Response
checkAnswer uid conn = do
    -- read query params
    (filePath,_,_) <- lookFile "file"
    pid <- look "problem"
    language <- look "language"
    
    -- set paths
    wd <- lift getCurrentDirectory
    let binPath = wd ++ "/bin/" ++ language ++ "/"
    let problemPath = wd ++ "/bin/plain/q" ++ pid ++ "/"
    let compilePath = wd ++ "/tmp/q" ++ pid ++ "/user" ++ show uid ++ "/"
    
    result <- lift $ fmap msum $ sequence [
              compile pid filePath binPath problemPath compilePath
            , runForEachInput problemPath compilePath
        ]
        
    case result of
        Nothing  -> do
            lift $ newUserSolution uid (read pid) conn -- user solved solution
            ok $ toResponse "Correct" 
        Just err -> do
            badRequest $ toResponse err
        

-- | Calls the "compile" binary under the language directory with the user code path, compilation path, and the problem id
compile :: String -> FilePath -> DirectoryPath -> DirectoryPath -> DirectoryPath -> IO (Maybe Error)
compile pid filePath binPath problemPath compilePath = do
    createDirectoryIfMissing True compilePath
    createDirectoryIfMissing True problemPath
    (exitCode, out, error) <- readCreateProcessWithExitCode 
        (proc (binPath ++ "compile") 
            [filePath,compilePath,pid]) { cwd = Just binPath }
        ""
    if (exitCode == ExitSuccess) 
    then return $ Nothing
    else return $ Just $ dropFirstLine error
    where
        dropFirstLine = unlines.drop 2.lines

-- | Runs the "main" binary in the compilation path, with the given input and return the output
run :: DirectoryPath -> Text -> IO (Either Error Text)
run compilePath input = do
    (exitCode, out, error) <- readCreateProcessWithExitCode 
        (shell $ "timeout -s KILL 2s " ++ compilePath ++ "main")
        (T.unpack input)
    case exitCode of
        ExitSuccess      -> return $ Right $ T.pack out
        ExitFailure (-9) -> return $ Left    "Program times out."
        otherwise        -> return $ Left  $ error

-- | Runs the "main" binary in the compilation path with each input in the problem path, 
--   and check the output against the expecation.
runForEachInput :: DirectoryPath -> DirectoryPath -> IO (Maybe Error)
runForEachInput problemPath compilePath = do
    pairs <- inOutFiles problemPath
    fmap msum $ mapM runCompare pairs
    where
        runCompare :: (FilePath, FilePath) -> IO (Maybe Error)
        runCompare (inf,expectf) = do
            input  <- T.readFile inf
            expect <- T.readFile expectf
            actual <- run compilePath input
            return $ case actual of
                Left error -> Just error
                Right out -> if (expect == out) then Nothing else Just $ T.unpack input

-- | Gets a list of (input, output) files under the path
inOutFiles :: DirectoryPath -> IO [(FilePath, FilePath)]
inOutFiles path = do
    inputs <- searchFileExt path ".in"
    let outputs = map (flip replaceExtension ".out") inputs
    return $ zip inputs outputs
    
searchFileExt :: String -> DirectoryPath -> IO [FilePath]
searchFileExt path ext = do
    list <- getDirectoryContents path
    return $ map (path ++) $ filter (ext `isSuffixOf`) list

