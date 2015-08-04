import System.Environment (getArgs)
import Data.Text.Lazy as Text (splitOn, pack, append)
import Data.Text.Lazy.IO as TextIO (readFile, writeFile)
import Control.Monad (forM_)

splitUserAnswerTo :: FilePath -> FilePath -> IO ()
splitUserAnswerTo destination filepath = do
    content <- TextIO.readFile filepath
    forM_ (zip [1..] $ tail $ Text.splitOn (pack "OUTPUT") content) (\(num, content) -> do
        TextIO.writeFile (destination ++ "/" ++ show num ++ ".out") $ (pack "OUTPUT") `append` content)
    
main :: IO ()
main = do
    args <- getArgs
    let [dest,file] = args
    splitUserAnswerTo dest file

