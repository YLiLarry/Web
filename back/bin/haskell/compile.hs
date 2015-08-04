-- | This file compiles the user uploaded code to the given directory

{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs, getExecutablePath)
import Data.Text.Lazy as T (unlines, pack)
import Data.Text.Lazy.IO as T (readFile, writeFile)

main :: IO ()
main = do
    args <- getArgs
    let [filePath, compilePath, problemID] = args
    let mainPath = "./q" ++ problemID ++ ".hs"
    execPath <- getExecutablePath
    
    content        <- T.readFile filePath
    mainFile       <- T.readFile mainPath
    let safeCode    = compilePath ++ "safeCode.hs"
    let safeContent = T.unlines [
                  T.pack $ "{- This file is automatically merged by " ++ execPath
                , T.pack $ "with user code in " ++ filePath
                , T.pack $ "with main code in " ++ mainPath
                , "-}"
                , "-----------------------"
                , safeHead
                , "-----------------------"
                , content
                , "-----------------------"
                , mainFile
            ]
            
    T.writeFile safeCode safeContent 
    
    where
        safeHead = "{-# LANGUAGE Safe #-}"
