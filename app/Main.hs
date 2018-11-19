module Main where

import Args (parseArgs)
import Youtube
import Data.Tree
import System.Environment (getArgs)
import System.Exit

type VideoTree = Tree VideoSnippet

main :: IO ()
main = do
    args <- getArgs >>= parseArgs
    exitWith ExitSuccess