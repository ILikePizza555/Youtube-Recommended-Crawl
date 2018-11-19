module Main where

import Args (parseArgs)
import Youtube

import Control.Exception
import Data.Tree
import System.Environment (getArgs)
import System.Exit

type VideoTree = Tree VideoSnippet

main :: IO ()
main = do
    (flags, begin_id) <- catch (getArgs >>= parseArgs) $ \e -> do
                        let err = show (e :: IOException)
                        putStrLn err
                        exitWith $ ExitFailure 1
    
    putStrLn $ "Starting with " ++ begin_id
    exitWith ExitSuccess