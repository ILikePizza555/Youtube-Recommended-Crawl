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
    (flags, (beginId, apiKey) <- catch (getArgs >>= parseArgs) $ \e -> do
                        let err = show (e :: IOException)
                        putStrLn err
                        exitWith $ ExitFailure 1
    
    putStrLn $ "Starting with " ++ beginId
    exitWith ExitSuccess