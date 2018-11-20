module Main where

import Args (parseArgs)
import Youtube

import Control.Exception
import Control.Monad
import Data.Tree
import System.Environment (getArgs)
import System.Exit

type VideoTree = Tree VideoSnippet

data DepthParam = DepthParam {videoID :: String, depth :: Int} deriving (Show, Eq) 

treeGrowth :: Bool -> String -> Int -> DepthParam -> IO (VideoSnippet, [DepthParam])
treeGrowth verbose apiKey maxDepth dp = do
    let vid = videoID dp
    let dep = depth dp

    when verbose $ putStrLn "[Verbose] Performing video.list request on " ++ vid
    vidSnippet <- buildVideoListRequest vid apiKey >>= performJSONRequest

    if dep == maxDept then do
        when verbose $ putStrLn "[Verbose] Stopping growth at max depth."
        return (vidSnippet, [])
    else do
        when verbose $ putStrLn "[Verbose] Performing search.list request on " ++ vid
        


main :: IO ()
main = do
    (flags, (beginId, apiKey)) <- catch (getArgs >>= parseArgs) $ \e -> do
                        let err = show (e :: IOException)
                        putStrLn err
                        exitWith $ ExitFailure 1
    
    putStrLn $ "Starting with " ++ beginId
    exitWith ExitSuccess