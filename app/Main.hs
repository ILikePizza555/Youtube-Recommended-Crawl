module Main where

import Args (parseArgs)
import Youtube

import Control.Exception
import Control.Monad
import Data.Tree
import System.Environment (getArgs)
import System.Exit
import System.IO

type VideoTree = Tree VideoSnippet

data DepthParam = DepthParam {videoID :: String, depth :: Int} deriving (Show, Eq) 

-- Prints an error message to stderr and exits the program
errExitMessage :: String -> IO ()
errExitMessage s = do
    hPutStrLn stderr s
    exitWith $ ExitFailure 1

treeGrowth :: Bool -> String -> Int -> DepthParam -> IO (VideoSnippet, [DepthParam])
treeGrowth verbose apiKey maxDepth dp = do
    let vid = videoID dp
    let dep = depth dp

    -- Perform the video.list api request and parse the response. Quit the program on parse failure or no response.
    when verbose (putStrLn $ "[Verbose] Performing video.list request on " ++ vid)
    vidSnippet <- case parseVideoListResponse <$> (buildVideoListRequest vid apiKey >>= performJSONRequest) of
                        Left errStr -> errExitMessage errStr
                        Right res -> if null res then errExitMessage "Recieved empty response from Youtube." else head res

    -- Check if we've reached maxDepth, if we have, return no seed values.
    if dep == maxDepth then do
        when verbose (putStrLn "[Verbose] Stopping growth at max depth.")
        return (vidSnippet, [])
    else do
        when verbose (putStrLn $ "[Verbose] Performing search.list request on " ++ vid)
        relatedL <- fmap parseSearchListResponse $ buildSearchListRequest (Just 10) vid apiKey >>= performJSONRequest
        when verbose (putStrLn $ "[Verbose] Got " ++ show relatedL)

        return (vidSnippet, map (flip DepthParam (dep + 1) . show) relatedL)

main :: IO ()
main = do
    (flags, (beginId, apiKey)) <- catch (getArgs >>= parseArgs) $ \e -> do
                        let err = show (e :: IOException)
                        errExitMessage err
    
    putStrLn $ "Starting with " ++ beginId
    exitWith ExitSuccess