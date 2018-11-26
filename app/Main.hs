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

-- Performs a video.list api request, and returns the parsed result. If an error occured, it was printed to the console and Nothing was returned.
maybeSnippet :: String -> String -> IO (Maybe VideoSnippet)
maybeSnippet vid apiKey = do
    netResp <- buildVideoListRequest vid apiKey >>= performJSONRequest
    parseResp <- parseVideoListResponse <$> netResp :: IO (Either String [VideoSnippet])

    case parseResp of
        Left errStr -> do
            hPutStrLn stderr errStr
            return Nothing
        Right resp ->
            if null resp then do
                hPutStrLn stderr "Recieved empty response from Youtube."
                return Nothing
            else return $ Just (head resp)

treeGrowth :: Bool -> String -> Int -> DepthParam -> IO (Maybe VideoSnippet, [DepthParam])
treeGrowth verbose apiKey maxDepth dp = do
    let vid = videoID dp
    let dep = depth dp

    when verbose (putStrLn $ "[Verbose] Performing video.list request on " ++ vid)
    case maybeSnippet vid apiKey of
        Nothing -> return (Nothing, [])
        Just vidSnippet -> do
            -- Check if we've reached maxDepth, if we have, return no seed values, otherwise continue to grow.
            if dep == maxDepth then do
                when verbose (putStrLn "[Verbose] Stopping growth at max depth.")
                return (vidSnippet, [])
            else do
                when verbose (putStrLn $ "[Verbose] Performing search.list request on " ++ vid)
                case fmap parseSearchListResponse $ buildSearchListRequest (Just 10) vid apiKey >>= performJSONRequest of
                    Left errStr -> do
                        when verbose (putStrLn $ "[Verbose] Stopping growth because of error: " ++ errStr)
                        return (vidSnippet, [])
                    Right relatedL -> do
                        when verbose (putStrLn $ "[Verbose] Got " ++ show relatedL)
                        return (vidSnippet, map (flip DepthParam (dep + 1) . show) relatedL)


main :: IO ()
main = do
    (flags, (beginId, apiKey)) <- catch (getArgs >>= parseArgs) $ \e -> do
                        let err = show (e :: IOException)
                        hPutStrLn stderr err
                        exitWith $ ExitFailure 1
    
    putStrLn $ "Starting with " ++ beginId
    exitWith ExitSuccess