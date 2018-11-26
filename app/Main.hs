module Main where

import Args (parseArgs)
import Youtube

import Control.Exception
import Control.Monad
import Data.Text          (Text)
import Data.Tree
import System.Environment (getArgs)
import System.Exit
import System.IO

type VideoTree = Tree VideoSnippet

data DepthParam = DepthParam {videoID :: String, depth :: Int} deriving (Show, Eq) 

-- Performs a video.list api request, and returns the parsed result. If an error occured, it was printed to the console and Nothing was returned.
maybeSnippet :: String -> String -> IO (Maybe VideoSnippet)
maybeSnippet vid api_key = do
    req <- buildVideoListRequest vid api_key
    parse_resp <- parseVideoListResponse <$> performJSONRequest req :: IO (Either String [VideoSnippet])

    case parse_resp of
        Left err_str -> do
            hPutStrLn stderr err_str
            return Nothing
        Right resp ->
            if null resp then do
                hPutStrLn stderr "Recieved empty response from Youtube."
                return Nothing
            else return $ Just (head resp)

-- Performs a search.list request, parses the value, and returns it as a list of string. If an error occured, it is returned as Left.
maybeSearchList :: Maybe Int -> String -> String -> IO (Either String [String])
maybeSearchList max_len vid api_key = do
    req <- buildSearchListRequest max_len vid api_key
    parse_resp <- parseSearchListResponse <$> performJSONRequest req :: IO (Either String [Text])

    return $ (fmap show) <$> parse_resp

treeGrowth :: Bool -> String -> Int -> DepthParam -> IO (Maybe VideoSnippet, [DepthParam])
treeGrowth verbose api_key max_depth dp = do
    let vid = videoID dp
    let dep = depth dp

    when verbose (putStrLn $ "[Verbose] Performing video.list request on " ++ vid)
    maybe_snippet <- maybeSnippet vid api_key
    
    case maybe_snippet of
        Nothing -> return (Nothing, [])
        Just vid_snippet -> do
            -- Check if we've reached max_depth, if we have, return no seed values, otherwise continue to grow.
            if dep == max_depth then do
                when verbose (putStrLn "[Verbose] Stopping growth at max depth.")
                return (Just vid_snippet, [])
            else do
                when verbose (putStrLn $ "[Verbose] Performing search.list request on " ++ vid)
                maybe_search_list <- maybeSearchList (Just 10) vid api_key

                case maybe_search_list of
                    Left err_str -> do
                        when verbose (putStrLn $ "[Verbose] Stopping growth because of error: " ++ err_str)
                        return (Just vid_snippet, [])
                    Right relatedL -> do
                        when verbose (putStrLn $ "[Verbose] Got " ++ show relatedL)
                        return (Just vid_snippet, map (flip DepthParam (dep + 1) . show) relatedL)


main :: IO ()
main = do
    (flags, (beginId, api_key)) <- catch (getArgs >>= parseArgs) $ \e -> do
                        let err = show (e :: IOException)
                        hPutStrLn stderr err
                        exitWith $ ExitFailure 1
    
    putStrLn $ "Starting with " ++ beginId
    exitWith ExitSuccess