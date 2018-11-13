{-# LANGUAGE OverloadedStrings #-}

module Youtube where

import           Control.Monad
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.IO.Class     (MonadIO)
import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

-- Takes an optional Int, a video id and an API key to return a URL for the Search.list API call
buildSearchListURL :: Maybe Int -> String -> String -> String
buildSearchListURL Nothing vID key = "https://www.googleapis.com/youtube/v3/search?part=snippet&relatedToVideoId=" ++ vID ++ "&type=video&key=" ++ key
buildSearchListURL (Just maxCount) vID key = "https://www.googleapis.com/youtube/v3/search?part=snippet&relatedToVideoId=" ++ vID ++ "&maxResults=" ++ show maxCount ++ "&type=video&key=" ++ key

-- Takes a video id and an api key to return a URL for the Video.list API call
buildVideoListURL :: String -> String -> String
buildVideoListURL vID key = "https://www.googleapis.com/youtube/v3/videos?part=snippet&id=" ++ vID ++ "&key=" ++ key

-- Same as buildSearchListURL but returns a Request
buildSearchListRequest :: MonadThrow m => Maybe Int -> String -> String -> m Request
buildSearchListRequest a b c = parseRequest $ buildSearchListURL a b c

-- Same as buildVideoListURL but returns a Request
buildVideoListRequest :: MonadThrow m => String -> String -> m Request
buildVideoListRequest a b = parseRequest $ buildVideoListURL a b

-- Performs a request, and assumes the response will be JSON
performJSONRequest :: (MonadIO io) => Request -> io JSON.Value
performJSONRequest req = getResponseBody <$> httpJSON req