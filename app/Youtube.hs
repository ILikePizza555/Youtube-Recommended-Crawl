{-# LANGUAGE OverloadedStrings #-}

module Youtube where

import           Control.Monad.IO.Class
import           Control.Monad.Catch.MonadThrow
-- "qualified" imports into a namespace
import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

-- Takes an optional Int, a video id and an API key to return a URL for the Search.list API call
buildListURL :: Maybe Int -> String -> String -> String
buildListURL Nothing vID key = "https://www.googleapis.com/youtube/v3/search?part=snippet&relatedToVideoId=" ++ vID ++ "&type=video&key=" ++ key
buildListURL (Just maxCount) vID key = "https://www.googleapis.com/youtube/v3/search?part=snippet&relatedToVideoId=" ++ vID ++ "&maxResults=" ++ show maxCount ++ "&type=video&key=" ++ key

-- Same as buildListURL but returns a Request
buildListRequest :: MonadThrow m => Maybe Int -> String -> String -> m Request
buildListRequest a b c = parseRequest $ buildListURL a b c

-- Performs a request, and assumes the response will be JSON
performJSONRequest :: (MonadIO io) => Request -> io JSON.Value
performJSONRequest req = getResponseBody <$> httpJSON req