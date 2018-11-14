{-# LANGUAGE OverloadedStrings #-}

module Youtube where

import           Control.Monad
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Aeson                 as JSON
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Maybe
import           Data.Time
import           Data.HashMap.Strict
import           Network.HTTP.Simple

data VideoSnippet = VideoSnippet {
    id          :: String
    published   :: UTCTime, 
    title       :: String, 
    description :: String, 
    channelID   :: String, 
    channelName :: String, 
    tags        :: [String]
    } deriving Show

instance JSON.FromJSON VideoSnippet where
    -- parseJSON :: Value -> Parser a
    -- withObject :: String -> (Object -> Parser a) -> Value -> Parser a
    parseJSON = withObject "VideoSnippet" $ \obj -> do
        id_ <- case lookup "id" obj of
            Just String sv -> sv
            Just o -> o .: "videoId"
            Nothing -> fail "No field 'id'"
        
        pub_    <- obj .: "publishedAt"
        title_  <- obj .: "title"
        desc_   <- obj .: "description"
        cID_    <- obj .: "channelId"
        cName_  <- obj .: "channelTitle"
        tags_   <- obj .:? "tags" .!= []
        return VideoSnippet id_ pub_ title_ desc_ cID_ cName_ tags_

-- Takes an optional Int, a video id and an API key to return a URL for the Search.list API call
buildSearchListURL :: Maybe Int -> String -> String -> String
buildSearchListURL Nothing vID key = "https://www.googleapis.com/youtube/v3/search?part=snippet&fields=items(id%2Csnippet(channelId%2CchannelTitle%2Cdescription%2CpublishedAt%2Ctitle))&relatedToVideoId=" ++ vID ++ "&type=video&key=" ++ key
buildSearchListURL (Just maxCount) vID key = "https://www.googleapis.com/youtube/v3/search?part=snippet&fields=items(id%2Csnippet(channelId%2CchannelTitle%2Cdescription%2CpublishedAt%2Ctitle))&relatedToVideoId=" ++ vID ++ "&maxResults=" ++ show maxCount ++ "&type=video&key=" ++ key

-- Takes a video id and an api key to return a URL for the Video.list API call
buildVideoListURL :: String -> String -> String
buildVideoListURL vID key = "https://www.googleapis.com/youtube/v3/videos?part=snippet&fields=items(id%2Csnippet(channelId%2CchannelTitle%2Cdescription%2CpublishedAt%2Ctags%2Ctitle))&id=" ++ vID ++ "&key=" ++ key

-- Same as buildSearchListURL but returns a Request
buildSearchListRequest :: MonadThrow m => Maybe Int -> String -> String -> m Request
buildSearchListRequest a b c = parseRequest $ buildSearchListURL a b c

-- Same as buildVideoListURL but returns a Request
buildVideoListRequest :: MonadThrow m => String -> String -> m Request
buildVideoListRequest a b = parseRequest $ buildVideoListURL a b

-- Performs a request, and assumes the response will be JSON
performJSONRequest :: (MonadIO io) => Request -> io JSON.Value
performJSONRequest req = getResponseBody <$> httpJSON req