{-# LANGUAGE OverloadedStrings #-}

module Youtube where

import           Control.Monad
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Aeson                 as JSON
import           Data.Aeson.Types           (parse)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Maybe
import           Data.Time
import           Data.Text                  (Text)
import           Data.HashMap.Strict        as HM
import qualified Data.Vector                as V
import           Network.HTTP.Simple

data VideoSnippet = VideoSnippet {
    id          :: String,
    published   :: UTCTime, 
    title       :: String, 
    description :: String, 
    channelID   :: String, 
    channelName :: String, 
    tags        :: [String]
    } deriving Show

instance JSON.FromJSON VideoSnippet where
    -- Parses object found in "items" array of video.list 
    parseJSON = withObject "VideoSnippet" $ \obj -> do
        id_     <- obj .: "id"
        
        sObj    <- obj .: "snippet"

        pub_    <- sObj .: "publishedAt"
        title_  <- sObj .: "title"
        desc_   <- sObj .: "description"
        cID_    <- sObj .: "channelId"
        cName_  <- sObj .: "channelTitle"
        tags_   <- sObj .:? "tags" .!= []
        return $ VideoSnippet id_ pub_ title_ desc_ cID_ cName_ tags_

-- Parses the videoId object into a string
-- E.g { "id": {"videoID": "desiredString" } }
parseVideoIdObj :: JSON.Value -> Parser Text
parseVideoIdObj = withObject "wrapper object" $ \obj -> do
    idObj <- obj .: "id"
    return $ idObj .: "videoId"

-- Parses a list of videoId objects
parseVideoIdList :: JSON.Value -> Parser [Text]
parseVideoIdList = withArray "array" $ \arr -> mapM parseVideoIdObj (V.toList arr)

-- Parses the JSON recieved from a search.list API call
parseSearchListJSON :: JSON.Value -> [Text]
parseSearchListJSON v = case parse parser v of
                            Error s -> []
                            Success a -> a
                        where parser = withObject "root object" $ \obj -> parseVideoIdList >>= (obj .: "items")

-- Takes an optional Int, a video id and an API key to return a URL for the Search.list API call
buildSearchListURL :: Maybe Int -> String -> String -> String
buildSearchListURL Nothing vID key = "https://www.googleapis.com/youtube/v3/search?part=snippet&fields=items%2Fid%2FvideoId&relatedToVideoId=" ++ vID ++ "&type=video&key=" ++ key
buildSearchListURL (Just maxCount) vID key = "https://www.googleapis.com/youtube/v3/search?part=snippet&fields=items%2Fid%2FvideoId&relatedToVideoId=" ++ vID ++ "&maxResults=" ++ show maxCount ++ "&type=video&key=" ++ key

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
performJSONRequest :: (MonadIO io, FromJSON json) => Request -> io json
performJSONRequest req = getResponseBody <$> httpJSON req