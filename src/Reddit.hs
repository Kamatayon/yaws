{-# LANGUAGE OverloadedStrings #-}

module Reddit where

import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Client (Request, Response (responseBody), parseRequest_)
import Network.HTTP.Simple (addRequestHeader, httpJSON)
import Types

newtype RedditResponse = RedditResponse {redditResponsePosts :: [RedditPost]} deriving (Show)

instance FromJSON RedditResponse where
  parseJSON = withObject "RedditResponse" $ \obj -> do
    d <- obj .: "data"
    c <- d .: "children"
    RedditResponse <$> parseJSON c

data RedditPost = RedditPost {redditPostImages :: [RedditImage], redditPostUrl :: String} deriving (Show)

instance FromJSON RedditPost where
  parseJSON = withObject "RedditPost" $ \obj -> do
    d <- obj .: "data"
    preview <- d .:? "preview"
    case preview of
      Nothing -> pure $ RedditPost [] ""
      Just p -> RedditPost <$> p .: "images" <*> d .: "permalink"

-- i <- p .: "images"
-- RedditPost <$> parseJSON i

data RedditImage = RedditImage
  { redditImageX :: Int,
    redditImageY :: Int,
    redditImageUrl :: String,
    redditImageId :: String
  }
  deriving (Show)

instance FromJSON RedditImage where
  parseJSON = withObject "RedditImage" $ \obj -> do
    s <- obj .: "source"
    url <- T.unpack . unescapeHTML <$> s .: "url"
    x <- s .: "width"
    y <- s .: "height"
    id <- obj .: "id"
    pure $ RedditImage x y url id
    where
      unescapeHTML = T.replace "&amp;" "&"

-- instance FromJSON RedditImage where
--     fromJSON val = withObject "RedditImage"

makeRequest :: String -> Request
makeRequest subredditName =
  let headerName = "User-Agent"
      headerValue = "yaws"
      url = parseRequest_ $ "https://reddit.com/r/" ++ subredditName ++ "/hot.json"
   in addRequestHeader headerName headerValue url

getImages :: Reddit -> (Int, Int) -> YawsIO [Image]
getImages redditOptions (w, h) = do
  let subredditName = redditSubreddit redditOptions
  let request = makeRequest subredditName
  body <- responseBody <$> httpJSON request
  let images = collectImages $ redditResponsePosts body
  pure images
  where
    predicate rImg = redditImageX rImg >= w && redditImageY rImg >= h
    postToImages :: RedditPost -> [Image]
    postToImages rp =
      let redditImages = redditPostImages rp
          fullUrl = "https://reddit.com" ++ redditPostUrl rp
          convert ri = Image {imageId = redditImageId ri, imageRawUrl = redditImageUrl ri, imageFullUrl = fullUrl}
       in map convert redditImages
    collectImages :: [RedditPost] -> [Image]
    collectImages posts = concatMap postToImages posts
