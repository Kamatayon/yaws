{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Unsplash where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Aeson.KeyMap as HML
import Data.ByteString hiding (filter)
import qualified Data.ByteString.Char8 as C
import Data.Functor
import GHC.Generics
import Network.HTTP.Client (Request, setQueryString)
import Network.HTTP.Conduit
import Network.HTTP.Simple (getResponseBody, httpJSON, setRequestHeader)
import Types

data UnsplashPhoto = UnsplashPhoto
  { upWidth :: Int,
    upHeight :: Int,
    upUrl :: String,
    upId :: String,
    upHtmlLink :: String
  }
  deriving (Show)

instance FromJSON UnsplashPhoto where
  parseJSON = withObject "UnsplashPhoto" $ \obj -> do
    width <- obj .: "width"
    height <- obj .: "height"
    id <- obj .: "id"
    urls <- obj .: "urls"
    url <- urls .: "raw"
    links <- obj .: "links"
    htmlLink <- links .: "html"
    pure $ UnsplashPhoto {upWidth = width, upHeight = height, upUrl = url, upId = id, upHtmlLink = htmlLink}

-- instance FromJSON UnsplashErrorResponse where
--   parseJSON = withObject "UnsplashErrorResponse" $ \obj -> obj .: "errors" <&> UnsplashErrorResponse

data UnsplashResponse
  = UnsplashResponse
      { urPhotos :: [UnsplashPhoto]
      }
  | UnsplashErrorResponse [String]
  deriving (Generic)

instance FromJSON UnsplashResponse where
  parseJSON a@(Array arr) = UnsplashResponse <$> parseJSON a
  parseJSON (Object obj) = UnsplashErrorResponse <$> obj .: "errors"
  parseJSON _ = fail "Unable to parse Unsplash response"

orientationToString :: Orientation -> String
orientationToString Landscape = "landscape"
orientationToString Portrait = "portrait"
orientationToString Squarish = "squarish"

apiKey :: ByteString
apiKey = "2169c4a98576908d5a2b1518a069d9c75d6bf9a06837350b4b5a141ca03e95b0"

makeRequest :: UnsplashSettings -> Request
makeRequest settings =
  let collections = usCollections settings
      topics = usTopics settings
      query = usQuery settings
      mkQueryParam k v = [(C.pack k, Just $ C.pack v)]
      filterParam = case (query, topics, collections) of
        (Just q, _, _) -> mkQueryParam "query" q
        (_, Just t, _) -> mkQueryParam "topics" t
        (_, _, Just c) -> mkQueryParam "collections" c
        _ -> []
      or = usOrientation settings
      orientationParam = case or of
        Nothing -> []
        Just o -> mkQueryParam "orientation" $ orientationToString o
      countParam = mkQueryParam "count" "30"
      queryParams = filterParam <> orientationParam <> countParam
      basicReq = "https://api.unsplash.com/photos/random" :: Request
      setAuthHeader = setRequestHeader "Authorization" [C.concat ["Client-ID ", apiKey]]
   in setAuthHeader $ setQueryString queryParams basicReq

photoToImage :: UnsplashPhoto -> Image
photoToImage uph = Image {imageId = upId uph, imageRawUrl = upUrl uph, imageFullUrl = upHtmlLink uph}

handleUnsplashError :: (MonadThrow m) => [String] -> m a
handleUnsplashError strs = throwM $ case strs of
  ["No photos found."] -> NoImagesMatching
  l -> UnsplashErrors l

getImages :: (MonadIO m, MonadThrow m) => UnsplashSettings -> (Int, Int) -> m [Image]
getImages settings (w, h) = do
  let req = makeRequest settings
  response <- responseBody <$> httpJSON req
  case response of
    UnsplashErrorResponse errors -> handleUnsplashError errors
    UnsplashResponse photos -> pure $ photoToImage <$> filter predicate photos
  where
    -- let images = photoToImage <$> Prelude.filter predicate photos
    -- pure images

    predicate uPh = upWidth uPh >= w && upHeight uPh >= h
