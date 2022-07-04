{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Unsplash where

import Data.Aeson
import Data.ByteString
import qualified Data.ByteString.Char8 as C
import GHC.Generics
import Network.HTTP.Client (Request, setQueryString)
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

newtype UnsplashResponse = UnsplashResponse
  { urPhotos :: [UnsplashPhoto]
  }
  deriving (Generic)

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

getImages :: UnsplashSettings -> (Int, Int) -> IO [Image]
getImages settings (w, h) = do
  let req = makeRequest settings
  resp <- httpJSON req
  let photos = getResponseBody resp
  let images = photoToImage <$> Prelude.filter predicate photos
  pure images
  where
    predicate uPh = upWidth uPh >= w && upHeight uPh >= h
