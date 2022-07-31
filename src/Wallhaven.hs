-- {-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wallhaven where

-- import Data.Aeson.Li

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import Data.List.NonEmpty (fromList)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Network.HTTP.Client (Response (responseBody), setQueryString)
import Network.HTTP.Simple
import Types

newtype WallhavenBody = WallhavenBody {images :: [Image]} deriving (Show)

instance FromJSON WallhavenBody where
  parseJSON = withObject "WallhavenBody" $ \obj -> do
    d <- obj .: "data"
    ar <- parseJSON d
    pure $ WallhavenBody ar

instance FromJSON Image where
  parseJSON = withObject "Image" $ \obj ->
    Image
      <$> obj .: "id"
      <*> obj .: "path"
      <*> obj .: "url"

makeRequest :: Wallhaven -> (Int, Int) -> Request
makeRequest options (w, h) = setQueryString query apiUrl
  where
    apiUrl = "https://wallhaven.cc/api/v1/search"
    atLeast = BSC.pack $ concat [show w, "x", show h]
    tags = BSC.pack $ wallhavenTags options
    query =
      [ ("atleast", Just atLeast),
        ("sorting", Just "random"),
        ("q", Just tags)
      ]

getPhotos :: MonadIO m => Wallhaven -> (Int, Int) -> m [Image]
getPhotos options (w, h) = do
  let request = makeRequest options (w, h)
  body <- responseBody <$> httpJSON request
  pure $ images body
