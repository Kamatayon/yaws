-- {-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Wallhaven where

import Types
import Data.Aeson
import Data.Aeson.KeyMap
-- import Data.Aeson.Li
import Data.List.NonEmpty (fromList)
import Network.HTTP.Client (setQueryString)
import Network.HTTP.Simple
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Char8 as Bytechar
import Data.Text.Encoding as E
import qualified Data.Text as T


newtype WallhavenBody = WallhavenBody {images :: [Image]} deriving Show

instance FromJSON WallhavenBody where
    parseJSON = withObject "WallhavenBody" $ \obj -> do
        d <- obj .: "data"
        ar <- parseJSON d
        pure $ WallhavenBody ar

instance FromJSON Image where
    parseJSON = withObject "Image" $ \obj -> Image
        <$> obj .: "id"
        <*> obj .: "path"
        <*> obj .: "url"
makeRequest :: Wallhaven -> (Int, Int) -> Request
makeRequest options (w, h) = setQueryString query apiUrl 
    where
        apiUrl = "https://wallhaven.cc/api/v1/search"
        atLeast = BSU.fromString $ concat [show w, "x", show h]
        tags = BSU.fromString $ wallhavenTags options
        query = [("atleast", Just atLeast)
                ,("sorting", Just "random")
                ,("q", Just tags)]

getPhotos :: Wallhaven -> (Int, Int) -> IO [Image]
getPhotos options (w, h) = do
    let request = makeRequest options (w, h)
    -- resp <- httpJSONEither request
    resp <- httpJSON request
    let body = getResponseBody resp
    pure $ images body


