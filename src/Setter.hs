{-# LANGUAGE OverloadedStrings #-}

module Setter where

-- import Types (Photo (photoUrl, photoId), SourceOptions (W, R))

-- import Data.Text (Text)
import Data.Aeson.Types (parse)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath
import System.Process (readProcess)
import Types (Image (..), Reddit (..), Source (..))

-- import Data.List.Split

getRootDir :: IO FilePath
getRootDir = getXdgDirectory XdgData "yaws"

-- getPhotoPath :: Photo -> IO FilePath
sourceDir :: Source -> FilePath -> FilePath
sourceDir src rootPath =
  let suff (R r) = sourceDirName src </> redditSubreddit r
      suff s = sourceDirName s
   in rootPath </> suff src

sourceDirName :: Source -> FilePath
sourceDirName a = case a of
  W wo -> "wallhaven"
  R ro -> "reddit"

saveImage :: Source -> Image -> IO FilePath
saveImage source photo = do
  rootDir <- getRootDir
  let sd = sourceDir source rootDir
  createDirectoryIfMissing True sd
  let url = imageRawUrl photo
  req <- parseRequest url
  resp <- httpBS req
  let img = getResponseBody resp
  let imgName = imageId photo ++ takeExtension url
  let imgPath = sd </> imgName
  BS.writeFile imgPath img
  return imgPath

setFeh :: Bool -> FilePath -> IO String
setFeh xinerama fp = readProcess "feh" args ""
  where
    bgFill = ["--bg-fill", fp]
    args =
      if xinerama
        then bgFill
        else bgFill ++ ["--no-xinerama"]
