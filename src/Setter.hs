{-# LANGUAGE OverloadedStrings #-}

module Setter where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson.Types (parse)
import qualified Data.ByteString as BS
import Data.Char
import qualified Data.Text as T
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath
import System.Process (readProcess)
import Types (Image (..), Reddit (..), Settings (..), Source (..), YawsIO)

getRootDir :: IO FilePath
getRootDir = getXdgDirectory XdgData "yaws"

sourceDir :: Source -> FilePath -> FilePath
sourceDir src rootPath =
  let sDir = toLower <$> srcToStr src
      suff (R r) = sDir </> redditSubreddit r
      suff s = sDir
   in rootPath </> suff src

srcToStr :: Source -> String
srcToStr (W _) = "Wallhaven"
srcToStr (R _) = "Reddit"
srcToStr (U _) = "Unsplash"

sourceDirName :: Source -> FilePath
sourceDirName = (toLower <$>) . srcToStr

saveImage :: Image -> YawsIO FilePath
saveImage image = do
  rootDir <- liftIO getRootDir
  source <- asks sSource
  let sd = sourceDir source rootDir
  liftIO $ createDirectoryIfMissing True sd
  let url = imageRawUrl image
  req <- parseRequest url
  resp <- httpBS req
  let img = getResponseBody resp
  let imgName = imageId image ++ takeExtension url
  let imgPath = sd </> imgName
  liftIO $ BS.writeFile imgPath img
  pure imgPath

setFeh :: Bool -> FilePath -> IO String
setFeh xinerama fp = readProcess "feh" args ""
  where
    bgFill = ["--bg-fill", fp]
    args =
      if xinerama
        then bgFill
        else bgFill ++ ["--no-xinerama"]
