{-# LANGUAGE OverloadedStrings #-}

module Setter where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson.Types (parse)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Maybe
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import Network.HTTP.Types
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, doesDirectoryExist, getXdgDirectory)
import System.FilePath
import System.Process (readProcess)
import Types (Image (..), Reddit (..), Settings (..), Source (..), YawsException (RootDirDoesNotExist, UnknownContentType), YawsIO)

-- | validates if provided root directory exists, if not throws exception otherwsie returns provided/default directory
getDir :: (MonadIO m, MonadThrow m) => Maybe FilePath -> m FilePath
getDir mb = liftIO $ do
  p <- case mb of
    Just _p -> pure _p
    Nothing -> do
      xd <- getXdgDirectory XdgData "yaws"
      createDirectoryIfMissing False xd
      pure xd
  exists <- doesDirectoryExist p
  if exists
    then pure p
    else throwM $ RootDirDoesNotExist p

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

getImageFormat :: Response a -> Maybe BS.ByteString
getImageFormat resp =
  let pred = (== hContentType) . fst
      headers = responseHeaders resp
      mbHeader = find pred headers
      getHeaderValue = BS.unpack . snd
      parseFormat = BSC.stripPrefix "image/"
   in mbHeader >>= parseFormat . snd

saveImage :: Image -> FilePath -> YawsIO FilePath
saveImage image dirPath = do
  source <- asks sSource
  let sd = sourceDir source dirPath
  liftIO $ createDirectoryIfMissing True sd
  let url = imageRawUrl image
  req <- parseRequest url
  resp <- httpBS req
  imgFormat <- case getImageFormat resp of
    Nothing -> throwM UnknownContentType
    Just x -> pure $ BSC.unpack x

  let img = getResponseBody resp
  let imgName = imageId image ++ "." ++ imgFormat
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
