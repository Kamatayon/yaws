{-# LANGUAGE FlexibleContexts #-}

module Settings where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Maybe (catMaybes)
import Graphics.X11
import Graphics.X11.Xinerama
import System.Directory
import System.Process
import Text.Regex.TDFA
import Types

getRootDir :: Maybe FilePath -> YawsIO FilePath
getRootDir rootDir = case rootDir of
  Nothing -> liftIO getDefaultRootDir
  Just s -> validateRootDir s
  where
    getDefaultRootDir = do
      dir <- getXdgDirectory XdgData "yaws"
      createDirectoryIfMissing True dir
      pure dir
    validateRootDir = pure

parseDimensions :: String -> Maybe (Int, Int)
parseDimensions = parseMatch . matchStr
  where
    regex = "([0-9]+)x([0-9]+)"
    matchStr :: String -> (String, String, String, [String])
    matchStr s = s =~ regex
    parseMatch m = case m of
      (_, _, _, [w, h]) -> pure (read w, read h)
      _ -> Nothing

getDimensions :: (MonadIO m, MonadReader Settings m) => m (Int, Int)
getDimensions = do
  mbStr <- asks sDimensions
  case mbStr of
    Nothing -> asks sXinerama >>= liftIO . getDefaultDimensions
    Just x -> pure x

getDefaultDimensions :: Bool -> IO (Int, Int)
getDefaultDimensions xinerama = do
  ds <- openDisplay ""
  screenInfo <- getScreenInfo ds
  let dimensions = (if xinerama then maxDimensions else dimensionsWithoutXinerama) screenInfo
  pure dimensions
  where
    -- gets minimum width/height from list of screens
    maxDimensions :: [Rectangle] -> (Int, Int)
    maxDimensions rects =
      let rectToTuple :: Rectangle -> (Int, Int)
          rectToTuple r = (fromIntegral $ rect_width r, fromIntegral $ rect_height r)
       in bimap maximum maximum $ unzip (rectToTuple <$> rects)
    dimensionsWithoutXinerama rects =
      let rectX r = fromIntegral (rect_width r) + fromIntegral (rect_x r)
          rectY r = fromIntegral (rect_height r) + fromIntegral (rect_y r)
          rectToTuple r = (rectX r, rectY r)
       in bimap maximum maximum $ unzip (rectToTuple <$> rects)
