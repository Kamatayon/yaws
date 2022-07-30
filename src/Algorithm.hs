{-# LANGUAGE FlexibleContexts #-}

module Algorithm where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Except
import qualified Reddit as R
import Setter (saveImage, setFeh, srcToStr)
import Settings (getDimensions)
import System.Exit
import System.Random (initStdGen, uniformR)
import Types (Image (..), Settings (..), Source (..), YawsException (NoImagesMatching), YawsIO)
import qualified Unsplash as U
import qualified Wallhaven as WH

getImages :: (MonadIO m, MonadReader Settings m, MonadThrow m) => m [Image]
getImages = do
  dimensions <- getDimensions
  source <- asks sSource
  case source of
    W wall -> WH.getPhotos wall dimensions
    R reddit -> R.getImages reddit dimensions
    U unsplash -> U.getImages unsplash dimensions

getImage :: (MonadIO m, MonadThrow m, MonadReader Settings m) => m Image
getImage = getImages >>= selectRandomImage
  where
    selectRandomImage [] = throwM NoImagesMatching
    selectRandomImage imgs = do
      stdGen <- initStdGen
      let (i, _) = uniformR (0, length imgs - 1) stdGen
      pure $ imgs !! i

setWallpaper :: YawsIO ()
setWallpaper = do
  image <- getImage
  source <- asks sSource
  msg $ "Selected image from " ++ srcToStr source ++ ". Url: " ++ imageFullUrl image
  imagePath <- saveImage image
  msg $ "Saved image to " ++ imagePath
  xinerama <- asks sXinerama
  set <- asks sSet
  if set
    then liftIO $ void $ setFeh xinerama imagePath
    else liftIO $ putStrLn imagePath
  pure ()
  where
    urlMsg img src = putStrLn $ "Selected image from " ++ srcToStr src ++ ". Url: " ++ imageFullUrl img
    handleYawsError ye = putStrLn ("Error occured during getting the image: " ++ show ye) >> exitFailure

-- | only prints message to stdout if sSet set to True.
msg :: (MonadIO m, (MonadReader Settings m)) => String -> m ()
msg str = asks sSet >>= \b -> when b $ liftIO $ putStrLn str

yawsMain :: YawsIO ()
yawsMain = do
  setWallpaper
  delay <- asks sDelay
  maybe (pure ()) infiniteCycle delay
  where
    minuteToMs = 60000000
    infiniteCycle time = forever $ liftIO (threadDelay (time * minuteToMs)) >> setWallpaper
