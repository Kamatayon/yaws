module Algorithm where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import qualified Reddit as R
import Setter (saveImage, setFeh, srcToStr)
import Settings (getDimensions)
import System.Exit
import System.Random (initStdGen, uniformR)
import Types (Image (..), Settings (..), Source (..), YawsException (NoImagesMatching), YawsIO)
import qualified Unsplash as U
import qualified Wallhaven as WH

getImage :: YawsIO Image
getImage = do
  d <- getDimensions
  source <- asks sSource
  images <- getImages source d
  selectRandomImage images
  where
    getImages (W wall) dimensions = WH.getPhotos wall dimensions
    getImages (R reddit) dimensions = R.getImages reddit dimensions
    getImages (U unsplash) dimensions = U.getImages unsplash dimensions
    selectRandomImage [] = throwM NoImagesMatching
    selectRandomImage imgs = do
      stdGen <- initStdGen
      let (i, _) = uniformR (0, length imgs - 1) stdGen
      pure $ imgs !! i

setWallpaper :: YawsIO ()
setWallpaper = do
  image <- getImage
  source <- asks sSource
  liftIO $ urlMsg image source
  imagePath <- saveImage image
  xinerama <- asks sXinerama
  liftIO $ setFeh xinerama imagePath

  pure ()
  where
    urlMsg img src = putStrLn $ "Selected image from " ++ srcToStr src ++ ". Url: " ++ imageFullUrl img
    handleYawsError ye = putStrLn ("Error occured during getting the image: " ++ show ye) >> exitFailure

yawsMain :: YawsIO ()
yawsMain = do
  setWallpaper
  delay <- asks sDelay
  maybe (pure ()) infiniteCycle delay
  where
    minuteToMs = 60000000
    infiniteCycle time = forever $ liftIO (threadDelay (time * minuteToMs)) >> setWallpaper
