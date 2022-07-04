module Algorithm where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Reddit as R
import Settings (getDefaultDimensions, getDimensions)
import System.Random (initStdGen, uniformR)
import Types (Image, Settings (..), Source (..), YawsError (NoImages, NotImplemented), YawsIO)
import qualified Unsplash as U
import qualified Wallhaven as WH

getImage :: Settings -> YawsIO Image
getImage settings = do
  let d = sDimensions settings
  let xinerama = sXinerama settings
  d <- getDimensions d xinerama
  let source = sSource settings
  images <- getImages source d
  selectRandomImage images
  where
    getImages (W wall) dimensions = liftIO $ WH.getPhotos wall dimensions
    getImages (R reddit) dimensions = liftIO $ R.getImages reddit dimensions
    getImages (U unsplash) dimensions = liftIO $ U.getImages unsplash dimensions
    selectRandomImage [] = throwE NoImages
    selectRandomImage imgs = do
      stdGen <- initStdGen
      let (i, _) = uniformR (0, length imgs) stdGen
      pure $ imgs !! i
