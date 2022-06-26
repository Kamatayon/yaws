module Algorithm where
import Types (Settings (..), YawsIO, Image, YawsError (NotImplemented, NoImages), Source (..))
import Settings (getDefaultDimensions, getDimensions)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Wallhaven as WH
import qualified Reddit as R
import System.Random (initStdGen, uniformR)

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
      selectRandomImage [] = throwE NoImages
      selectRandomImage imgs = do
        stdGen <- initStdGen
        let (i, _) = uniformR (0, length imgs) stdGen 
        pure $ imgs !! i        
