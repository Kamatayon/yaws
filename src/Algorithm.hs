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
    images <- case sSource settings of
      W wall -> liftIO $ WH.getPhotos wall d
      R red -> liftIO $ R.getImages red d
    selectRandomImage images
    where
      -- selectRandomImage :: [Image] -> YawsIO Image
      selectRandomImage [] = throwE NoImages
      selectRandomImage imgs = do
        stdGen <- initStdGen
        let (i, _) = uniformR (0, length imgs) stdGen 
        pure $ imgs !! i        


-- algorithm :: Settings -> IO ()
-- algorithm = do
    
-- algorithm :: Settings -> IO ()
-- algorithm = do
--     photo <- runExceptT $ getPhoto config
--     case photo of      
--       Left ce -> print ce
--       Right ph -> saveImage $ source config $  photo
--     pure ()
