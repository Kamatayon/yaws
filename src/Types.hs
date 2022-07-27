module Types where

import Control.Monad.Catch
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (Reader, ReaderT)
import Data.Data (Typeable)
import Data.Text
import Network.HTTP.Client (HttpException)
import Network.HTTP.Simple

-- class Photo p where
--     getId :: p -> Text
--     getUrl :: p -> Text

data Image = Image
  { imageId :: String,
    imageRawUrl :: String,
    imageFullUrl :: String
  }
  deriving (Show)

newtype Wallhaven = Wallhaven
  { wallhavenTags :: String
  }
  deriving (Show)

newtype Reddit = Reddit
  { redditSubreddit :: String
  }
  deriving (Show)

data Orientation = Landscape | Portrait | Squarish deriving (Show)

data UnsplashSettings = UnsplashSettings
  { usCollections :: Maybe String,
    usTopics :: Maybe String,
    usQuery :: Maybe String,
    usOrientation :: Maybe Orientation
  }
  deriving (Show)

data Source = W Wallhaven | R Reddit | U UnsplashSettings deriving (Show)

data Settings = Settings
  { sDimensions :: Maybe (Int, Int),
    sRootDir :: Maybe FilePath,
    sXinerama :: Bool,
    sSet :: Bool,
    sDelay :: Maybe Int,
    sSource :: Source
  }
  deriving (Show)

type YawsIO = ReaderT Settings IO

data YawsException = RootDirDoesNotExist | NoImagesMatching | UnsplashErrors [String] deriving (Show, Typeable)

instance Exception YawsException
