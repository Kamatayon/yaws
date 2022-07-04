module Types where

import Control.Monad.Trans.Except (ExceptT)
import Data.Text

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
  { sDimensions :: Maybe String,
    sRootDir :: Maybe FilePath,
    sXinerama :: Bool,
    sSet :: Bool,
    sSource :: Source
  }
  deriving (Show)

data YawsError
  = XRandrParseError
  | XdpyInfoParseError
  | CannotParseDimensions
  | RootDirDoesNotExist
  | NotImplemented
  | NoImages
  deriving (Show)

type YawsIO = ExceptT YawsError IO
