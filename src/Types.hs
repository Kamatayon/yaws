


module Types where
import Data.Text
import Control.Monad.Trans.Except (ExceptT)


-- class Photo p where
--     getId :: p -> Text
--     getUrl :: p -> Text


data Image = Image {
    imageId :: String,
    imageRawUrl :: String,
    imageFullUrl :: String
} deriving (Show)

newtype Wallhaven = Wallhaven {
    wallhavenTags :: String
} deriving (Show)

newtype Reddit = Reddit {
    redditSubreddit :: String
} deriving (Show)

data Source = W Wallhaven | R Reddit deriving (Show)

data Settings = Settings {
    sDimensions :: Maybe String,
    sRootDir :: Maybe FilePath,
    sXinerama :: Bool,
    sSet :: Bool,
    sSource :: Source
} deriving (Show)

data YawsError = XRandrParseError | XdpyInfoParseError | CannotParseDimensions 
    | RootDirDoesNotExist | NotImplemented | NoImages deriving (Show)

type YawsIO = ExceptT YawsError IO




-- data Photo = Photo {
--     photoId :: String,
--     photoUrl :: String 
-- } deriving (Show)


-- newtype WallhavenOptions = WallhavenOptions {
--     tags :: String 
-- }

-- newtype RedditOptions = RedditOptions {
--     subreddit :: [String]
-- }

-- data SourceOptions = W WallhavenOptions | R RedditOptions

-- data GlobalOptions = GlobalOptions {
--     width :: Maybe Int,
--     height :: Maybe Int
-- }

-- data ParsedOptions = ParsedOptions {
--     sourceOptions :: SourceOptions,
--     globalOptions :: GlobalOptions
-- }