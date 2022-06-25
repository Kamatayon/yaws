module Main where


import Types
import qualified Wallhaven as W
import Options.Applicative
import Control.Monad.Trans.Except (runExceptT)
import Algorithm (getImage)
import Setter (setFeh, saveImage)

wallhavenParser :: Parser Source
wallhavenParser = W . Wallhaven <$> option str (long "tags" <> short 't' <> metavar "TAGS..." <> value "" <> help "Tags help")

redditParser :: Parser Source
redditParser = R . Reddit <$> option str (long "subreddit" <> metavar "SUBREDDIT" <> help "Subreddit from which to parse images")

sourceParser :: Parser Source
sourceParser = hsubparser ( command "wallhaven" (info wallhavenParser (progDesc "Get wallpaper from wallhaven"))
                        <> command "reddit" (info redditParser (progDesc "Get wallpaper from selected subreddit"))
                        <> commandGroup "Available sources")

dimensionsParser :: Parser ( Maybe String)
dimensionsParser = optional (option auto (long "dimensions" <> short 'd' <> metavar "WIDTHxHEIGHT"))
-- settingsParser :: Parser Settings
settingsParser = Settings
    <$> dimensionsParser
    <*> rootDirParser
    <*> xineramaParser
    <*> setParser
    <*> sourceParser
    where
        rootDirParser = optional $ option auto (long "root-directory" <> metavar "$XDG_DATA_DIR/yaws")
        xineramaParser = flag True False (long "no-xinerama" <> help "Disable xinerama and set wallpaper on all screens")
        setParser = flag True False (long "no-set" <> help "Don't set wallpaper instead just save it and return path to it to stdout")

main = do
    settings <- execParser opts 
    image <- runExceptT $ getImage settings
    case image of
      Left ye -> putStrLn  $ "Error occured during getting image: " ++ show ye
      Right img -> if sSet settings
            then setImage (sXinerama settings) ( sSource settings) img 
            else putStrLn $ imageRawUrl img 
    -- print settings
    where
        opts = info (settingsParser <**> helper)
            (fullDesc
            <> progDesc "Download random wallpaper from selected source"
            <> header "yaws - wallpaper downloader")
        setImage xinerama src img = do
            imagePath <- saveImage src img
            
            setFeh xinerama imagePath
            pure ()
            

-- import Data.Aeson
-- import Network.HTTP.Simple
-- import Data.Text

-- import Options.Applicative
-- import Options.Applicative.Arrows

-- import Types
-- import Wallhaven (getPhotos)
-- import System.Random (initStdGen, uniformR)
-- import Setter (saveImage, setFeh)
-- -- import Config
-- -- import Types
-- -- import Wallhaven (getPhoto, WallhavenPhoto)
-- -- import Files (saveImage)
-- main :: IO ()
-- -- putStrLn "g"

-- -- data UniversalSettings = UniversalSettings {
-- --     minimumWidth :: Int,
-- --     minimumHeight :: Int
-- -- }

-- -- data SourceSettings = Wallhaven {
-- --     tags :: [Text]
-- -- } | Reddit {
-- --     subreddit :: Text 
-- -- } deriving (Show)
-- wallhavenParser :: Parser Source
-- wallhavenParser 

-- -- wallhavenParser :: Parser SourceOptions
-- -- wallhavenParser = W . Wallhaven 
-- --     <$>  option str (long "tags" <> short 't' <> metavar "TAGS..." <> value "" <> help "Tags help")


-- -- redditParser :: Parser SourceOptions
-- -- redditParser = R . RedditOptions <$> option auto (long "subreddit" <> metavar "SUBREDDIT")



-- -- universalSettingsParser = GlobalOptions
-- --     <$> optional (option auto (long "width" <> help "Minimum width ( default: maximum width of your displays )"))
-- --     <*> optional (option auto (long "height" <> help "Minimum height ( default : maximum height of your displays )"))

-- -- -- sourceSettingsParser :: Parser GlobalConfig
-- -- parser :: Parser ParsedOptions
-- -- parser = ParsedOptions <$> hsubparser (
-- --     command "wallhaven" (info wallhavenParser (progDesc "Get wallpaper from wallhaven"))
-- --     <> command "reddit" (info redditParser (progDesc "Get wallpaper from selected subreddit"))
-- --     <> commandGroup "Available sources"
-- --     ) <*> universalSettingsParser

-- -- data GlobalConfig = GlobalConfig {
-- --     sourceSettings :: SourceSettings,
-- --     universalSettings :: UniversalSettings
-- -- }

-- -- wallhavenOpts :: ParserInfo Wallhaven t
-- -- wallhavenOpts = info (sample <**> helper)


-- -- parser :: Parser GlobalConfig
-- -- parser = subparser (command "wallhaven" (info Wallhaven <$> argument str idm) idm)

-- main = do
--     let opts = info (parser <**> helper)
--               ( fullDesc
--                 <> progDesc "Gets random wallpaper from selected source"
--                 <> header "yaws header placeholder" )
--     options <- execParser opts
--     let so = sourceOptions options
--     let go = globalOptions options
--     -- photos <- case so of
--     --     W wo -> getPhotos wo go
--     --     R ro -> pure []
--     -- stdGen <- initStdGen
--     -- let (i, _) = uniformR (0, Prelude.length photos) stdGen
--     -- let photo = photos !! i
--     -- imgPath <- saveImage so photo
--     -- output <- setFeh imgPath
--     -- getPhotos so go
--     -- print $ width $ globalOptions  


--     -- let config = Config {width=1920, height=1080, tags=[]}
--     -- photo <- getPhoto config
--     -- let source = Wallhaven

--     -- saveImage source photo
--     -- response <- httpLBS "https://picsum.photos/200/300"
--     -- print response
--     -- -- client <- H.
--     putStrLn "Hello"

