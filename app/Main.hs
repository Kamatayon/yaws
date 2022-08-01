module Main where

import Algorithm (getImage, setWallpaper, yawsMain)
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), runReader)
import Options.Applicative
import Setter (saveImage, setFeh, srcToStr)
import System.Directory (doesPathExist)
import System.Directory.Internal.Prelude (exitFailure)
import System.Exit (exitSuccess)
import Text.Regex.TDFA
import Types
import qualified Wallhaven as W

wallhavenParser :: Parser Source
wallhavenParser = W . Wallhaven <$> option str (long "tags" <> short 't' <> metavar "TAGS..." <> value "" <> help "Tags help")

redditParser :: Parser Source
redditParser = R . Reddit <$> option str (long "subreddit" <> metavar "SUBREDDIT" <> help "Subreddit from which to parse images")

-- unsplashP :: Parser Source
unsplashP =
  U
    <$> ( UnsplashSettings
            <$> collectionsP
            <*> topicsP
            <*> queryP
            <*> orientationP
        )
  where
    orientationReadM :: ReadM Orientation
    orientationReadM =
      str >>= \s -> case s of
        "landscape" -> pure Landscape
        "squarish" -> pure Squarish
        "portrait" -> pure Portrait
        _ -> readerError "Accepted orientation types are 'landscape', 'squarish' and 'portrait'."
    collectionsP = optional $ option str (long "collections")
    topicsP = optional (option str (long "topics"))
    queryP = optional (option str (long "query"))
    orientationP = optional (option orientationReadM (long "orientation"))

sourceP :: Parser Source
sourceP =
  hsubparser
    ( command "wallhaven" (info wallhavenParser (progDesc "Get wallpaper from wallhaven"))
        <> command "reddit" (info redditParser (progDesc "Get wallpaper from selected subreddit"))
        <> command "unsplash" (info unsplashP (progDesc "Get wallpaper from unsplash"))
        <> commandGroup "Available sources"
    )

settingsParser =
  Settings
    <$> dimensionsP
    <*> rootDirP
    <*> xineramaP
    <*> setP
    <*> delayP
    <*> sourceP
  where
    rootDirP = optional $ strOption (long "directory" <> metavar "$XDG_DATA_DIR/yaws" <> help "Directory where to save images. Defaults to $XDG_DATA_DIR/yaws")
    xineramaP = flag True False (long "no-xinerama" <> help "Disable xinerama and set wallpaper on all screens")
    setP = flag True False (long "no-set" <> help "Don't set wallpaper instead just download it and return path to it to stdout")
    delayP = optional $ option auto (long "delay" <> help "If set to any number it will be a delay ( in minutes ) before each setting otherwise it will download random wallpaper only once")

dimensionsP = optional (option dimensionsReadM (long "dimensions" <> short 'd' <> metavar "WIDTHxHEIGHT"))
  where
    regex = "([0-9]+)x([0-9]+)"
    matchStr :: String -> (String, String, String, [String])
    matchStr s = s =~ regex
    parseMatch m = case m of
      (_, _, _, [w, h]) -> Right (read w, read h)
      _ -> Left "Dimensions must be in format of WIDTHxHEIGHT"
    dimensionsReadM = eitherReader $ parseMatch . matchStr

-- -- move it somewhere else?
-- verifyDirectory :: Maybe FilePath -> IO ()
-- verifyDirectory Nothing = pure ()
-- verifyDirectory (Just str) = doesPathExist str >>= \e -> if e then pure () else throwM RootDirDoesNotExist

main :: IO ()
main = do
  settings <- execParser opts
  runReaderT yawsMain settings
  where
    opts =
      info
        (settingsParser <**> helper)
        ( fullDesc
            <> progDesc "Download random wallpaper from selected source"
            <> header "yaws - wallpaper downloader"
        )
