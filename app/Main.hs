module Main where

import Algorithm (getImage)
import Control.Monad.Trans.Except (runExceptT)
import Options.Applicative
import Setter (saveImage, setFeh)
import Types
import qualified Wallhaven as W

wallhavenParser :: Parser Source
wallhavenParser = W . Wallhaven <$> option str (long "tags" <> short 't' <> metavar "TAGS..." <> value "" <> help "Tags help")

redditParser :: Parser Source
redditParser = R . Reddit <$> option str (long "subreddit" <> metavar "SUBREDDIT" <> help "Subreddit from which to parse images")

sourceParser :: Parser Source
sourceParser =
  hsubparser
    ( command "wallhaven" (info wallhavenParser (progDesc "Get wallpaper from wallhaven"))
        <> command "reddit" (info redditParser (progDesc "Get wallpaper from selected subreddit"))
        <> commandGroup "Available sources"
    )

settingsParser =
  Settings
    <$> dimensionsParser
    <*> rootDirParser
    <*> xineramaParser
    <*> setParser
    <*> sourceParser
  where
    dimensionsParser = optional (option auto (long "dimensions" <> short 'd' <> metavar "WIDTHxHEIGHT"))
    rootDirParser = optional $ option auto (long "root-directory" <> metavar "$XDG_DATA_DIR/yaws")
    xineramaParser = flag True False (long "no-xinerama" <> help "Disable xinerama and set wallpaper on all screens")
    setParser = flag True False (long "no-set" <> help "Don't set wallpaper instead just save it and return path to it to stdout")

main :: IO ()
main = do
  settings <- execParser opts
  image <- runExceptT $ getImage settings
  case image of
    Left ye -> putStrLn $ "Error occured during getting image: " ++ show ye
    Right img ->
      if sSet settings
        then setImage (sXinerama settings) (sSource settings) img
        else putStrLn $ imageRawUrl img
  where
    opts =
      info
        (settingsParser <**> helper)
        ( fullDesc
            <> progDesc "Download random wallpaper from selected source"
            <> header "yaws - wallpaper downloader"
        )
    setImage xinerama src img = do
      imagePath <- saveImage src img

      setFeh xinerama imagePath
      pure ()
