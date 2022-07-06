{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Main where

import Algorithm (getImage)
import Control.Monad.Trans.Except (runExceptT)
import Options.Applicative
import Setter (saveImage, setFeh)
import System.Directory.Internal.Prelude (exitFailure)
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
        "landscape" -> return Landscape
        "squarish" -> return Squarish
        "portrait" -> return Portrait
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
    <*> sourceP
  where
    dimensionsP = optional (option auto (long "dimensions" <> short 'd' <> metavar "WIDTHxHEIGHT"))
    rootDirP = optional $ option auto (long "root-directory" <> metavar "$XDG_DATA_DIR/yaws" <> help "Directory where to save images. Defaults to $XDG_DATA_DIR/yaws")
    xineramaP = flag True False (long "no-xinerama" <> help "Disable xinerama and set wallpaper on all screens")
    setP = flag True False (long "no-set" <> help "Don't set wallpaper instead just download it and return path to it to stdout")

main :: IO ()
main = do
  settings <- execParser opts
  imgEither <- runExceptT $ getImage settings
  image <- case imgEither of
    Left ye -> handleYawsError ye
    Right img -> pure img
  let source = sSource settings
  if sSet settings
    then setImage (sXinerama settings) (sSource settings) image
    else saveImage source image >>= putStrLn
  where
    handleYawsError ye = putStrLn ("Error occured during getting the image: " ++ show ye) >> exitFailure
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
