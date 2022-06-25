module Settings where

import Types
import System.Directory
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import System.Process
import Text.Regex.TDFA
import Data.Maybe (catMaybes)
import Data.Bifunctor



getRootDir :: Maybe FilePath -> YawsIO FilePath
getRootDir rootDir = case rootDir of
  Nothing -> liftIO getDefaultRootDir
  Just s -> validateRootDir s
  where
      getDefaultRootDir = do
        dir <- getXdgDirectory XdgData "yaws"
        createDirectoryIfMissing True dir
        pure dir
      validateRootDir s = do
        exists <- liftIO $  doesDirectoryExist s 
        if exists
            then pure s
            else throwE RootDirDoesNotExist

getDefaultDimensions :: Bool -> YawsIO (Int, Int)
getDefaultDimensions False = do
    output <- liftIO $ readProcess "xdpyinfo" [] ""
    let regex = "dimensions:[ ]+([0-9]+)x([0-9]+)"
    case (output =~ regex :: (String, String, String, [String])) of
        (_, _, _, [w, h]) -> pure (read w, read h)
        _ -> throwE XdpyInfoParseError

getDefaultDimensions True = do
    output <- liftIO $ readProcess "xrandr" [] ""
    let matches = (output =~ regex) :: [[String]]
    let monitors = catMaybes $ parseMatch <$> matches
    getMinimum monitors
    where
        regex = "([0-9]+)x([0-9]+)[ ]+[0-9]+.[0-9]+[*]"
        parseMatch :: [String] -> Maybe (Int, Int)
        parseMatch [_, w, h] = Just (read w, read h)
        parseMatch _ = Nothing
        getMinimum :: [(Int, Int)] -> YawsIO (Int, Int)
        getMinimum [] = throwE XRandrParseError
        getMinimum l = pure $ bimap minimum minimum $ unzip l

parseDimensions :: String -> Maybe (Int, Int)
parseDimensions = parseMatch . matchStr
    where
        regex = "([0-9]+)x([0-9]+)"
        matchStr :: String -> (String, String, String, [String])
        matchStr s = s =~ regex
        parseMatch m = case m of
            (_, _, _, [w, h]) -> pure (read w, read h)
            _ -> Nothing

getDimensions :: Maybe String -> Bool -> YawsIO (Int, Int)
getDimensions dimensionsStr xinerama = case dimensionsStr of
  Nothing -> getDefaultDimensions xinerama
  Just s -> case parseDimensions s of
    Nothing -> throwE CannotParseDimensions
    Just x1 -> pure x1
            


    



    