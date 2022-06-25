{-# LANGUAGE OverloadedStrings #-}
module Setter where
import System.Directory (getXdgDirectory, XdgDirectory (XdgData), createDirectoryIfMissing)
-- import Types (Photo (photoUrl, photoId), SourceOptions (W, R))
import System.FilePath
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Network.HTTP.Simple (httpBS, parseRequest, getResponseBody)
-- import Data.Text (Text)
import Data.Aeson.Types (parse)
import System.Process (readProcess)
import Types (Source(..), Image(..))
-- import Data.List.Split

getRootDir :: IO FilePath
getRootDir = getXdgDirectory XdgData "yaws"

-- getPhotoPath :: Photo -> IO FilePath

getSourceDirName :: Source -> FilePath
getSourceDirName a = case a of
  W wo -> "wallhaven"
  R ro -> "reddit"

-- getImageFormat :: String -> String 
-- getImageFormat = last . splitOn "." 
    


saveImage :: Source -> Image -> IO FilePath 
saveImage source photo= do
    rootDir <- getRootDir
    let sourceDirName = getSourceDirName source
    let sourceDirectory = rootDir </> sourceDirName
    createDirectoryIfMissing True sourceDirectory
    let url = imageRawUrl photo
    req <- parseRequest url
    resp <- httpBS req
    let img = getResponseBody resp
    let imgName = imageId photo ++ takeExtension url
    let imgPath = sourceDirectory </>  imgName
    BS.writeFile imgPath img
    return imgPath


setFeh :: Bool -> FilePath -> IO String
setFeh xinerama fp = readProcess "feh" args ""
  where
    bgFill = ["--bg-fill", fp] 
    args = if xinerama
      then bgFill
      else bgFill ++ ["--no-xinerama"]   