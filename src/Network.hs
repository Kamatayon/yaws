{-# LANGUAGE ScopedTypeVariables #-}

module Network where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, throwE)
import Data.Aeson (FromJSON)
import Data.Functor
import Network.HTTP.Simple
import Types

-- performRequest :: FromJSON a => Request -> IO (Either  YawsError Response a)
performRequest :: FromJSON a => Request -> YawsIO a
performRequest req = doReq
  where
    doReq =
      catches
        (getResponseBody <$> httpJSON req)
        [ Handler (\(e :: HttpException) -> throwE $ HE e),
          Handler (\(e :: JSONException) -> throwE $ JE e)
        ]
    -- respToYawsIO = catch doReq
    handleExc (e :: HttpException) = pure . Left $ HE e
