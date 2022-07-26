module Network where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON)
import Data.Functor
import Network.HTTP.Simple
import Types

-- performRequest :: FromJSON a => Request -> IO (Either  YawsError Response a)
-- performRequest :: FromJSON a => Request -> YawsIO a
-- performRequest req = liftIO . httpJSON
