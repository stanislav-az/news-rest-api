module WebServer.UrlParser.Query where

import qualified Data.ByteString.Char8         as B8
import qualified Text.Read                     as R
import           Network.Wai
import           Control.Monad                  ( join )

getQueryParam :: (Read a) => Request -> B8.ByteString -> Maybe a
getQueryParam req key = join param >>= (R.readMaybe . B8.unpack)
  where param = lookup key (queryString req)

getQueryBS :: Request -> B8.ByteString -> Maybe B8.ByteString
getQueryBS req key = join $ lookup key (queryString req)
