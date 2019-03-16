module WebServer.UrlParser.Query where

import qualified Control.Monad as M (join)
import qualified Data.ByteString.Char8 as B8 (ByteString(..), unpack)
import qualified Network.Wai as W (Request(..), queryString)
import qualified Text.Read as R (readMaybe)

getQueryParam :: (Read a) => W.Request -> B8.ByteString -> Maybe a
getQueryParam req key = M.join param >>= (R.readMaybe . B8.unpack)
  where
    param = lookup key (W.queryString req)

getQueryBS :: W.Request -> B8.ByteString -> Maybe B8.ByteString
getQueryBS req key = M.join $ lookup key (W.queryString req)
