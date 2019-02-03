{-# LANGUAGE OverloadedStrings #-}

module WebServer.Pagination where

import           Control.Monad
import           Data.Maybe                     ( fromMaybe )
import qualified Data.ByteString.Char8         as B8
import qualified Text.Read                     as R
import           Network.Wai

newtype Limit = Limit {
  unwrapLimit :: Integer
}

instance Read Limit where
  readsPrec x a = map mapper $ readsPrec x a
    where mapper (x, y) = (Limit x, y)

instance Show Limit where
  show (Limit x) = show x

newtype Offset = Offset {
  unwrapOffset :: Integer
}

instance Read Offset where
  readsPrec x a = map mapper $ readsPrec x a
    where mapper (x, y) = (Offset x, y)

instance Show Offset where
  show (Offset x) = show x

getLimitOffset :: Limit -> Request -> (Limit, Offset)
getLimitOffset maxLimit req =
  ( fromMaybe maxLimit   (getQueryParam req "limit")
  , fromMaybe (Offset 0) (getQueryParam req "offset")
  )

getQueryParam :: (Read a) => Request -> B8.ByteString -> Maybe a
getQueryParam req key = join param >>= (R.readMaybe . B8.unpack)
  where param = lookup key (queryString req)
