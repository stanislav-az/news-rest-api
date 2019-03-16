{-# LANGUAGE OverloadedStrings #-}

module WebServer.UrlParser.Pagination where

import qualified Data.Maybe as MB (fromMaybe)
import qualified Network.Wai as W (Request(..))
import WebServer.UrlParser.Query (getQueryParam)

newtype Limit = Limit
  { unwrapLimit :: Integer
  }

newtype Offset = Offset
  { unwrapOffset :: Integer
  }

instance Read Limit where
  readsPrec x a = map mapper $ readsPrec x a
    where
      mapper (x, y) = (Limit x, y)

instance Show Limit where
  show (Limit x) = show x

instance Read Offset where
  readsPrec x a = map mapper $ readsPrec x a
    where
      mapper (x, y) = (Offset x, y)

instance Show Offset where
  show (Offset x) = show x

getLimitOffset :: Limit -> W.Request -> (Limit, Offset)
getLimitOffset maxLimit req =
  ( MB.fromMaybe maxLimit (getQueryParam req "limit")
  , MB.fromMaybe (Offset 0) (getQueryParam req "offset"))
