{-# LANGUAGE OverloadedStrings #-}

module WebServer.UrlParser.Pagination where

import           Data.Maybe                     ( fromMaybe )
import           WebServer.UrlParser.Query
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

