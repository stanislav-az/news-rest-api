{-# LANGUAGE OverloadedStrings #-}

module News.WebServer.UrlParser.Sorter where

import qualified Network.Wai as W (Request(..))
import News.WebServer.UrlParser.Query (getQueryBS)

data Sorter
  = Author
  | Category
  | Date
  | Photos
  deriving (Show)

getSorter :: W.Request -> Maybe Sorter
getSorter req = getQueryBS req "sort_by" >>= parse
  where
    parse "author" = Just Author
    parse "category" = Just Category
    parse "date" = Just Date
    parse "photos" = Just Photos
    parse _ = Nothing
