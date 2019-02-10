{-# LANGUAGE OverloadedStrings #-}

module WebServer.UrlParser.Sorter where

import           Network.Wai
import           WebServer.UrlParser.Query

data Sorter = Author | Category | Date | Photos
  deriving Show

getSorter :: Request -> Maybe Sorter
getSorter req = getQueryBS req "sort_by" >>= parse
 where
  parse "author"   = Just Author
  parse "category" = Just Category
  parse "date"     = Just Date
  parse "photos"   = Just Photos
  parse _          = Nothing
