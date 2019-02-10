{-# LANGUAGE OverloadedStrings #-}

module WebServer.UrlParser.Filter where

import           Data.Time
import           Network.Wai
import           Control.Applicative
import qualified Data.Text                     as T
import           WebServer.UrlParser.Query

data Filter = Filter {
  filterDateCreated :: Maybe DateCreated,
  filterAuthorName :: Maybe T.Text,
  filterCategoryId :: Maybe Integer,
  filterTagIds :: Maybe TagIds,
  filterNewsTitleHas :: Maybe T.Text,
  filterNewsContentHas :: Maybe T.Text
} deriving Show
-- search content, author_name, category_name, tag_name
-- sort by date, author_name, category_name, photos_num

data DateCreated = CreatedAt Day | CreatedAtLt Day | CreatedAtGt Day
  deriving Show

-- instance Show DateCreated where
--   show (CreatedAt   d) = show d
--   show (CreatedAtLt d) = show d
--   show (CreatedAtGt d) = show d

data TagIds = TagId Integer | TagsIn [Integer] | TagsAll [Integer]
  deriving Show

getFilter :: Request -> Filter
getFilter req = Filter
  { filterDateCreated    = (CreatedAt <$> getQueryParam req "created_at")
                           <|> (CreatedAtLt <$> getQueryParam req "created_at_lt")
                           <|> (CreatedAtGt <$> getQueryParam req "created_at_gt")
  , filterAuthorName     = getQueryParam req "author_name"
  , filterCategoryId     = getQueryParam req "category_id"
  , filterTagIds         = (TagId <$> getQueryParam req "tag_id")
                           <|> (TagsIn <$> getQueryParam req "tag_id_in")
                           <|> (TagsAll <$> getQueryParam req "tag_id_all")
  , filterNewsTitleHas   = getQueryParam req "title_has"
  , filterNewsContentHas = getQueryParam req "content_has"
  }
