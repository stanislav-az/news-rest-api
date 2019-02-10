{-# LANGUAGE OverloadedStrings #-}

module WebServer.UrlParser.Filter where

import           Data.Time
import           Network.Wai
import           Control.Applicative
import qualified Data.Text                     as T
import qualified Data.ByteString               as B
import           WebServer.UrlParser.Query

data Filter = Filter {
  filterDateCreated :: Maybe DateCreated,
  filterAuthorName :: Maybe B.ByteString,
  filterCategoryId :: Maybe Integer,
  filterTagIds :: Maybe TagIds,
  filterNewsTitleHas :: Maybe B.ByteString,
  filterNewsContentHas :: Maybe B.ByteString
} deriving Show

data DateCreated = CreatedAt Day | CreatedAtLt Day | CreatedAtGt Day
  deriving Show

data TagIds = TagId Integer | TagsIn [Integer] | TagsAll [Integer]
  deriving Show

getFilter :: Request -> Filter
getFilter req = Filter
  { filterDateCreated    = (CreatedAt <$> getQueryParam req "created_at")
                           <|> (CreatedAtLt <$> getQueryParam req "created_at_lt")
                           <|> (CreatedAtGt <$> getQueryParam req "created_at_gt")
  , filterAuthorName     = getQueryBS req "author_name"
  , filterCategoryId     = getQueryParam req "category_id"
  , filterTagIds         = (TagId <$> getQueryParam req "tag_id")
                           <|> (TagsIn <$> getQueryParam req "tag_id_in")
                           <|> (TagsAll <$> getQueryParam req "tag_id_all")
  , filterNewsTitleHas   = getQueryBS req "title_has"
  , filterNewsContentHas = getQueryBS req "content_has"
  }
