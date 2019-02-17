{-# LANGUAGE OverloadedStrings #-}

module WebServer.UrlParser.Filter where

import qualified Data.Time                     as Time
                                                ( Day(..) )
import qualified Network.Wai                   as W
                                                ( Request(..) )
import           Control.Applicative            ( (<|>) )
import qualified Data.Text                     as T
                                                ( Text(..) )
import qualified Data.ByteString               as B
                                                ( ByteString(..) )
import           WebServer.UrlParser.Query      ( getQueryParam
                                                , getQueryBS
                                                )

data Filter = Filter {
  filterDateCreated :: Maybe DateCreated,
  filterAuthorName :: Maybe B.ByteString,
  filterCategoryId :: Maybe Integer,
  filterTagIds :: Maybe TagIds,
  filterNewsTitleHas :: Maybe B.ByteString,
  filterNewsContentHas :: Maybe B.ByteString
} deriving Show

data DateCreated = CreatedAt Time.Day | CreatedAtLt Time.Day | CreatedAtGt Time.Day
  deriving Show

data TagIds = TagId Integer | TagsIn [Integer] | TagsAll [Integer]
  deriving Show

getFilter :: W.Request -> Filter
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
