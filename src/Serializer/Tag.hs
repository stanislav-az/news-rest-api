{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Serializer.Tag where

import           Data.Aeson
import qualified Data.Text                     as T
import           Database.Models.Tag

newtype CreateTagRequest = CreateTagRequest TagRaw

instance FromJSON CreateTagRequest where
  parseJSON = withObject "CreateTagRequest"
    $ \v -> CreateTagRequest . TagRaw <$> v .: "name"

newtype UpdateTagRequest = UpdateTagRequest TagRaw

instance FromJSON UpdateTagRequest where
  parseJSON = withObject "UpdateTagRequest"
    $ \v -> UpdateTagRequest . TagRaw <$> v .: "name"

newtype CreateTagResponse = CreateTagResponse Tag

instance ToJSON CreateTagResponse where
  toJSON (CreateTagResponse Tag {..}) =
    object ["id" .= tagId, "name" .= tagName]

requestToTag :: CreateTagRequest -> TagRaw
requestToTag (CreateTagRequest tag) = tag

requestToUpdateTag :: UpdateTagRequest -> TagRaw
requestToUpdateTag (UpdateTagRequest tag) = tag

tagToResponse :: Tag -> CreateTagResponse
tagToResponse = CreateTagResponse
