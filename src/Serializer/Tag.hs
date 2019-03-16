{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serializer.Tag where

import qualified Data.Aeson as JSON
  ( FromJSON(..)
  , ToJSON(..)
  , object
  , withObject
  )
import Data.Aeson ((.:), (.=))
import Database.Models.Tag (Tag(..), TagRaw(..))

newtype CreateTagRequest =
  CreateTagRequest TagRaw

newtype UpdateTagRequest =
  UpdateTagRequest TagRaw

newtype CreateTagResponse =
  CreateTagResponse Tag

instance JSON.FromJSON CreateTagRequest where
  parseJSON =
    JSON.withObject "CreateTagRequest" $ \v ->
      CreateTagRequest . TagRaw <$> v .: "name"

instance JSON.FromJSON UpdateTagRequest where
  parseJSON =
    JSON.withObject "UpdateTagRequest" $ \v ->
      UpdateTagRequest . TagRaw <$> v .: "name"

instance JSON.ToJSON CreateTagResponse where
  toJSON (CreateTagResponse Tag {..}) =
    JSON.object ["id" .= tagId, "name" .= tagName]

requestToTag :: CreateTagRequest -> TagRaw
requestToTag (CreateTagRequest tag) = tag

requestToUpdateTag :: UpdateTagRequest -> TagRaw
requestToUpdateTag (UpdateTagRequest tag) = tag

tagToResponse :: Tag -> CreateTagResponse
tagToResponse = CreateTagResponse
