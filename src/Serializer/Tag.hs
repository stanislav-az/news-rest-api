{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serializer.Tag where

import qualified Data.Aeson                    as JSON
                                                ( FromJSON(..)
                                                , ToJSON(..)
                                                , withObject
                                                , object
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.=)
                                                )
import           Database.Models.Tag            ( Tag(..)
                                                , TagRaw(..)
                                                )

newtype CreateTagRequest = CreateTagRequest TagRaw

instance JSON.FromJSON CreateTagRequest where
  parseJSON = JSON.withObject "CreateTagRequest"
    $ \v -> CreateTagRequest . TagRaw <$> v .: "name"

newtype UpdateTagRequest = UpdateTagRequest TagRaw

instance JSON.FromJSON UpdateTagRequest where
  parseJSON = JSON.withObject "UpdateTagRequest"
    $ \v -> UpdateTagRequest . TagRaw <$> v .: "name"

newtype CreateTagResponse = CreateTagResponse Tag

instance JSON.ToJSON CreateTagResponse where
  toJSON (CreateTagResponse Tag {..}) =
    JSON.object ["id" .= tagId, "name" .= tagName]

requestToTag :: CreateTagRequest -> TagRaw
requestToTag (CreateTagRequest tag) = tag

requestToUpdateTag :: UpdateTagRequest -> TagRaw
requestToUpdateTag (UpdateTagRequest tag) = tag

tagToResponse :: Tag -> CreateTagResponse
tagToResponse = CreateTagResponse
