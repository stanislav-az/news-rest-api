{-# LANGUAGE OverloadedStrings #-}

module Serializer.Category where

import qualified Data.Aeson as JSON
  ( FromJSON(..)
  , ToJSON(..)
  , object
  , withObject
  )
import Data.Aeson ((.:), (.:?), (.=))
import Database.Models.Category
  ( CategoryNested(..)
  , CategoryRaw(..)
  , CategoryRawPartial(..)
  )

newtype CreateCategoryRequest =
  CreateCategoryRequest CategoryRaw

newtype UpdateCategoryRequest =
  UpdateCategoryRequest CategoryRawPartial

newtype CreateCategoryResponse =
  CreateCategoryResponse CategoryNested

instance JSON.FromJSON CreateCategoryRequest where
  parseJSON =
    JSON.withObject "CreateCategoryRequest" $ \v ->
      fmap CreateCategoryRequest $
      CategoryRaw <$> v .: "name" <*> v .:? "parent_id"

instance JSON.FromJSON UpdateCategoryRequest where
  parseJSON =
    JSON.withObject "UpdateCategoryRequest" $ \v ->
      fmap UpdateCategoryRequest $
      CategoryRawPartial <$> v .:? "name" <*> v .:? "parent_id"

instance JSON.ToJSON CreateCategoryResponse where
  toJSON (CreateCategoryResponse (Parent id name)) =
    JSON.object ["id" .= id, "name" .= name]
  toJSON (CreateCategoryResponse (CategoryNested id name parent)) =
    JSON.object
      [ "id" .= id
      , "name" .= name
      , "parent" .= (JSON.toJSON $ CreateCategoryResponse parent)
      ]

requestToCategory :: CreateCategoryRequest -> CategoryRaw
requestToCategory (CreateCategoryRequest category) = category

requestToUpdateCategory :: UpdateCategoryRequest -> CategoryRawPartial
requestToUpdateCategory (UpdateCategoryRequest category) = category

categoryNestedToResponse :: CategoryNested -> CreateCategoryResponse
categoryNestedToResponse = CreateCategoryResponse
