{-# LANGUAGE OverloadedStrings #-}

module Serializer.Category where

import           Data.Aeson
import           Database.Models.Category

newtype CreateCategoryRequest = CreateCategoryRequest CategoryRaw

instance FromJSON CreateCategoryRequest where
  parseJSON = withObject "CreateCategoryRequest" $ \v ->
    fmap CreateCategoryRequest
      $   CategoryRaw
      <$> v
      .:  "name"
      <*> v
      .:? "parent_id"

newtype UpdateCategoryRequest = UpdateCategoryRequest CategoryRawPartial

instance FromJSON UpdateCategoryRequest where
  parseJSON = withObject "UpdateCategoryRequest" $ \v ->
    fmap UpdateCategoryRequest
      $   CategoryRawPartial
      <$> v
      .:? "name"
      <*> v
      .:? "parent_id"

newtype CreateCategoryResponse = CreateCategoryResponse CategoryNested

instance ToJSON CreateCategoryResponse where
  toJSON (CreateCategoryResponse (Parent id name)) =
    object ["id" .= id, "name" .= name]
  toJSON (CreateCategoryResponse (CategoryNested id name parent)) = object
    [ "id" .= id
    , "name" .= name
    , "parent" .= (toJSON $ CreateCategoryResponse parent)
    ]

requestToCategory :: CreateCategoryRequest -> CategoryRaw
requestToCategory (CreateCategoryRequest category) = category

requestToUpdateCategory :: UpdateCategoryRequest -> CategoryRawPartial
requestToUpdateCategory (UpdateCategoryRequest category) = category

categoryNestedToResponse :: CategoryNested -> CreateCategoryResponse
categoryNestedToResponse = CreateCategoryResponse
