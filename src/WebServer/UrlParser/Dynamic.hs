{-# LANGUAGE OverloadedStrings #-}

module WebServer.UrlParser.Dynamic where

import qualified Data.Text                     as T
                                                ( Text(..) )
import           Helpers                        ( textToInteger )
import           WebServer.HandlerMonad         ( DynamicPathsMap(..) )

getIdFromUrl :: DynamicPathsMap -> Either String Integer
getIdFromUrl dpMap = getKeyFromUrl dpMap "id" >>= textToInteger

getSearchTextFromUrl :: DynamicPathsMap -> Either String T.Text
getSearchTextFromUrl dpMap = getKeyFromUrl dpMap "search"

getKeyFromUrl :: DynamicPathsMap -> T.Text -> Either String T.Text
getKeyFromUrl dpMap key = maybe (Left "no info") Right $ lookup key dpMap
