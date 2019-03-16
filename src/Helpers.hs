module Helpers where

import qualified Data.Text as T (Text(..), pack)
import qualified Data.Text.Read as R (decimal)

textToInteger :: T.Text -> Either String Integer
textToInteger t = fst <$> R.decimal t

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

listToEither :: e -> [a] -> Either e a
listToEither e [] = Left e
listToEither _ (x:_) = Right x

texify :: (Show a) => a -> T.Text
texify = T.pack . show
