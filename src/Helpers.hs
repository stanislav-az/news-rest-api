module Helpers where

import qualified Data.Text                     as T
import qualified Data.Text.Read                as R
import           Database.PostgreSQL.Simple
import qualified Data.String                   as S

textToInteger :: T.Text -> Either String Integer
textToInteger t = fst <$> R.decimal t

textToQuery :: T.Text -> Query
textToQuery = S.fromString . T.unpack

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

listToEither :: e -> [a] -> Either e a
listToEither e []      = Left e
listToEither _ (x : _) = Right x

texify :: (Show a) => a -> T.Text
texify = T.pack . show
