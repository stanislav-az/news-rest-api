module Ext.Data.Text.Read where

import qualified Data.Text as T (Text(..))
import qualified Data.Text.Read as R (decimal)

textToInteger :: T.Text -> Either String Integer
textToInteger t = fst <$> R.decimal t
