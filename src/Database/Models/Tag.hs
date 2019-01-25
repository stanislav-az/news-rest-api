module Database.Models.Tag where

import           Database.PostgreSQL.Simple.FromRow
import           Data.Text

data Tag = Tag {
  tagId :: Integer,
  tagName :: Text
}

instance FromRow Tag where
  fromRow = Tag <$> field <*> field

data TagRaw = TagRaw {
  tagRawName :: Text
}
