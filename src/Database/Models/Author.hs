module Database.Models.Author where

import           Database.PostgreSQL.Simple.FromRow
import           Data.Text

data Author = Author {
  authorId :: Integer,
  authorUserId :: Integer,
  authorDescription :: Text
} deriving Show

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field

data AuthorRaw = AuthorRaw {
  authorRawDescription :: Text
}
