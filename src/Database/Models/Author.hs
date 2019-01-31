module Database.Models.Author where

import           Database.PostgreSQL.Simple.FromRow
import           Data.Text
import           Database.Models.User

data Author = Author {
  authorId :: Integer,
  authorUserId :: Integer,
  authorDescription :: Text
} deriving Show

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field

data AuthorNested = AuthorNested {
  authorNestedId :: Integer,
  authorNestedUser :: User,
  authorNestedDescription :: Text
}

data AuthorRaw = AuthorRaw {
  authorRawDescription :: Text
}
