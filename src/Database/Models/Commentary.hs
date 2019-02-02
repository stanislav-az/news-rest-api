module Database.Models.Commentary where

import           Database.PostgreSQL.Simple.FromRow
import           Data.Text

data Commentary = Commentary {
  commentaryId :: Integer,
  commentaryContent :: Text,
  commentaryNewsId :: Integer
}
