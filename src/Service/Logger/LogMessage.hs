{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Service.Logger.LogMessage where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Service.Logger.LogLevel (LogLevel)

data LogMessage = LogMessage
  { level :: LogLevel,
    timestamp :: UTCTime,
    message :: Text
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)
