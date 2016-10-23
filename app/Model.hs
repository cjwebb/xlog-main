{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Model () where

import Data.Aeson
import GHC.Generics

newtype LogName = LogName String deriving (Generic, ToJSON)
newtype UserName = UserName String deriving (Generic, ToJSON)

data LogData = LogData {
    t :: String
  , d :: String
} deriving (Generic, ToJSON)

data UserLogs = UserLogs {
  logs :: [LogName]
} deriving (Generic, ToJSON)

data Log = Log {
    name :: LogName
  , logData :: [LogData]
} deriving (Generic, ToJSON)
