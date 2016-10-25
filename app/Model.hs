{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model ( UserLog, LogData, UserName, LogName, Id ) where

import Data.Aeson
import GHC.Generics
import Database.SQLite.Simple.FromRow (FromRow(..), field)

type UserName = String
type LogName = String
type Id = String

data UserLog = UserLog {
    id :: Id
  , username :: UserName
  , logname :: LogName
} deriving (Generic, ToJSON)

instance FromRow UserLog where
  fromRow = UserLog <$> field <*> field <*> field

data LogData = LogData {
    id :: Id
  , t :: String -- todo: make timestamp
  , d :: String -- todo: make some kind of ADT
} deriving (Generic, ToJSON)

instance FromRow LogData where
  fromRow = LogData <$> field <*> field <*> field
