{-# LANGUAGE OverloadedStrings #-}

module Database (getUserLogs, getLog) where

import Model (UserName, UserLog, LogData, LogName)
import Database.SQLite.Simple (open, close, query, Only(..), Connection)

getUserLogs :: Connection -> UserName -> IO [UserLog]
getUserLogs conn username =
  query conn "SELECT * FROM userlogs WHERE username = ?" (Only username)

getLog :: Connection -> LogName -> IO [LogData]
getLog conn logname =
  query conn "SELECT * FROM logs WHERE name = ? ORDER BY t DESC" (Only logname)
