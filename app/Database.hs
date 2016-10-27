{-# LANGUAGE OverloadedStrings #-}

module Database (getUserLogs, getLogData) where

import Model (UserName, UserLog, LogData, LogName)
import Database.SQLite.Simple (query, Only(..), Connection)

getUserLogs :: Connection -> UserName -> IO [UserLog]
getUserLogs conn username =
  query conn "SELECT * FROM userlogs WHERE username = ?" (Only username)

getLogData :: Connection -> UserName -> LogName -> IO [LogData]
getLogData conn username logname =
  query conn "SELECT id, t, d FROM logdata WHERE name = ? AND username = ? ORDER BY t DESC" (logname, username)
