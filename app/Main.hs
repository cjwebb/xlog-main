{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import Network.Wai (responseLBS, Application, Request, Response, rawPathInfo, responseFile)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static (staticPolicy, only)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Data.Aeson (encode)
import Database.SQLite.Simple (open, close, Connection)
import Model (UserLog, UserName, LogName)
import Database

main :: IO ()
main = do
  let port = 3000
  let middleware = staticPolicy (only [("static/main.js", "./elm/main.js")])
  putStrLn $ "Listening on port " ++ show port
  conn <- open "db.sqlite"
  run port $ middleware $ (app conn)
  close conn

rootRoute :: Request -> Response
rootRoute req =
  responseFile
  status200
  [(hContentType, "text/html")]
  "static/index.html"
  Nothing

notFoundRoute :: Response
notFoundRoute = responseLBS
  status404
  [(hContentType, "application/json")]
  "404 - Not Found"

-- https://singpolyma.net/2013/09/making-a-website-with-haskell/
-- https://stackoverflow.com/questions/29785737/avoiding-errors-caused-by-io-when-talking-to-a-database-inside-of-a-wai-handler

-- todo: refactor 200 OK Json to method,
--       handle bad database access
--       handle non-existent user/logs
userLogsRoute :: Connection -> UserName -> IO Response
userLogsRoute conn username = do
  userlogs <- Database.getUserLogs conn username
  return (responseLBS status200 [(hContentType, "application/json")] (encode userlogs))

logDataRoute :: Connection -> UserName -> LogName -> IO Response
logDataRoute conn username logname = do
  logData <- Database.getLogData conn username logname
  return (responseLBS status200 [(hContentType, "application/json")] (encode logData))

-- todo: remove the do block
app :: Connection -> Application
app conn request respond = do
  res <- case rawPathInfo request of
    "/"         -> return $ rootRoute request
    "/api/users/cjwebb" -> userLogsRoute conn "cjwebb"
    "/api/users/cjwebb/logs/weight" -> logDataRoute conn "cjwebb" "weight"
    _           -> return $ notFoundRoute
  respond res
