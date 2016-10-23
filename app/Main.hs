{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import Network.Wai (responseLBS, Application, Request, Response, rawPathInfo)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Data.Aeson
import GHC.Generics

import Database.SQLite.Simple (open, close, query, Only(..), Connection)
import Database.SQLite.Simple.FromRow (FromRow(..), field)

data Hello = Hello { hello :: String } deriving (Generic, ToJSON)

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  conn <- open "db.sqlite"
  run port (app conn)
  close conn

helloRoute :: Request -> Response
helloRoute req =
  responseLBS
  status200
  [(hContentType, "application/json")]
  . encode $ Hello "World"

notFoundRoute :: Response
notFoundRoute = responseLBS
  status404
  [(hContentType, "application/json")]
  "404 - Not Found"

-- https://singpolyma.net/2013/09/making-a-website-with-haskell/
-- https://stackoverflow.com/questions/29785737/avoiding-errors-caused-by-io-when-talking-to-a-database-inside-of-a-wai-handler

data UserLog = UserLog {
  id :: String,
  username :: String,
  logname :: String
} deriving (ToJSON, Generic)

instance FromRow UserLog where
  fromRow = UserLog <$> field <*> field <*> field

myRoute :: Connection -> IO Response
myRoute conn = do
  let username = "cjwebb" :: String
  [userlogs] <- query conn "SELECT * FROM userlogs WHERE username = ?" (Only username) :: IO [UserLog]
  return (responseLBS status200 [(hContentType, "application/json")] (encode userlogs))

-- todo: remove the do block
app :: Connection -> Application
app conn request respond = do
  res <- case rawPathInfo request of
    "/"         -> return $ helloRoute request
    "/u/cjwebb" -> myRoute conn
    _           -> return $ notFoundRoute
  respond res
