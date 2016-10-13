{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import Network.Wai (responseLBS, Application, Request, Response, rawPathInfo)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Data.Aeson

import GHC.Generics

--import Lib

data Hello = Hello { hello :: String } deriving (Generic, ToJSON)

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  run port app

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

-- will need to use this at some point
-- http://stackoverflow.com/questions/7771523/how-do-i-perform-io-inside-a-wai-warp-application

app :: Application
app req res =
  res $ case rawPathInfo req of
    "/" -> helloRoute req
    _   -> notFoundRoute
