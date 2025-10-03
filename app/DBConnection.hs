{-# LANGUAGE OverloadedStrings #-}
module DBConnection where

import Database.MySQL.Simple

getConn :: IO Connection
getConn = connect defaultConnectInfo
  { connectHost = "localhost"
  , connectUser = "root"
  , connectPassword = "kvayb"
  , connectDatabase = "software_portal"
  }