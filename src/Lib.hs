module Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Api.Router
import Database.Persist.Postgresql (createPostgresqlPool, ConnectionPool)
import Api.Configuration (configuration, AppSettings (..), DatabaseSettings (..), ApiSettings)
import Data.ByteString.Char8 (pack)
import Control.Monad.Logger (runStderrLoggingT)

startApp :: IO ()
startApp = do
  settings <- configuration
  pool <- connectionPool settings
  run 8081 (app settings.api pool)

app :: ApiSettings -> ConnectionPool -> Application
app apiSettings pool = do
  serve api (server apiSettings pool)

connectionPool :: AppSettings -> IO ConnectionPool
connectionPool settings = do
  let 
    connectionAsString = unwords 
      [ "host=" ++ settings.db.host
      , "port=" ++ show settings.db.port
      , "user=" ++ settings.db.user
      , "password=" ++ settings.db.password
      , "dbname=" ++ settings.db.database
      ]
  runStderrLoggingT $ createPostgresqlPool (pack connectionAsString) 10