module Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Api.Router

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server
