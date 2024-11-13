{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Controller.Router where
import Servant
import Controller.SystemController (SYSTEM_API, ping)
import Controller.UserController (USER_API, getUser, putUser)

type API = SYSTEM_API :<|> USER_API

api :: Proxy API
api = Proxy

server :: Server API
server = ping 
    :<|> getUser
    :<|> putUser