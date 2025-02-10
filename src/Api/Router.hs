{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Api.Router where
import Servant ( Proxy(..), type (:<|>)(..), Server )
import Api.SystemApi (SYSTEM_API, ping)
import Api.UserApi (USER_API, getUser, putUser)

type API = SYSTEM_API :<|> USER_API

api :: Proxy API
api = Proxy

server :: Server API
server = ping 
    :<|> getUser
    :<|> putUser