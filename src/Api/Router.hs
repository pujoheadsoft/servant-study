{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Api.Router where
import Servant ( Proxy(..), type (:<|>)(..), Server )
import Api.SystemApi (SYSTEM_API, ping)
import Api.UserApi (USER_API, getUser, putUser)
import Database.Persist.Postgresql (ConnectionPool)
import Api.Configuration (ApiSettings)

type API = SYSTEM_API :<|> USER_API

api :: Proxy API
api = Proxy

server :: ApiSettings -> ConnectionPool -> Server API
server apiSettings pool = ping 
    :<|> getUser pool
    :<|> putUser apiSettings pool