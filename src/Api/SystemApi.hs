{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api.SystemApi where
import Servant

type SYSTEM_API = "v1" :> "systems" :> "ping" :> Get '[PlainText] String

ping :: Handler String
ping = return "pong"
