module Driver.Api.NotificationApiDriverReq where

import Network.HTTP.Req
import Data.Aeson ((.=), object, ToJSON, FromJSON)
import Api.Configuration (NotificationApiSettings(..))
import Text.URI (mkURI)
import Data.Text (pack, Text)
import Control.Exception.Safe (throwIO, Exception)
import Control.Monad.IO.Class (MonadIO)

post
  :: (ToJSON payload, FromJSON response, MonadIO m)
  => NotificationApiSettings
  -> Text
  -> payload
  -> m response
post settings endpoint payload = runReq defaultHttpConfig $ do
  uri <- mkURI $ pack settings.baseUrl
  url <- maybe (throwIO (ParseURIException $ "Error: URI not found: " <> settings.baseUrl)) pure (useURI uri)
  response <- either toRequest toRequest url
  pure (responseBody response)
  where
    toRequest :: FromJSON b => (Url scheme, Option scheme) -> Req (JsonResponse b)
    toRequest (u, o) = req POST (u /: "notifications" /: endpoint) (ReqBodyJson payload) jsonResponse o

postMessage :: MonadIO m => NotificationApiSettings -> Text -> m ()
postMessage settings message = do
  let
    payload = object [ "message" .= message ]
  post settings (pack "userRegistered") payload

newtype ParseURIException = ParseURIException String
  deriving Show

instance Exception ParseURIException