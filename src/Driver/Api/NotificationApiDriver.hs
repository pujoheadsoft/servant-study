module Driver.Api.NotificationApiDriver where

import Network.HTTP.Req
import Data.Aeson ((.=), object, ToJSON, FromJSON)
import Api.Configuration (NotificationApiSettings(..))
import Text.URI (mkURI)
import Data.Text (pack, Text)
import Control.Exception.Safe (throwIO, Exception)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8)

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
    toRequest (u, o) = do
      let lbs = encodeUtf8 $ encodeToLazyText payload
      req POST (u /: "notifications" /: endpoint) (ReqBodyLbs lbs) jsonResponse o

postMessage :: MonadIO m => NotificationApiSettings -> Text -> m ()
postMessage settings message = do
  let
    text = "ユーザーID: 1で、名無しの権兵衛さんが登録されました。" :: Text
    payload = object [ "message" .= text ]
  post settings (pack "userRegistered") payload

newtype ParseURIException = ParseURIException String
  deriving Show

instance Exception ParseURIException