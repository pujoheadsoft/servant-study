module Driver.Api.NotificationApiDriver where

import Network.HTTP.Req
import Data.Aeson ((.=), object, ToJSON, FromJSON)
import Api.Configuration (NotificationApiSettings(..))
import Text.URI (mkURI)
import Data.Text (pack, Text)
import Control.Exception.Safe (throwIO, Exception)

post
  :: (ToJSON payload, FromJSON response)
  => NotificationApiSettings
  -> Text
  -> payload
  -> IO response
post settings endpoint payload = runReq defaultHttpConfig $ do
  x <- mkURI $ pack settings.baseUrl
  result <- maybe (throwIO (ParseURIException $ "Error: URI not found: " <> settings.baseUrl)) pure (useURI x)
  response <- either k k result

  pure (responseBody response)
  where
    k :: FromJSON b => (Url scheme, Option scheme) -> Req (JsonResponse b)
    k (u, o) = req POST (u /: endpoint) (ReqBodyJson payload) jsonResponse o

postMessage :: NotificationApiSettings -> String -> IO ()
postMessage settings message = do
  let payload = object [ "message" .= message ]
  post settings (pack "userRegistered") payload

newtype ParseURIException = ParseURIException String
  deriving Show

instance Exception ParseURIException