module Driver.Api.NotificationApiDriverWreq where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Api.Configuration (NotificationApiSettings(..))
import Data.Text (Text)
import Data.Aeson (object, (.=))
import Network.Wreq (post)

postMessage :: MonadIO m => NotificationApiSettings -> Text -> m ()
postMessage settings message = do
  let
    url = settings.baseUrl <> "/notifications/userRegistered"
    payload = object [ "message" .= message ]
  _ <- liftIO $ post url payload
  pure ()
