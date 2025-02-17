module Architecture.TaglessFinal.Gateway.NotificationGatewayPort where

import Data.Text (Text)

class Monad m => NotificationGatewayPort m where
  sendNotification :: Text -> m ()

