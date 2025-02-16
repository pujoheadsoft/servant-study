module Architecture.Simple.Gateway.NotificationGateway where

import Domain.Message (Message, HasValue(..))
import Control.Lens ((^.))
import Data.Text (Text)

sendNotification :: NotificationGatewayPort m -> Message -> m ()
sendNotification port message = port.sendNotification $ message ^. value

data NotificationGatewayPort m = NotificationGatewayPort {
  sendNotification :: Text -> m ()
}
