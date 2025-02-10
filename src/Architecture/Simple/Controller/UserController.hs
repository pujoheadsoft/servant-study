module Architecture.Simple.Controller.UserController where

import Domain.User (UnvalidatedUser, NotificationSettings)
import Domain.Email (EmailError(InvalidEmailFormat))
import Servant (Handler, ServerError (errBody), throwError, err400, err500)
import Control.Exception (SomeException(..))
import Architecture.Simple.Usecase.UpdateUser (execute)
import Control.Exception.Safe (catches)
import qualified Control.Exception.Safe as Ex 
import Data.ByteString.Lazy.Char8 (pack)

handleSaveUserRequest :: UnvalidatedUser -> NotificationSettings -> Handler ()
handleSaveUserRequest user notificationSettings = do
  execute user notificationSettings
    `catches`
    [ Ex.Handler $ \(InvalidEmailFormat e) -> throwError $ err400 { errBody = pack e }
    , Ex.Handler $ \(SomeException e) -> throwError $ err500 { errBody = pack $ show e }
    ]