{-
  run系関数を全部ほかのモジュールに移動したバージョン
  runを他レイヤーに依存させないようにするために、レコードに定義した関数に処理を依存させている。
  レコードは然るべきレイヤーで作る。
  GatewayとUsecaseの関係を、レコードを通して示すことができるが、レコードの定義が必要になり、コードが増える。
  そのためrun系はControllerにまとめてしまった方がよいかもしれない。
-}
module Architecture.Polysemy.Controller.UserController2 where

import Domain.User (UnvalidatedUser, UserProfile)
import Domain.Email (EmailError(InvalidEmailFormat))
import Servant (Handler, ServerError (errBody), throwError, err400, err500)
import Control.Exception (SomeException(..))
import Control.Exception.Safe (catches)
import qualified Control.Exception.Safe as Ex
import Data.ByteString.Lazy.Char8 (pack)
import Database.Persist.Postgresql (runSqlPool, ConnectionPool, SqlBackend)
import Control.Monad.IO.Class (liftIO)
import Architecture.Polysemy.Usecase.SaveUser (execute)
import Polysemy (runM, Member, Embed, Sem, embed)
import Polysemy.Error (runError)
import Control.Monad.Reader (ReaderT)
import qualified Architecture.Polysemy.Usecase.UserPort as UserPort
import qualified Architecture.Polysemy.Usecase.NotificationPort as NotificationPort
import qualified Architecture.Polysemy.Gateway.UserGateway as UserGateway
import qualified Architecture.Polysemy.Gateway.NotificationGateway as NotificationGateway
import qualified Architecture.Polysemy.Gateway.UserGatewayPort as UserGatewayPort
import qualified Architecture.Polysemy.Gateway.NotificationGatewayPort as NotificationGatewayPort
import qualified Driver.UserDb.UserDriver as UserDriver
import qualified Driver.Api.NotificationApiDriverReq as NotificationDriver
import Api.Configuration (NotificationApiSettings)
import Common.Logger (logError)

handleSaveUserRequest :: NotificationApiSettings -> ConnectionPool -> UnvalidatedUser -> UserProfile -> Bool -> Handler String
handleSaveUserRequest notificationApiSettings pool user profile withNotify = do
  liftIO $ flip runSqlPool pool do
    run notificationApiSettings user profile withNotify >>= either
      Ex.throw -- 外側のハンドラに任せる
      (const $ pure "OK")
  `catches`
  [ Ex.Handler $ \(InvalidEmailFormat e) -> do
    logError e
    throwError $ err400 { errBody = pack e }
  , Ex.Handler $ \(SomeException e) -> do
    logError e
    throwError $ err500 { errBody = pack $ show e }
  ]

run :: NotificationApiSettings -> UnvalidatedUser -> UserProfile -> Bool -> ReaderT SqlBackend IO (Either EmailError ())
run notificationApiSettings user profile withNotify =
  runM
  . runError
  . UserGatewayPort.runUserGatewayPort createUserGatewayPortFunctions
  . UserPort.runUserPort UserGateway.createUserPortFunctions
  . NotificationGatewayPort.runNotificationGatewayPort (createNotificationGatewayPortFunctions notificationApiSettings)
  . NotificationPort.runNotificationPort NotificationGateway.createNotificationPortFunctions
  $ execute user profile withNotify

createUserGatewayPortFunctions
  :: Member (Embed (ReaderT SqlBackend IO)) r
  => UserGatewayPort.UserGatewayPortFunctions (Sem r)
createUserGatewayPortFunctions = UserGatewayPort.UserGatewayPortFunctions
  { saveUser = embed . UserDriver.saveUser
  , saveProfile = embed . UserDriver.saveProfile
  }

createNotificationGatewayPortFunctions
  :: Member (Embed (ReaderT SqlBackend IO)) r
  => NotificationApiSettings
  -> NotificationGatewayPort.NotificationGatewayPortFunctions (Sem r)
createNotificationGatewayPortFunctions s = NotificationGatewayPort.NotificationGatewayPortFunctions
  { sendNotification = embed . NotificationDriver.postMessage s
  }
