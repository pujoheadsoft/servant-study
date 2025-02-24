module Architecture.Polysemy.Controller.UserController3 where

import Control.Monad.Reader (ReaderT (runReaderT, ReaderT))
import qualified Control.Monad.Reader as R
import Database.Persist.Postgresql (SqlBackend, runSqlPool, Sql)
import Polysemy (makeSem, Members, Embed (Embed), Sem, interpret, embed, Member, runM, subsume, reinterpret, run)
import Polysemy.Reader (Reader, ask, runReader, inputToReader)
import Database.Persist.Sql (ConnectionPool)
import Data.Effect.Unlift (UnliftIO)
import Control.Monad.IO.Class (MonadIO)

data DB m a where
  RunDB :: ReaderT SqlBackend IO a -> DB m a

makeSem ''DB

runDBPool :: Members '[Reader SqlBackend, Embed IO] r => Sem (DB ': r) a -> Sem r a
runDBPool = interpret $ \case
  RunDB action -> do
    backend <- ask
    embed $ runReaderT action backend

yyy :: Sem '[DB] a -> ReaderT SqlBackend IO a
yyy sem = runM $ reinterpret (\(RunDB action) -> embed action) sem

runPoolSql :: Member (Embed IO) r => ConnectionPool -> Sem '[DB] a -> Sem r a
runPoolSql pool sem = do
  let
    program = yyy sem
  embed $ runSqlPool program pool

main2 :: IO ()
main2 = do
  let 
    pool :: ConnectionPool
    pool = undefined
  _ <- runM . runPoolSql pool $ do
    runDB $ pure "a"
    runDB $ pure ()
    runDB $ pure 10
  pure ()
