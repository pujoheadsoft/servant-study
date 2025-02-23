module Architecture.Polysemy.Controller.UserController3 where

import Control.Monad.Reader (ReaderT (runReaderT, ReaderT))
import Database.Persist.Postgresql (SqlBackend, runSqlPool)
import Polysemy (makeSem, Members, Embed (Embed), Sem, interpret, embed, Member, runM, subsume, reinterpret, run)
import Polysemy.Reader (Reader, ask, runReader)
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

runPoolSql :: Members '[Reader SqlBackend, Embed IO] r => ConnectionPool -> Sem (DB ': r) a -> IO a
runPoolSql pool sem = do
  let
    -- x = reinterpret (\(RunDB action) -> do
    --         backend <- ask
    --         embed $ runReaderT action backend
    --       ) sem
    y = runDBPool sem
    backend :: SqlBackend
    backend = undefined
    
    r :: Sem (Reader SqlBackend : r) a
    r = undefined

    ef :: MonadIO m => Sem '[Embed (ReaderT SqlBackend m)] a
    ef = runReader backend r

  let
    program :: MonadIO m => ReaderT SqlBackend m a
    program = runM ef
  --z <- runM y

  runSqlPool program pool





-- main :: IO ()
-- main = do
--   pool <- createSqlPool
--   runPoolSql pool $ do
--     runDB $ insert_ $ MyEntity "Test" -- DBエフェクトの処理