{-# LANGUAGE UndecidableInstances #-}
module Architecture.Polysemy.Controller.Handling where
import Data.Kind
import Control.Exception.Safe
import GHC.TypeError


-- 例外型の抽出
class ExtractException h where
  type Extracted h :: Type

instance Exception e => ExtractException (Handler m e) where
  type Extracted (Handler m e) = e

-- 型リストを集める
type family CollectExceptions (hs :: [Type]) :: [Type] where
  CollectExceptions '[] = '[]
  CollectExceptions (h ': hs) = Extracted h ': CollectExceptions hs

-- 型リストに含まれるかのチェック
type family Elem (e :: Type) (es :: [Type]) :: Bool where
  Elem e '[] = 'False
  Elem e (e ': es) = 'True
  Elem e (_ ': es) = Elem e es

-- すべての例外型がハンドラーリストに含まれているかをチェック
type family CheckHandlers (es :: [Type]) (hs :: [Type]) :: Constraint where
  CheckHandlers '[] _ = ()
  CheckHandlers (e ': es) hs =
    If (Elem e hs)
      (CheckHandlers es hs)
      (TypeError ('Text "Unhandled exception: " ':<>: 'ShowType e))

-- 型レベルのIf
type family If (cond :: Bool) (t :: Constraint) (f :: Constraint) :: Constraint where
  If 'True t f = t
  If 'False t f = f

-- 例外定義
data AuthError = InvalidCredentials | TokenExpired deriving (Show)
instance Exception AuthError

data FileError = FileNotFound FilePath | PermissionDenied FilePath deriving (Show)
instance Exception FileError

-- Eitherのアンラップ
class MonadThrow m => UnwrapEither e m a where
  unwrapEither :: e -> m a

instance (MonadThrow m, Exception e) => UnwrapEither (Either e a) m a where
  unwrapEither (Left e) = throwM e
  unwrapEither (Right a) = return a

instance MonadThrow m => UnwrapEither a m a where
  unwrapEither = return

instance {-# OVERLAPPABLE #-} (MonadThrow m, Exception e, UnwrapEither b m a) => UnwrapEither (Either e b) m a where
  unwrapEither (Left e) = throwM e
  unwrapEither (Right b) = unwrapEither b

-- メイン関数
toHandler ::
  forall a hs.
  (CheckHandlers '[AuthError, FileError] (CollectExceptions hs)) =>
  MonadThrow IO =>
  Either AuthError (Either FileError a) ->
  [Handler IO a] ->
  IO a
toHandler result handlers =
  unwrapEither result `catches` handlers

-- テスト
authHandler :: Handler IO String
authHandler = Handler $ \(e :: AuthError) -> return $ "AuthError: " <> show e

fileHandler :: Handler IO String
fileHandler = Handler $ \(e :: FileError) -> return $ "FileError: " <> show e

main :: IO ()
main = do
  print =<< toHandler (Right (Left (FileNotFound "test.txt"))) [authHandler, fileHandler]
  -- ✅ すべての例外をハンドリング → コンパイルOK

  -- これを試すと未ハンドリング例外でコンパイルエラー
  -- print =<< toHandler (Right (Left (FileNotFound "test.txt"))) [authHandler]