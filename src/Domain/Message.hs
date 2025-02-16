module Domain.Message where
import Control.Lens (makeFieldsId)
import Data.Text (Text)

newtype Message = Message { value :: Text }
  deriving newtype (Eq, Show)
makeFieldsId ''Message