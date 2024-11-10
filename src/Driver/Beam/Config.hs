module Driver.Beam.Config where
import Database.Beam.Postgres (Connection, connect, defaultConnectInfo, ConnectInfo (..))

connection :: IO Connection
connection = connect defaultConnectInfo
  { connectUser = "admin"
  , connectPassword = "admin"
  , connectDatabase = "user"
  }