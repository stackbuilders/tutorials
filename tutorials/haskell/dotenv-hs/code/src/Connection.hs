module Connection where

import Database.PostgreSQL.Simple

getConnection :: ConnectInfo -> IO Connection
getConnection = connect
