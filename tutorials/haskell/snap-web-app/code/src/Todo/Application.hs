{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Todo.Application where

------------------------------------------------------------------------------
import           Control.Lens
import           Database.HDBC         ()
import           Database.HDBC.Sqlite3
import           Snap.Snaplet
import           Snap.Snaplet.Heist

------------------------------------------------------------------------------
data App = App
  { _heist :: Snaplet (Heist App)
  , _conn  :: Connection
  }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App
