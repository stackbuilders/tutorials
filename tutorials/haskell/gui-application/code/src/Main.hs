module Main (main) where

import Control.Monad
import Graphics.UI.Gtk
import Control.Monad.IO.Class

main :: IO ()
main = do
  void initGUI
  window <- windowNew
  window `on` deleteEvent $ liftIO mainQuit >> return False
  -- manipulate the window
  widgetShowAll window
  mainGUI
