{-# LANGUAGE OverloadedStrings #-}
module MainWindow (startMainWindow) where

import           Prelude       ()
import           Relude

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS

startMainWindow :: IO ()
startMainWindow =
  do
  putTextLn "A window should open. This has not been implemented yet."
  return ()