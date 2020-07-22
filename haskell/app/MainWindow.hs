{-# LANGUAGE OverloadedStrings #-}
module MainWindow (startMainWindow) where

import           Prelude       (read)
import           Relude

import qualified Data.Text as T

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS

import           Lib           (voicedIPA)
import EnglishUSText (application_title, makeAPhonemeVoicedUIText)

buttonCb :: Ref Input -> IO ()
buttonCb i' = do
  l' <- getValue i'
  setLabel i' (voicedIPA l')

ui :: IO ()
ui = do
 window <- windowNew
           (Size (Width 915) (Height 570))
           Nothing
           (Just application_title)
 begin window
 b' <- buttonNew
        (Rectangle (Position (X 10) (Y 80)) (Size (Width 125) (Height 30)))
        (Just makeAPhonemeVoicedUIText)
 setLabelsize b' (FontSize 10)

 i' <- inputNew
        (Rectangle (Position (X 50) (Y 30)) (Size (Width 30) (Height 30)))
        (Just "")
        (Just FlNormalInput)

 setCallback i' buttonCb


 end window
 showWidget window

startMainWindow :: IO ()
startMainWindow = ui >> FL.run >> FL.flush

replMain :: IO ()
replMain = ui >> FL.replRun