{-# LANGUAGE OverloadedStrings #-}
module MainWindow (startMainWindow) where

import           Prelude       (read)
import           Relude

import qualified Data.Text as T

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS

import  Lib          (analyzeIPAToSPE, describeIPA, devoicedIPA, englishPhonetInventoryReport,
                      ipaTextToPhonetListReport, voicedIPA)
import EnglishUSText (application_title, showPhonemeInventoryUIText, makeAPhonemeVoicedUIText,
                      quitUIText, makeAPhonemeUnvoicedUIText, describePhonemeUIText,
                      getFeaturesOfPhonemeUIText, splitTranscriptionUIText)

voicePhonemeCallback :: Ref Input -> Ref TextBuffer -> Ref Button ->  IO ()
voicePhonemeCallback inputBox outputBox _ = do
  ipaText <- getValue inputBox
  setText outputBox (voicedIPA ipaText)


genericInteractCallback :: (Text -> Text) -> Ref Input -> Ref TextBuffer -> Ref Button -> IO ()
genericInteractCallback func inputBox textDisplay _ = do
  ipaText <- getValue inputBox
  setText textDisplay (func ipaText)


devoicePhonemeCallback :: Ref Input -> Ref TextBuffer -> Ref Button ->  IO ()
devoicePhonemeCallback = genericInteractCallback devoicedIPA

englishPhoneteInventoryCallback :: Ref Input -> Ref TextBuffer -> Ref Button ->  IO ()
englishPhoneteInventoryCallback = genericInteractCallback (\_ -> englishPhonetInventoryReport)

describePhonemeCallback :: Ref Input -> Ref TextBuffer -> Ref Button ->  IO ()
describePhonemeCallback = genericInteractCallback describeIPA

featurizePhonemeCallback :: Ref Input -> Ref TextBuffer -> Ref Button ->  IO ()
featurizePhonemeCallback = genericInteractCallback analyzeIPAToSPE

splitTranscriptionCallback :: Ref Input -> Ref TextBuffer -> Ref Button ->  IO ()
splitTranscriptionCallback = genericInteractCallback ipaTextToPhonetListReport


ui :: IO ()
ui = do
 window <- windowNew
           (Size (Width 915) (Height 570))
           Nothing
           (Just application_title)
 begin window
 voicePhonemeButton <- buttonNew
        (Rectangle (Position (X 10) (Y 80)) (Size (Width 275) (Height 30)))
        (Just makeAPhonemeVoicedUIText)
 setLabelsize voicePhonemeButton (FontSize 10)

 devoicePhonemeButton <- buttonNew
        (Rectangle (Position (X 10) (Y 120)) (Size (Width 275) (Height 30)))
        (Just makeAPhonemeUnvoicedUIText)
 setLabelsize devoicePhonemeButton (FontSize 10)

 describePhonemeButton <- buttonNew
        (Rectangle (Position (X 10) (Y 160)) (Size (Width 275) (Height 30)))
        (Just describePhonemeUIText)
 setLabelsize describePhonemeButton (FontSize 10)

 featurizePhonemeButton <- buttonNew
        (Rectangle (Position (X 10) (Y 200)) (Size (Width 275) (Height 30)))
        (Just getFeaturesOfPhonemeUIText)
 setLabelsize featurizePhonemeButton (FontSize 10)

 showPhonemeInventoryButton <- buttonNew
        (Rectangle (Position (X 10) (Y 200)) (Size (Width 275) (Height 30)))
        (Just showPhonemeInventoryUIText)
 setLabelsize showPhonemeInventoryButton (FontSize 10)

 splitTranscriptionButton <- buttonNew
        (Rectangle (Position (X 10) (Y 240)) (Size (Width 275) (Height 30)))
        (Just splitTranscriptionUIText)
 setLabelsize splitTranscriptionButton (FontSize 10)

 inputBox <- inputNew
        (Rectangle (Position (X 50) (Y 30)) (Size (Width 30) (Height 30)))
        (Just "")
        (Just FlNormalInput)

 textDisplay <- textDisplayNew
     (Rectangle (Position (X 350) (Y 30)) (Size (Width 505) (Height 400)))
     Nothing

 setLabel textDisplay "Result:"
 textBuffer <- textBufferNew Nothing Nothing
 setBuffer textDisplay (Just textBuffer)

 setCallback voicePhonemeButton (voicePhonemeCallback inputBox textBuffer)
 setCallback devoicePhonemeButton (devoicePhonemeCallback inputBox textBuffer)
 setCallback describePhonemeButton (describePhonemeCallback inputBox textBuffer)
 setCallback featurizePhonemeButton (featurizePhonemeCallback inputBox textBuffer)
 setCallback splitTranscriptionButton (splitTranscriptionCallback inputBox textBuffer)
 setCallback showPhonemeInventoryButton (englishPhoneteInventoryCallback inputBox textBuffer)

 end window
 showWidget window

startMainWindow :: IO ()
startMainWindow = ui >> FL.run >> FL.flush

replMain :: IO ()
replMain = ui >> FL.replRun