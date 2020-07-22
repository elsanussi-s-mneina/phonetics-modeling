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
                      getFeaturesOfPhonemeUIText, splitTranscriptionUIText, resultHeader, voicedPhonemeHeader,
                      unvoicedPhonemeHeader, englishPhonemeInventoryHeader, featuresHeader, phonemeDescriptionHeader,
                      phonemesSplitHeader, inputHeader)

voicePhonemeCallback :: Ref Input -> Ref TextBuffer -> Ref TextDisplay -> Ref Button ->  IO ()
voicePhonemeCallback inputBox outputBox textDisplay _ = do
  ipaText <- getValue inputBox
  setText outputBox (voicedPhonemeHeader <> ":\n\n" <> voicedIPA ipaText)

genericInteractCallback :: (Text -> Text) -> Ref Input -> Ref TextBuffer -> Ref Button -> IO ()
genericInteractCallback func inputBox textDisplay _ = do
  ipaText <- getValue inputBox
  setText textDisplay (func ipaText)


devoicePhonemeCallback :: Ref Input -> Ref TextBuffer -> Ref TextDisplay -> Ref Button ->  IO ()
devoicePhonemeCallback inputBox outputBox textDisplay _ = do
  ipaText <- getValue inputBox
  setText outputBox (unvoicedPhonemeHeader <> ":\n\n" <> devoicedIPA ipaText)

englishPhoneteInventoryCallback :: Ref Input -> Ref TextBuffer -> Ref TextDisplay -> Ref Button ->  IO ()
englishPhoneteInventoryCallback inputBox outputBox textDisplay _ = do
  ipaText <- getValue inputBox
  setText outputBox (englishPhonemeInventoryHeader <> ":\n\n" <> englishPhonetInventoryReport)

describePhonemeCallback :: Ref Input -> Ref TextBuffer -> Ref TextDisplay -> Ref Button ->  IO ()
describePhonemeCallback inputBox outputBox textDisplay _ = do
  ipaText <- getValue inputBox
  setText outputBox (phonemeDescriptionHeader <> ":\n\n" <> describeIPA ipaText)


featurizePhonemeCallback :: Ref Input -> Ref TextBuffer -> Ref TextDisplay -> Ref Button ->  IO ()
featurizePhonemeCallback inputBox outputBox textDisplay _ = do
  ipaText <- getValue inputBox
  setText outputBox (featuresHeader <> ":\n\n" <> analyzeIPAToSPE ipaText)


splitTranscriptionCallback :: Ref Input -> Ref TextBuffer -> Ref TextDisplay -> Ref Button ->  IO ()
splitTranscriptionCallback inputBox outputBox textDisplay _ = do
  ipaText <- getValue inputBox
  setText outputBox (phonemesSplitHeader <> ":\n\n" <> ipaTextToPhonetListReport ipaText)


ui :: IO ()
ui = do
 window <- windowNew
           (Size (Width 915) (Height 570))
           Nothing
           (Just application_title)
 begin window
 voicePhonemeButton <- buttonNew
        (Rectangle (Position (X 10) (Y 140)) (Size (Width 275) (Height 30)))
        (Just makeAPhonemeVoicedUIText)
 setLabelsize voicePhonemeButton (FontSize 10)

 devoicePhonemeButton <- buttonNew
        (Rectangle (Position (X 10) (Y 180)) (Size (Width 275) (Height 30)))
        (Just makeAPhonemeUnvoicedUIText)
 setLabelsize devoicePhonemeButton (FontSize 10)

 describePhonemeButton <- buttonNew
        (Rectangle (Position (X 10) (Y 220)) (Size (Width 275) (Height 30)))
        (Just describePhonemeUIText)
 setLabelsize describePhonemeButton (FontSize 10)

 featurizePhonemeButton <- buttonNew
        (Rectangle (Position (X 10) (Y 260)) (Size (Width 275) (Height 30)))
        (Just getFeaturesOfPhonemeUIText)
 setLabelsize featurizePhonemeButton (FontSize 10)

 showPhonemeInventoryButton <- buttonNew
        (Rectangle (Position (X 10) (Y 30)) (Size (Width 275) (Height 30)))
        (Just showPhonemeInventoryUIText)
 setLabelsize showPhonemeInventoryButton (FontSize 10)

 splitTranscriptionButton <- buttonNew
        (Rectangle (Position (X 10) (Y 300)) (Size (Width 275) (Height 30)))
        (Just splitTranscriptionUIText)
 setLabelsize splitTranscriptionButton (FontSize 10)

 inputBox <- inputNew
        (Rectangle (Position (X 50) (Y 100)) (Size (Width 90) (Height 30)))
        (Just "")
        (Just FlNormalInput)
 setLabel inputBox inputHeader


 textDisplay <- textDisplayNew
     (Rectangle (Position (X 350) (Y 30)) (Size (Width 505) (Height 400)))
     Nothing

 setLabel textDisplay resultHeader
 textBuffer <- textBufferNew Nothing Nothing
 setBuffer textDisplay (Just textBuffer)

 setCallback voicePhonemeButton (voicePhonemeCallback inputBox textBuffer textDisplay)
 setCallback devoicePhonemeButton (devoicePhonemeCallback inputBox textBuffer textDisplay)
 setCallback describePhonemeButton (describePhonemeCallback inputBox textBuffer textDisplay)
 setCallback featurizePhonemeButton (featurizePhonemeCallback inputBox textBuffer textDisplay)
 setCallback splitTranscriptionButton (splitTranscriptionCallback inputBox textBuffer textDisplay)
 setCallback showPhonemeInventoryButton (englishPhoneteInventoryCallback inputBox textBuffer textDisplay)

 end window
 showWidget window

startMainWindow :: IO ()
startMainWindow = ui >> FL.run >> FL.flush

replMain :: IO ()
replMain = ui >> FL.replRun