{-# LANGUAGE OverloadedStrings #-}
module MainWindow (openWindow) where

import           Prelude       (read)
import           Relude

import qualified Data.Text as T

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS

import  IPA          (devoicedIPA, describeIPA, voicedIPA, ipaTextToPhonetListReport, analyzeIPAToSPE,
                      englishPhonetInventoryReport)

import EnglishUSText (applicationTitle, showPhonemeInventoryUIText, makeAPhonemeVoicedUIText,
                      quitUIText, makeAPhonemeUnvoicedUIText, describePhonemeUIText,
                      getFeaturesOfPhonemeUIText, splitTranscriptionUIText, resultHeader, voicedPhonemeHeader,
                      unvoicedPhonemeHeader, englishPhonemeInventoryHeader, featuresHeader, phonemeDescriptionHeader,
                      phonemesSplitHeader, inputHeader)

headerThenContent :: Text -> Text -> Text
headerThenContent header content = header <> ":\n\n" <> content


voicePhonemeCallback :: Ref Input -> Ref TextBuffer -> Ref TextDisplay -> Ref Button ->  IO ()
voicePhonemeCallback inputBox outputBox outputToUserWidget _ = do
  ipaText <- getValue inputBox
  setText outputBox (headerThenContent voicedPhonemeHeader (voicedIPA ipaText))

devoicePhonemeCallback :: Ref Input -> Ref TextBuffer -> Ref TextDisplay -> Ref Button ->  IO ()
devoicePhonemeCallback inputBox outputBox outputToUserWidget _ = do
  ipaText <- getValue inputBox
  setText outputBox (headerThenContent unvoicedPhonemeHeader (devoicedIPA ipaText))

englishPhoneteInventoryCallback :: Ref Input -> Ref TextBuffer -> Ref TextDisplay -> Ref Button ->  IO ()
englishPhoneteInventoryCallback inputBox outputBox outputToUserWidget _ = do
  ipaText <- getValue inputBox
  setText outputBox (headerThenContent englishPhonemeInventoryHeader englishPhonetInventoryReport)

describePhonemeCallback :: Ref Input -> Ref TextBuffer -> Ref TextDisplay -> Ref Button ->  IO ()
describePhonemeCallback inputBox outputBox outputToUserWidget _ = do
  ipaText <- getValue inputBox
  setText outputBox (headerThenContent phonemeDescriptionHeader (describeIPA ipaText))


featurizePhonemeCallback :: Ref Input -> Ref TextBuffer -> Ref TextDisplay -> Ref Button ->  IO ()
featurizePhonemeCallback inputBox outputBox outputToUserWidget _ = do
  ipaText <- getValue inputBox
  setText outputBox (headerThenContent featuresHeader (analyzeIPAToSPE ipaText))


splitTranscriptionCallback :: Ref Input -> Ref TextBuffer -> Ref TextDisplay -> Ref Button ->  IO ()
splitTranscriptionCallback inputBox outputBox outputToUserWidget _ = do
  ipaText <- getValue inputBox
  setText outputBox (headerThenContent phonemesSplitHeader (ipaTextToPhonetListReport ipaText))


ui :: IO ()
ui = do
  window <- windowNew
            (Size (Width 915) (Height 570))
            Nothing
            (Just applicationTitle)
  begin window

  outputToUser <- textBufferNew Nothing Nothing

  outputToUserWidget <- textDisplayNew
      (Rectangle (Position (X 350) (Y 30)) (Size (Width 505) (Height 400)))
      Nothing
  setBuffer outputToUserWidget (Just outputToUser)

  setLabel outputToUserWidget resultHeader

  inputBox <- inputNew
         (Rectangle (Position (X 50) (Y 100)) (Size (Width 90) (Height 30)))
         Nothing
         (Just FlNormalInput)
  setLabel inputBox inputHeader

  showInventoryButton <- buttonNew
         (Rectangle (Position (X 10) (Y 30)) (Size (Width 275) (Height 30)))
         (Just showPhonemeInventoryUIText)
  setLabelsize showInventoryButton (FontSize 10)

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

  splitTranscriptionButton <- buttonNew
         (Rectangle (Position (X 10) (Y 300)) (Size (Width 275) (Height 30)))
         (Just splitTranscriptionUIText)
  setLabelsize splitTranscriptionButton (FontSize 10)


  setCallback voicePhonemeButton (voicePhonemeCallback inputBox outputToUser outputToUserWidget)
  setCallback devoicePhonemeButton (devoicePhonemeCallback inputBox outputToUser outputToUserWidget)
  setCallback describePhonemeButton (describePhonemeCallback inputBox outputToUser outputToUserWidget)
  setCallback featurizePhonemeButton (featurizePhonemeCallback inputBox outputToUser outputToUserWidget)
  setCallback splitTranscriptionButton (splitTranscriptionCallback inputBox outputToUser outputToUserWidget)
  setCallback showInventoryButton (englishPhoneteInventoryCallback inputBox outputToUser outputToUserWidget)

  end window
  showWidget window

openWindow :: IO ()
openWindow = ui >> FL.run >> FL.flush

replMain :: IO ()
replMain = ui >> FL.replRun