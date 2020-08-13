{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module MainWindow where

import           Relude

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS

import  IPA          (devoicedIPA, describeIPA, voicedIPA, ipaTextToPhonetListReport, analyzeIPAToSPE,
                      englishPhonetInventoryReport)

import UserInterfaceText
  (i18n
  , UITextTicket( ApplicationTitle
                , ShowPhonemeInventoryUIText
                , MakeAPhonemeVoicedUIText
                , MakeAPhonemeUnvoicedUIText
                , DescribePhonemeUIText
                , GetFeaturesOfPhonemeUIText
                , SplitTranscriptionUIText
                , ResultHeader
                , VoicedPhonemeHeader
                , UnvoicedPhonemeHeader
                , EnglishPhonemeInventoryHeader
                , FeaturesHeader
                , PhonemeDescriptionHeader
                , PhonemesSplitHeader
                , InputHeader
                )
  , NatLanguage(English))

headerThenContent :: Text -> Text -> Text
headerThenContent header content = header <> ":\n\n" <> content


voicePhonemeCallback :: NatLanguage -> Ref Input -> Ref TextBuffer -> Ref Button ->  IO ()
voicePhonemeCallback lang inputBox outputBox _ = do
  ipaText <- getValue inputBox
  setText outputBox (headerThenContent voicedPhonemeHeader (voicedIPA ipaText))
  where uiTxt = i18n lang
        voicedPhonemeHeader = uiTxt VoicedPhonemeHeader

devoicePhonemeCallback :: NatLanguage -> Ref Input -> Ref TextBuffer -> Ref Button ->  IO ()
devoicePhonemeCallback lang inputBox outputBox _ = do
  ipaText <- getValue inputBox
  setText outputBox (headerThenContent unvoicedPhonemeHeader (devoicedIPA ipaText))
  where uiTxt = i18n lang
        unvoicedPhonemeHeader = uiTxt UnvoicedPhonemeHeader

englishPhoneteInventoryCallback :: NatLanguage -> Ref TextBuffer -> Ref Button ->  IO ()
englishPhoneteInventoryCallback lang outputBox _ = do
  setText outputBox (headerThenContent englishPhonemeInventoryHeader englishPhonetInventoryReport)
  where uiTxt = i18n lang
        englishPhonemeInventoryHeader = uiTxt EnglishPhonemeInventoryHeader

describePhonemeCallback :: NatLanguage -> Ref Input -> Ref TextBuffer -> Ref Button ->  IO ()
describePhonemeCallback lang inputBox outputBox _ = do
  ipaText <- getValue inputBox
  setText outputBox (headerThenContent phonemeDescriptionHeader (describeIPA ipaText))
  where uiTxt = i18n lang
        phonemeDescriptionHeader = uiTxt PhonemeDescriptionHeader


featurizePhonemeCallback :: NatLanguage -> Ref Input -> Ref TextBuffer -> Ref Button ->  IO ()
featurizePhonemeCallback lang inputBox outputBox _ = do
  ipaText <- getValue inputBox
  setText outputBox (headerThenContent featuresHeader (analyzeIPAToSPE ipaText))
  where uiTxt = i18n lang
        featuresHeader = uiTxt FeaturesHeader


splitTranscriptionCallback :: NatLanguage -> Ref Input -> Ref TextBuffer -> Ref Button ->  IO ()
splitTranscriptionCallback lang inputBox outputBox _ = do
  ipaText <- getValue inputBox
  setText outputBox (headerThenContent phonemesSplitHeader (ipaTextToPhonetListReport ipaText))
  where uiTxt = i18n lang
        phonemesSplitHeader = uiTxt PhonemesSplitHeader

ui :: NatLanguage -> IO ()
ui lang = do
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

  setLabel outputToUserWidget (i18n lang ResultHeader)

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


  setCallback voicePhonemeButton (voicePhonemeCallback lang inputBox outputToUser)
  setCallback devoicePhonemeButton (devoicePhonemeCallback lang inputBox outputToUser)
  setCallback describePhonemeButton (describePhonemeCallback lang inputBox outputToUser)
  setCallback featurizePhonemeButton (featurizePhonemeCallback lang inputBox outputToUser)
  setCallback splitTranscriptionButton (splitTranscriptionCallback lang inputBox outputToUser)
  setCallback showInventoryButton (englishPhoneteInventoryCallback lang outputToUser)

  end window
  showWidget window
  where
    uiTxt = i18n lang
    inputHeader =  uiTxt InputHeader
    applicationTitle = uiTxt ApplicationTitle
    splitTranscriptionUIText = uiTxt SplitTranscriptionUIText
    getFeaturesOfPhonemeUIText = uiTxt GetFeaturesOfPhonemeUIText
    describePhonemeUIText = uiTxt DescribePhonemeUIText
    makeAPhonemeUnvoicedUIText = uiTxt MakeAPhonemeUnvoicedUIText
    makeAPhonemeVoicedUIText = uiTxt MakeAPhonemeVoicedUIText
    showPhonemeInventoryUIText = uiTxt ShowPhonemeInventoryUIText

openWindow :: IO ()
openWindow = openWindowLangSpecific English

openWindowLangSpecific :: NatLanguage -> IO ()
openWindowLangSpecific lang = ui lang >> FL.run >> FL.flush


replMain :: IO ()
replMain = replMainLangSpecific English

replMainLangSpecific :: NatLanguage -> IO ()
replMainLangSpecific natLang = ui natLang >> FL.replRun