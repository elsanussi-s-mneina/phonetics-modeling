{-# LANGUAGE OverloadedStrings #-}
module Main (main, doAnalyzeIPA, doConstructIPA) where

import           Lib           (Phonet, analyzeFeatures, analyzeIPA,
                                constructIPA, describeIPA, devoicedIPA,
                                englishPhonetInventoryReport,
                                ipaTextToPhonetListReport, showFeatures,
                                showPhonet, voicedIPA)
import           Prelude       ()
import           Relude
import           System.IO     (hFlush)

import           EnglishUSText

-- | Print characters to the terminal, so that the
--   user knows that they are expected to enter
--   some text.
putPrompt :: IO ()
putPrompt =
  putText prompt
  >> hFlush stdout

analyzeIPAToSPE :: Text -> Text
analyzeIPAToSPE ipaText =
  maybe sorryUnableToCalculate (showFeatures . analyzeFeatures) (analyzeIPA ipaText)

putBlankLine :: IO ()
putBlankLine = putTextLn ""

putBlankLines :: Int -> IO ()
putBlankLines n = replicateM_ n putBlankLine

promptForTextAndApply :: (Text -> Text) -> Text -> IO ()
promptForTextAndApply func instructions =
    putTextLn instructions
    >> putPrompt
    >> interact func

interact :: (Text -> Text) -> IO ()
interact func =
  getLine
  >>= \userInput -> putTextLn (func userInput)

promptForPhonemeToDevoice :: IO ()
promptForPhonemeToDevoice =
  promptForTextAndApply devoicedIPA phonemeToDevoiceMessage

promptForPhonemeToVoice :: IO ()
promptForPhonemeToVoice =
  promptForTextAndApply voicedIPA phonemeToVoiceMessage

promptForPhonemeToDescribe :: IO ()
promptForPhonemeToDescribe =
  promptForTextAndApply describeIPA phonemeToDescribeMessage

promptForPhonemeToCalculateSPEFeaturesFrom :: IO ()
promptForPhonemeToCalculateSPEFeaturesFrom =
  promptForTextAndApply analyzeIPAToSPE phonemeToCalculateSPEMessage

promptForIPATextToSplit :: IO ()
promptForIPATextToSplit =
  promptForTextAndApply ipaTextToPhonetListReport ipaTextToDivideMessage

main :: IO ()
main =
  putTextLn pleaseReadReadmeMessage
  >>  putText menu
  >>  putPrompt
  >>  getLine
  >>= handleSelection
  >>  putBlankLine
  >>  putTextLn programTerminatedNormallyMessage
  >>  putBlankLines 2

handleSelection :: Text -> IO ()
handleSelection selection =
  putTextLn (unwords [userSelectedMessage, selection])
  >> putBlankLine
  >> respondToSelection selection

respondToSelection :: Text -> IO ()
respondToSelection selection
  | selection == userInputViewEnglishPhonemeInventory = putTextLn englishPhonetInventoryReport
  | selection == userInputMakeAPhonemeVoiced          = promptForPhonemeToVoice
  | selection == userInputMakeAPhonemeUnvoiced        = promptForPhonemeToDevoice
  | selection == userInputDescribeAPhonemeInEnglish   = promptForPhonemeToDescribe
  | selection == userInputDescribeAPhonemeInSPE       = promptForPhonemeToCalculateSPEFeaturesFrom
  | selection == userInputChunkIPAByPhoneme           = promptForIPATextToSplit
  | otherwise                                         = putTextLn unrecognizedSelectionMessage

doAnalyzeIPA :: Text -> Text
doAnalyzeIPA x =
  maybe noAnalysisFoundMessage showPhonet (analyzeIPA x)

doConstructIPA :: Phonet -> IO ()
doConstructIPA = putTextLn . constructIPA
