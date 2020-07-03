{-# LANGUAGE OverloadedStrings #-}
module Main (main, doAnalyzeIPA, doConstructIPA) where

import Prelude ()
import Relude
import System.IO (hFlush)
import Lib ( voicedIPA, devoicedIPA, describeIPA, analyzeIPA
           , englishPhonetInventoryReport
           , analyzeFeatures, showFeatures
           , constructIPA
           , showPhonet, ipaTextToPhonetListReport
           , Phonet
           )

import EnglishUSText

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
  | selection == userInput_viewEnglishPhonemeInventory = putTextLn englishPhonetInventoryReport
  | selection == userInput_makeAPhonemeVoiced          = promptForPhonemeToVoice
  | selection == userInput_makeAPhonemeUnvoiced        = promptForPhonemeToDevoice
  | selection == userInput_describeAPhonemeInEnglish   = promptForPhonemeToDescribe
  | selection == userInput_describeAPhonemeInSPE       = promptForPhonemeToCalculateSPEFeaturesFrom
  | selection == userInput_chunkIPAByPhoneme           = promptForIPATextToSplit
  | otherwise                                         = putTextLn unrecognizedSelectionMessage

doAnalyzeIPA :: Text -> Text
doAnalyzeIPA x =
  maybe noAnalysisFoundMessage showPhonet (analyzeIPA x)

doConstructIPA :: Phonet -> IO ()
doConstructIPA = putTextLn . constructIPA
