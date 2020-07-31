{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main, doAnalyzeIPA, doConstructIPA) where

import  IPA          (devoicedIPA, describeIPA, voicedIPA, ipaTextToPhonetListReport, analyzeIPAToSPE,
                      englishPhonetInventoryReport, analyzeIPA, constructIPA)



import           Lib           (Phonet,
                                showPhonet)
import           Relude
import           System.IO     (hFlush)

import           UserInterfaceText
                   ( UITextTicket( IpaTextToDivideMessage
                                 , Menu
                                 , NoAnalysisFoundMessage
                                 , UnrecognizedSelectionMessage
                                 , UnrecognizedSelectionMessage
                                 , UserInputViewEnglishPhonemeInventory
                                 , UserInputMakeAPhonemeVoiced
                                 , UserInputMakeAPhonemeUnvoiced
                                 , UserInputDescribeAPhonemeInEnglish
                                 , UserInputDescribeAPhonemeInSPE
                                 , UserInputChunkIPAByPhoneme
                                 , UserInputOpenWindow
                                 , UserInputStartServer
                                 , UserSelectedMessage
                                 , PhonemeToCalculateSPEMessage
                                 , PhonemeToDevoiceMessage
                                 , PhonemeToDescribeMessage
                                 , PhonemeToVoiceMessage
                                 , PleaseReadReadmeMessage
                                 , ProgramTerminatedNormallyMessage
                                 , Prompt
                                 )
                   , NatLanguage(English)
                   , i18n
                   , il8nGenMain)
import           MainWindow (openWindowLangSpecific)
import           MainServer (startServer)

-- | Print characters to the terminal, so that the
--   user knows that they are expected to enter
--   some text.
putPrompt :: IO ()
putPrompt =
  putText prompt
  >> hFlush stdout
  where prompt = i18n English Prompt

-- | Print a blank line on the terminal.
putBlankLine :: IO ()
putBlankLine = putTextLn ""

-- | Print a specified number of blank lines on the terminal.
putBlankLines :: Int -- ^ number of lines
              -> IO ()
putBlankLines n = replicateM_ n putBlankLine

promptForTextAndApply :: (Text -> Text) -> Text -> IO ()
promptForTextAndApply func instructions =
    putTextLn instructions
    >> putPrompt
    >> interact func

-- | Given a function, asks the user for input,
--   apply the function to the user input,
--   and prints the result of the application
--   of the function.
interact :: (Text -> Text) -- ^ the function to apply to the user input
         -> IO ()
interact func =
  getLine
  >>= \userInput -> putTextLn (func userInput)

-- | Ask the user for a phoneme.
--   The user inputs a phoneme.
--   Take the phoneme, and print the phoneme that is as similar
--   to the original phoneme, but unvoiced.
promptForPhonemeToDevoice :: NatLanguage -> IO ()
promptForPhonemeToDevoice lang =
  promptForTextAndApply devoicedIPA phonemeToDevoiceMessage
  where phonemeToDevoiceMessage = i18n lang PhonemeToDevoiceMessage

-- | Ask the user for a phoneme.
--   The user inputs a phoneme.
--   Take the phoneme, and print the phoneme that is as similar
--   to the original phoneme, but voiced.
promptForPhonemeToVoice :: NatLanguage -> IO ()
promptForPhonemeToVoice lang =
  promptForTextAndApply voicedIPA phonemeToVoiceMessage
  where phonemeToVoiceMessage = i18n lang PhonemeToVoiceMessage

-- | Ask the user for a phoneme.
--   The user inputs a phoneme.
--   Take the phoneme, and print the
--   description of the phoneme.
promptForPhonemeToDescribe :: NatLanguage -> IO ()
promptForPhonemeToDescribe lang =
  promptForTextAndApply describeIPA phonemeToDescribeMessage
  where phonemeToDescribeMessage = i18n lang PhonemeToDescribeMessage

-- | Ask the user for a phoneme.
--   The user inputs a phoneme.
--   Take the phoneme, calculate what
--   the features of it are (according to Sound Patterns of English)
--   and print those features.
promptForPhonemeToCalculateSPEFeaturesFrom :: NatLanguage -> IO ()
promptForPhonemeToCalculateSPEFeaturesFrom lang =
  promptForTextAndApply analyzeIPAToSPE phonemeToCalculateSPEMessage
  where phonemeToCalculateSPEMessage = i18n lang PhonemeToCalculateSPEMessage

-- | Ask the user for IPA text which may contain multiple IPA characters
--   and phonemes. Take that input, and print each phoneme on
--   separate lines.
promptForIPATextToSplit :: NatLanguage -> IO ()
promptForIPATextToSplit lang =
  promptForTextAndApply ipaTextToPhonetListReport ipaTextToDivideMessage
  where ipaTextToDivideMessage = i18n lang IpaTextToDivideMessage

-- | This function is where the program starts running.
main :: IO ()
main =
  putTextLn pleaseReadReadmeMessage
  >>  putText menu
  >>  putPrompt
  >>  getLine
  >>= acknowledgeAndRespond English
  >>  putBlankLine
  >>  putTextLn programTerminatedNormallyMessage
  >>  putBlankLines 2
  where pleaseReadReadmeMessage = i18n English PleaseReadReadmeMessage
        menu = i18n English Menu
        programTerminatedNormallyMessage = i18n English ProgramTerminatedNormallyMessage

-- | Tell the user what they selected. This is necessary
--   for better user-friendliness.
acknowledgeAndRespond :: NatLanguage
                      -> Text -- ^ what the user typed in
                      -> IO ()
acknowledgeAndRespond lang selection =
  putTextLn (unwords [userSelectedMessage, selection])
  >> putBlankLine
  >> respondToSelection lang selection
  where userSelectedMessage = i18n lang UserSelectedMessage

-- | Start the appropriate action according to what the user already selected.
respondToSelection :: NatLanguage
                   -> Text -- ^ the text the user put in after being shown the menu
                   -> IO ()
respondToSelection lang selection
  | selection == userInputViewEnglishPhonemeInventory = putTextLn englishPhonetInventoryReport
  | selection == userInputMakeAPhonemeVoiced          = promptForPhonemeToVoice lang
  | selection == userInputMakeAPhonemeUnvoiced        = promptForPhonemeToDevoice lang
  | selection == userInputDescribeAPhonemeInEnglish   = promptForPhonemeToDescribe lang
  | selection == userInputDescribeAPhonemeInSPE       = promptForPhonemeToCalculateSPEFeaturesFrom lang
  | selection == userInputChunkIPAByPhoneme           = promptForIPATextToSplit lang
  | selection == userInputOpenWindow                  = openWindowLangSpecific lang
  | selection == userInputStartServer                 = startServer
  | selection == "00"                                 = il8nGenMain
  | otherwise                                         = putTextLn unrecognizedSelectionMessage
  where
        unrecognizedSelectionMessage         = i18n lang UnrecognizedSelectionMessage
        userInputViewEnglishPhonemeInventory = i18n lang UserInputViewEnglishPhonemeInventory
        userInputMakeAPhonemeVoiced          = i18n lang UserInputMakeAPhonemeVoiced
        userInputMakeAPhonemeUnvoiced        = i18n lang UserInputMakeAPhonemeUnvoiced
        userInputDescribeAPhonemeInEnglish   = i18n lang UserInputDescribeAPhonemeInEnglish
        userInputDescribeAPhonemeInSPE       = i18n lang UserInputDescribeAPhonemeInSPE
        userInputChunkIPAByPhoneme           = i18n lang UserInputChunkIPAByPhoneme
        userInputOpenWindow                  = i18n lang UserInputOpenWindow
        userInputStartServer                 = i18n lang UserInputStartServer

-- | Given an IPA transcription, return the
--   name of the phoneme that IPA transcription describes.
--   If the IPA transcription could not be named,
--   return a message saying so.
doAnalyzeIPA :: NatLanguage
             -> Text -- ^ text from the International Phonetic Alphabet
             -> Text -- ^ the name of the phoneme,
                     -- or a message saying the phoneme was not recognized
doAnalyzeIPA lang x =
  maybe noAnalysisFoundMessage showPhonet (analyzeIPA x)
  where noAnalysisFoundMessage = i18n lang NoAnalysisFoundMessage

-- | Given a set of phoneme properties that describe a phoneme, like:
--   voiced velar fricative pulmonic egressive,
--   print the IPA transcription of it to the terminal.
doConstructIPA :: Phonet  -- ^ A phonete to get the transcription of
               -> IO ()
doConstructIPA = putTextLn . constructIPA
