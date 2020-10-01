module Main (main, doAnalyzeIPA, doConstructIPA) where

import  IPA          (devoicedIPA, describeIPA, voicedIPA, ipaTextToPhonetListReport, analyzeIPAToSPE,
                      arabicPhonetInventoryReport, plainsCreePhonetInventoryReport, englishPhonetInventoryReport,
                      irishPhonetInventoryReport, analyzeIPA, constructIPA)



import           Lib           (Phonet)
import ShowFunctions (showPhonet)

import Prelude (IO, Int, (.), maybe, otherwise, (==), (>>), (>>=))
import           System.IO     (hFlush, stdout)
import Control.Monad (replicateM_)
import Data.Text (Text, unwords)
import Data.Text.IO (getLine, putStr, putStrLn)
import qualified Data.Text as T
import DefaultLanguageText

-- | Print characters to the terminal, so that the
--   user knows that they are expected to enter
--   some text.
putPrompt :: IO ()
putPrompt =
  putStr prompt
  >> hFlush stdout

-- | Print a blank line on the terminal.
putBlankLine :: IO ()
putBlankLine = putStrLn T.empty

-- | Print a specified number of blank lines on the terminal.
putBlankLines :: Int -- ^ number of lines
              -> IO ()
putBlankLines n = replicateM_ n putBlankLine

promptForTextAndApply :: (Text -> Text) -> Text -> IO ()
promptForTextAndApply func instructions =
    putStrLn instructions
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
  >>= \userInput -> putStrLn (func userInput)

-- | Ask the user for a phoneme.
--   The user inputs a phoneme.
--   Take the phoneme, and print the phoneme that is as similar
--   to the original phoneme, but unvoiced.
promptForPhonemeToDevoice :: IO ()
promptForPhonemeToDevoice =
  promptForTextAndApply devoicedIPA phonemeToDevoiceMessage

-- | Ask the user for a phoneme.
--   The user inputs a phoneme.
--   Take the phoneme, and print the phoneme that is as similar
--   to the original phoneme, but voiced.
promptForPhonemeToVoice :: IO ()
promptForPhonemeToVoice =
  promptForTextAndApply voicedIPA phonemeToVoiceMessage

-- | Ask the user for a phoneme.
--   The user inputs a phoneme.
--   Take the phoneme, and print the
--   description of the phoneme.
promptForPhonemeToDescribe :: IO ()
promptForPhonemeToDescribe =
  promptForTextAndApply describeIPA phonemeToDescribeMessage

-- | Ask the user for a phoneme.
--   The user inputs a phoneme.
--   Take the phoneme, calculate what
--   the features of it are (according to Sound Patterns of English)
--   and print those features.
promptForPhonemeToCalculateSPEFeaturesFrom :: IO ()
promptForPhonemeToCalculateSPEFeaturesFrom =
  promptForTextAndApply analyzeIPAToSPE phonemeToCalculateSPEMessage

-- | Ask the user for IPA text which may contain multiple IPA characters
--   and phonemes. Take that input, and print each phoneme on
--   separate lines.
promptForIPATextToSplit :: IO ()
promptForIPATextToSplit =
  promptForTextAndApply ipaTextToPhonetListReport ipaTextToDivideMessage

-- | This function is where the program starts running.
main :: IO ()
main =
  putStrLn pleaseReadReadmeMessage
  >>  putStr menu
  >>  putPrompt
  >>  getLine
  >>= acknowledgeAndRespond
  >>  putBlankLine
  >>  putStrLn programTerminatedNormallyMessage
  >>  putBlankLines 2

-- | Tell the user what they selected. This is necessary
--   for better user-friendliness.
acknowledgeAndRespond :: Text -- ^ what the user typed in
                      -> IO ()
acknowledgeAndRespond selection =
  (putStrLn . unwords) [userSelectedMessage, selection]
  >> putBlankLine
  >> respondToSelection selection

-- | Start the appropriate action according to what the user already selected.
respondToSelection :: Text -- ^ the text the user put in after being shown the menu
                   -> IO ()
respondToSelection selection
  | selection == userInputViewEnglishPhonemeInventory = putStrLn englishPhonetInventoryReport
  | selection == userInputViewArabicPhonemeInventory  = putStrLn arabicPhonetInventoryReport
  | selection == userInputViewCreePhonemeInventory    = putStrLn plainsCreePhonetInventoryReport
  | selection == userInputViewIrishPhonemeInventory   = putStrLn irishPhonetInventoryReport
  | selection == userInputMakeAPhonemeVoiced          = promptForPhonemeToVoice
  | selection == userInputMakeAPhonemeUnvoiced        = promptForPhonemeToDevoice
  | selection == userInputDescribeAPhonemeInEnglish   = promptForPhonemeToDescribe
  | selection == userInputDescribeAPhonemeInSPE       = promptForPhonemeToCalculateSPEFeaturesFrom
  | selection == userInputChunkIPAByPhoneme           = promptForIPATextToSplit
  | otherwise                                         = putStrLn unrecognizedSelectionMessage


-- | Given an IPA transcription, return the
--   name of the phoneme that IPA transcription describes.
--   If the IPA transcription could not be named,
--   return a message saying so.
doAnalyzeIPA :: Text -- ^ text from the International Phonetic Alphabet
             -> Text -- ^ the name of the phoneme,
                     -- or a message saying the phoneme was not recognized
doAnalyzeIPA x =
  maybe noAnalysisFoundMessage showPhonet (analyzeIPA x)

-- | Given a set of phoneme properties that describe a phoneme, like:
--   voiced velar fricative pulmonic egressive,
--   print the IPA transcription of it to the terminal.
doConstructIPA :: Phonet  -- ^ A phonete to get the transcription of
               -> IO ()
doConstructIPA = putStrLn . constructIPA
