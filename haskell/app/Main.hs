module Main (main) where

import Prelude (IO, String, Maybe(Just, Nothing), getLine, putStr, putStrLn)
import Prelude.Unicode
  ( (⧺)
  )

import System.IO (hFlush, stdout)
import InternationalPhoneticAlphabet (showIPA, voicedIPA, devoicedIPA, describeIPA, analyzeIPA)
import English (englishPhonetInventory)
import PhonemeFeature (analyzeFeatures, showFeatures)
import DeveloperMode (developerMain)

menu ∷ String
menu = "What do you want to accomplish?\n\n"
        ⧺ "1) view the English phoneme inventory (as IPA graphemes).\n"
        ⧺ "2) make a phoneme voiced.\n"
        ⧺ "3) make a phoneme unvoiced.\n"
        ⧺ "4) describe a phoneme in English.\n"
        ⧺ "5) describe a phoneme in SPE Features.\n"
        ⧺ "\n"
        ⧺ "Enter the number representing your selection below, after the prompt, and press enter/return.\n\n\n"


prompt ∷ String
prompt = "(PROMPT:) "


putPrompt ∷ IO ()
putPrompt = do
    putStr prompt
    hFlush stdout

analyzeIPAToSPE ∷ String → String
analyzeIPAToSPE ipaText = 
  let maybePhonet = analyzeIPA ipaText
  in case maybePhonet of 
       Nothing → "Sorry, unable to calculate answer with that input."
       Just phonet → showFeatures (analyzeFeatures phonet)


promptForPhonemeAndApply ∷ (String → String) → String → IO ()
promptForPhonemeAndApply func instructions = 
  do
    putStrLn instructions
    putPrompt
    phoneme ← getLine
    putStrLn (func phoneme)

promptForPhonemeToDevoice ∷ IO ()
promptForPhonemeToDevoice = 
  promptForPhonemeAndApply devoicedIPA "Type a phoneme using IPA symbols, and then press the enter key, and the computer will display the devoiced counterpart (on the subsequent line):"

promptForPhonemeToVoice ∷ IO ()
promptForPhonemeToVoice =  
  promptForPhonemeAndApply voicedIPA "Type a phoneme using IPA symbols, and then press the enter key, and the computer will display the voiced counterpart (on the subsequent line):"

promptForPhonemeToDescribe ∷ IO ()
promptForPhonemeToDescribe =  
  promptForPhonemeAndApply describeIPA "Type a phoneme using IPA symbols, and then press the enter key, and the computer will display its English description (on the subsequent line):"


promptForPhonemeToCalculateSPEFeaturesFrom ∷ IO ()
promptForPhonemeToCalculateSPEFeaturesFrom =
  promptForPhonemeAndApply analyzeIPAToSPE "Type a phoneme using IPA symbols, and then press the enter key, and the computer will display its SPE features (on the subsequent line):"


main ∷ IO ()
main = 
  do
    putStrLn "Please read README.md file for instructions on how to use."
    putStr menu
    putPrompt
    selection ← getLine
    putStrLn ("The user selected: " ⧺ selection ⧺ "\n")
    case selection of
                   "1" → putStrLn (showIPA englishPhonetInventory)
                   "2" → promptForPhonemeToVoice
                   "3" → promptForPhonemeToDevoice
                   "4" → promptForPhonemeToDescribe
                   "5" → promptForPhonemeToCalculateSPEFeaturesFrom
                   "road not taken" → developerMain
                   _ → putStrLn "Unrecognized selection. No action taken."

    putStrLn "\nProgram terminated normally.\n\n"