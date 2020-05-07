{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main, doAnalyzeIPA, doConstructIPA) where

import Prelude ()
import Relude
import Data.Monoid.Unicode ( (⊕) )
import Prelude.Unicode ((∘))
import System.IO (hFlush)
import Lib ( showIPA, voicedIPA, devoicedIPA, describeIPA, analyzeIPA
           , englishPhonetInventory
           , analyzeFeatures, showFeatures
           , constructIPA
           , showPhonet
           , Phonet
           )

menu ∷ Text
menu = "What do you want to accomplish?\n\n"
     ⊕ "1) view the English phoneme inventory (as IPA graphemes).\n"
     ⊕ "2) make a phoneme voiced.\n"
     ⊕ "3) make a phoneme unvoiced.\n"
     ⊕ "4) describe a phoneme in English.\n"
     ⊕ "5) describe a phoneme in SPE Features.\n"
     ⊕ "\n"
     ⊕ "Enter the number representing your selection below, "
     ⊕ "after the prompt, and press enter/return.\n\n\n"

prompt ∷ Text
prompt = "(PROMPT:) "

putPrompt ∷ IO ()
putPrompt =
  putText prompt
  >> hFlush stdout

analyzeIPAToSPE ∷ Text → Text
analyzeIPAToSPE ipaText = 
  maybe "Sorry, unable to calculate answer with that input." (showFeatures ∘ analyzeFeatures) (analyzeIPA ipaText)


promptForPhonemeAndApply ∷ (Text → Text) → Text → IO ()
promptForPhonemeAndApply func instructions =
    putTextLn instructions
    >> putPrompt
    >> getLine
    >>= \phoneme -> putTextLn (func phoneme)

typeAPhoneme ∷  Text
typeAPhoneme
  = "Type a phoneme using IPA symbols, and then press the enter key,"
  ⊕ " and the computer will display"

promptForPhonemeToDevoice ∷ IO ()
promptForPhonemeToDevoice =
  promptForPhonemeAndApply devoicedIPA
    (typeAPhoneme ⊕ " the devoiced counterpart (on the subsequent line):")

promptForPhonemeToVoice ∷ IO ()
promptForPhonemeToVoice =
  promptForPhonemeAndApply voicedIPA
    (typeAPhoneme ⊕ " the voiced counterpart (on the subsequent line):")

promptForPhonemeToDescribe ∷ IO ()
promptForPhonemeToDescribe =
  promptForPhonemeAndApply describeIPA
    (typeAPhoneme ⊕ " its English description (on the subsequent line):")

promptForPhonemeToCalculateSPEFeaturesFrom ∷ IO ()
promptForPhonemeToCalculateSPEFeaturesFrom =
  promptForPhonemeAndApply analyzeIPAToSPE
    (typeAPhoneme ⊕ " its SPE features (on the subsequent line):")

main ∷ IO ()
main =
  putTextLn "Please read README.md file for instructions on how to use."
  >>  putText menu
  >>  putPrompt
  >>  getLine
  >>= handleSelection
  >>  putTextLn "\nProgram terminated normally.\n\n"

handleSelection ∷ Text → IO ()
handleSelection selection =
  putTextLn ("The user selected: " ⊕ selection ⊕ "\n")
  >> case selection of
       "1" → putText (showIPA englishPhonetInventory)
       "2" → promptForPhonemeToVoice
       "3" → promptForPhonemeToDevoice
       "4" → promptForPhonemeToDescribe
       "5" → promptForPhonemeToCalculateSPEFeaturesFrom
       _   → putTextLn "Unrecognized selection. No action taken."

doAnalyzeIPA ∷ Text → Text
doAnalyzeIPA x =
  maybe "No analysis found!" showPhonet (analyzeIPA x)

doConstructIPA ∷ Phonet → IO ()
doConstructIPA = putTextLn ∘ constructIPA
