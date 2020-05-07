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
menu = unlines
     ["What do you want to accomplish?"
     , "1) view the English phoneme inventory (as IPA graphemes)."
     , "2) make a phoneme voiced."
     , "3) make a phoneme unvoiced."
     , "4) describe a phoneme in English."
     , "5) describe a phoneme in SPE Features."
     , ""
     , "Enter the number representing your selection below, "
     , "after the prompt, and press enter/return."
     , ""
     , ""
     ]

prompt ∷ Text
prompt = "(PROMPT:) "

putPrompt ∷ IO ()
putPrompt =
  putText prompt
  >> hFlush stdout

analyzeIPAToSPE ∷ Text → Text
analyzeIPAToSPE ipaText =
  maybe "Sorry, unable to calculate answer with that input." (showFeatures ∘ analyzeFeatures) (analyzeIPA ipaText)

putBlankLine ∷ IO ()
putBlankLine = putTextLn ""

putBlankLines ∷ Int → IO ()
putBlankLines n = replicateM_ n putBlankLine

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
  >>  putBlankLine
  >>  putTextLn "Program terminated normally."
  >>  putBlankLines 2

handleSelection ∷ Text → IO ()
handleSelection selection =
  putTextLn ("The user selected: " ⊕ selection)
  >> putTextLn ""
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
