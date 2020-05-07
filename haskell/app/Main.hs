{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main, doAnalyzeIPA, doConstructIPA) where

import Prelude ()
import Relude
import Prelude.Unicode ((∘), (≡))
import System.IO (hFlush)
import Lib ( showIPA, voicedIPA, devoicedIPA, describeIPA, analyzeIPA
           , englishPhonetInventory
           , analyzeFeatures, showFeatures
           , constructIPA
           , showPhonet
           , Phonet
           )
import Data.Monoid.Unicode ( (⊕) )

import EnglishUSText


putPrompt ∷ IO ()
putPrompt =
  putText prompt
  >> hFlush stdout

analyzeIPAToSPE ∷ Text → Text
analyzeIPAToSPE ipaText =
  maybe sorryUnableToCalculate (showFeatures ∘ analyzeFeatures) (analyzeIPA ipaText)

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


promptForPhonemeToDevoice ∷ IO ()
promptForPhonemeToDevoice =
  promptForPhonemeAndApply devoicedIPA phonemeToDevoiceMessage

promptForPhonemeToVoice ∷ IO ()
promptForPhonemeToVoice =
  promptForPhonemeAndApply voicedIPA phonemeToVoiceMessage

promptForPhonemeToDescribe ∷ IO ()
promptForPhonemeToDescribe =
  promptForPhonemeAndApply describeIPA phonemeToDescribeMessage

promptForPhonemeToCalculateSPEFeaturesFrom ∷ IO ()
promptForPhonemeToCalculateSPEFeaturesFrom =
  promptForPhonemeAndApply analyzeIPAToSPE phonemeToCalculateSPEMessage

main ∷ IO ()
main =
  putTextLn pleaseReadReadmeMessage
  >>  putText menu
  >>  putPrompt
  >>  getLine
  >>= handleSelection
  >>  putBlankLine
  >>  putTextLn programTerminatedNormallyMessage
  >>  putBlankLines 2

handleSelection ∷ Text → IO ()
handleSelection selection =
  putTextLn (userSelectedMessage ⊕ selection)
  >> putTextLn ""
  >> case selection of
       _ | selection ≡ userInput_viewEnglishPhonemeInventory → putText (showIPA englishPhonetInventory)
       _ | selection ≡ userInput_makeAPhonemeVoiced          → promptForPhonemeToVoice
       _ | selection ≡ userInput_makeAPhonemeUnvoiced        → promptForPhonemeToDevoice
       _ | selection ≡ userInput_describeAPhonemeInEnglish   → promptForPhonemeToDescribe
       _ | selection ≡ userInput_describeAPhonemeInSPE       → promptForPhonemeToCalculateSPEFeaturesFrom
       _                                                     → putTextLn unrecognizedSelectionMessage

doAnalyzeIPA ∷ Text → Text
doAnalyzeIPA x =
  maybe noAnalysisFoundMessage showPhonet (analyzeIPA x)

doConstructIPA ∷ Phonet → IO ()
doConstructIPA = putTextLn ∘ constructIPA
