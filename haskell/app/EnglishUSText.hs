{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module EnglishUSText where

import Prelude ()
import Relude (Text)

menu ∷ Text
menu =
  "What do you want to accomplish? \n\
  \ 1) view the English phoneme inventory (as IPA graphemes). \n\
  \ 2) make a phoneme voiced. \n\
  \ 3) make a phoneme unvoiced. \n\
  \ 4) describe a phoneme in English. \n\
  \ 5) describe a phoneme in SPE Features. \n\
  \  \n\
  \ Enter the number representing your selection below, \n\
  \ after the prompt, and press enter/return. \n\
  \ \n\
  \ \n\
  \ \n" 

userInput_viewEnglishPhonemeInventory, userInput_makeAPhonemeVoiced, userInput_makeAPhonemeUnvoiced,
  userInput_describeAPhonemeInEnglish, userInput_describeAPhonemeInSPE ∷ Text
userInput_viewEnglishPhonemeInventory = "1"
userInput_makeAPhonemeVoiced = "2"
userInput_makeAPhonemeUnvoiced = "3"
userInput_describeAPhonemeInEnglish = "4"
userInput_describeAPhonemeInSPE = "5"

prompt ∷ Text
prompt = "(PROMPT:) "



sorryUnableToCalculate ∷ Text
sorryUnableToCalculate = "Sorry, unable to calculate answer with that input."

typeAPhoneme ∷  Text
typeAPhoneme =
  "Type a phoneme using IPA symbols, and then press the enter key, \
  \ and the computer will display"


phonemeToDevoiceMessage, phonemeToVoiceMessage, phonemeToDescribeMessage, phonemeToCalculateSPEMessage,
  pleaseReadReadmeMessage, programTerminatedNormallyMessage, userSelectedMessage, unrecognizedSelectionMessage,
  noAnalysisFoundMessage ∷ Text
phonemeToDevoiceMessage = 
  "Type a phoneme using IPA symbols, and then press the enter key,\
  \ and the computer will display\ 
  \ the devoiced counterpart (on the subsequent line):"

phonemeToVoiceMessage = 
  "Type a phoneme using IPA symbols, and then press the enter key,\
  \ and the computer will display\ 
  \ the voiced counterpart (on the subsequent line):"

phonemeToDescribeMessage = 
  "Type a phoneme using IPA symbols, and then press the enter key,\
  \ and the computer will display\ 
  \ its English description (on the subsequent line):"

phonemeToCalculateSPEMessage = 
  "Type a phoneme using IPA symbols, and then press the enter key,\
  \ and the computer will display\ 
  \ its SPE features (on the subsequent line):"

pleaseReadReadmeMessage = "Please read README.md file for instructions on how to use."
programTerminatedNormallyMessage = "Program terminated normally."
userSelectedMessage = "The user selected: "
unrecognizedSelectionMessage =  "Unrecognized selection. No action taken."
noAnalysisFoundMessage = "No analysis found!"

