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
  \ 6) divide IPA text into phonemes (chunk) \n\
  \  \n\
  \ Enter the number representing your selection below, \n\
  \ after the prompt, and press enter/return. \n\
  \ \n\
  \ \n\
  \ \n" 

userInput_viewEnglishPhonemeInventory, userInput_makeAPhonemeVoiced, userInput_makeAPhonemeUnvoiced,
  userInput_describeAPhonemeInEnglish, userInput_describeAPhonemeInSPE, userInput_chunkIPAByPhoneme ∷ Text
userInput_viewEnglishPhonemeInventory = "1"
userInput_makeAPhonemeVoiced = "2"
userInput_makeAPhonemeUnvoiced = "3"
userInput_describeAPhonemeInEnglish = "4"
userInput_describeAPhonemeInSPE = "5"
userInput_chunkIPAByPhoneme = "6"

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
  noAnalysisFoundMessage, ipaTextToDivideMessage ∷ Text
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

ipaTextToDivideMessage =
  "Type text using IPA symbols, and then press the enter key,\
  \ and the computer will display\
  \ the text you entered with separate phonemes on separate lines:"

pleaseReadReadmeMessage = "Please read README.md file for instructions on how to use."
programTerminatedNormallyMessage = "Program terminated normally."
userSelectedMessage = "The user selected:"
unrecognizedSelectionMessage =  "Unrecognized selection. No action taken."
noAnalysisFoundMessage = "No analysis found!"



noEnglishDescriptionFound_message ∷ Text   
noEnglishDescriptionFound_message = "(no English description found.)"


consonant_Text
  , vowel_Text
     
  , front_BacknessText
  , central_BacknessText
  , back_BacknessText                  
             
  , close_HeightText
  , nearClose_HeightText
  , closeMid_HeightText
  , mid_HeightText
  , openMid_HeightText
  , nearOpen_HeightText
  , open_HeightText
                  
  , rounded_RoundingText
  , unrounded_RoundingText
            
  , bilabial_PlaceText
  , labioDental_PlaceText
  , dental_PlaceText
  , alveolar_PlaceText
  , postAlveolar_PlaceText
  , retroflex_PlaceText
  , palatal_PlaceText
  , velar_PlaceText
  , uvular_PlaceText
  , pharyngeal_PlaceText
  , glottal_PlaceText
  , epiglottal_PlaceText
  , labialVelar_PlaceText
  , labialPalatal_PlaceText
  , alveoloPalatal_PlaceText
  , palatoAlveolar_PlaceText
            
  , plosive_MannerText
  , nasal_MannerText
  , trill_MannerText
  , tapOrFlap_MannerText
  , approximant_MannerText
  , fricative_MannerText
  , affricate_MannerText
  , lateralFricative_MannerText
  , lateralApproximant_MannerText
  , lateralFlap_MannerText
  , lateral_MannerText
                  
  , pulmonicEgressive_AirstreamText
  , click_AirstreamText
  , implosive_AirstreamText
      
  , voiced_VocalFoldsText
  , voiceless_VocalFoldsText
  , voicedAspirated_VocalFoldsText
  , voicelessAspirated_VocalFoldsText
  , creakyVoiced_VocalFoldsText
   
  , syllabic_PhonemeFeatureText
  , consonantal_PhonemeFeatureText
  , sonorant_PhonemeFeatureText
  , continuant_PhonemeFeatureText
  , voice_PhonemeFeatureText
  , atr_PhonemeFeatureText
  , nasal_PhonemeFeatureText
  , lateral_PhonemeFeatureText
  , delayedRelease_PhonemeFeatureText
  , spreadGlottis_PhonemeFeatureText
  , constrictedGlottis_PhonemeFeatureText
  , labial_PhonemeFeatureText
  , coronal_PhonemeFeatureText
  , dorsal_PhonemeFeatureText
  , pharyngeal_PhonemeFeatureText
  , laryngeal_PhonemeFeatureText
  , round_PhonemeFeatureText
  , anterior_PhonemeFeatureText
  , distributed_PhonemeFeatureText
  , strident_PhonemeFeatureText
  , high_PhonemeFeatureText
  , low_PhonemeFeatureText
  , back_PhonemeFeatureText ∷ Text




consonant_Text                = "consonant"
vowel_Text                    = "vowel"

front_BacknessText            = "front"  
central_BacknessText          = "central"
back_BacknessText             = "back"   


close_HeightText              = "close"     
nearClose_HeightText          = "near-close"
closeMid_HeightText           = "close-mid" 
mid_HeightText                = "mid"       
openMid_HeightText            = "open-mid"  
nearOpen_HeightText           = "near-open" 
open_HeightText               = "open"      



rounded_RoundingText          = "rounded"
unrounded_RoundingText        = "unrounded"


bilabial_PlaceText            = "bilabial"
labioDental_PlaceText         = "labio-dental"
dental_PlaceText              = "dental"
alveolar_PlaceText            = "alveolar"
postAlveolar_PlaceText        = "post-alveolar"
retroflex_PlaceText           = "retroflex"
palatal_PlaceText             = "palatal"
velar_PlaceText               = "velar"
uvular_PlaceText              = "uvular"
pharyngeal_PlaceText          = "pharyngeal"
glottal_PlaceText             = "glottal"
epiglottal_PlaceText          = "epiglottal"
labialVelar_PlaceText         = "labial-velar"
labialPalatal_PlaceText       = "labial-palatal"
alveoloPalatal_PlaceText      = "alveolo-palatal"
palatoAlveolar_PlaceText      = "palato-alveolar"


plosive_MannerText            = "plosive"
nasal_MannerText              = "nasal"
trill_MannerText              = "trill"
tapOrFlap_MannerText          = "tap or flap"
approximant_MannerText        = "approximant"
fricative_MannerText          = "fricative"
affricate_MannerText          = "affricate"
lateralFricative_MannerText   = "lateral fricative"
lateralApproximant_MannerText = "lateral approximant"
lateralFlap_MannerText        = "lateral flap"
lateral_MannerText            = "lateral"



pulmonicEgressive_AirstreamText       = "pulmonic egressive"
click_AirstreamText                   = "click"
implosive_AirstreamText               = "implosive"


voiced_VocalFoldsText                 = "voiced"
voiceless_VocalFoldsText              = "voiceless"
voicedAspirated_VocalFoldsText        = "voiced aspirated"
voicelessAspirated_VocalFoldsText     = "voiceless aspirated"
creakyVoiced_VocalFoldsText           = "creaky voiced"


syllabic_PhonemeFeatureText           = "syllabic"
consonantal_PhonemeFeatureText        = "consonantal"
sonorant_PhonemeFeatureText           = "sonorant"
continuant_PhonemeFeatureText         = "continuant"
voice_PhonemeFeatureText              = "voice"
atr_PhonemeFeatureText                = "ATR"
nasal_PhonemeFeatureText              = "nasal"
lateral_PhonemeFeatureText            = "lateral"
delayedRelease_PhonemeFeatureText     = "delayed release"
spreadGlottis_PhonemeFeatureText      = "spread glottis"
constrictedGlottis_PhonemeFeatureText = "constricted glottis"
labial_PhonemeFeatureText             = "labial"
coronal_PhonemeFeatureText            = "coronal"
dorsal_PhonemeFeatureText             = "dorsal"
pharyngeal_PhonemeFeatureText         = "pharyngeal"
laryngeal_PhonemeFeatureText          = "laryngeal"
round_PhonemeFeatureText              = "round"
anterior_PhonemeFeatureText           = "anterior"
distributed_PhonemeFeatureText        = "distributed"
strident_PhonemeFeatureText           = "strident"
high_PhonemeFeatureText               = "high"
low_PhonemeFeatureText                = "low"
back_PhonemeFeatureText               = "back"
