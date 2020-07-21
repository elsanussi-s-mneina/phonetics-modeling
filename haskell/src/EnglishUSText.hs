{-# LANGUAGE OverloadedStrings #-}

module EnglishUSText where

import           Prelude ()
import           Relude  (Text)

menu :: Text
menu =
  "What do you want to accomplish? \n\
  \ 1) view the English phoneme inventory (as IPA graphemes). \n\
  \ 2) make a phoneme voiced. \n\
  \ 3) make a phoneme unvoiced. \n\
  \ 4) describe a phoneme in English. \n\
  \ 5) describe a phoneme in SPE Features. \n\
  \ 6) divide IPA text into phonemes (chunk) \n\
  \ 7) open window (user-friendly mode) \n\
  \  \n\
  \ Enter the number representing your selection below, \n\
  \ after the prompt, and press enter/return. \n\
  \ \n\
  \ \n\
  \ \n"

userInputViewEnglishPhonemeInventory,
  userInputMakeAPhonemeVoiced,
  userInputMakeAPhonemeUnvoiced,
  userInputDescribeAPhonemeInEnglish,
  userInputDescribeAPhonemeInSPE,
  userInputChunkIPAByPhoneme,
  userInputOpenWindow ::
    Text
userInputViewEnglishPhonemeInventory = "1"
userInputMakeAPhonemeVoiced = "2"
userInputMakeAPhonemeUnvoiced = "3"
userInputDescribeAPhonemeInEnglish = "4"
userInputDescribeAPhonemeInSPE = "5"
userInputChunkIPAByPhoneme = "6"
userInputOpenWindow = "7"

prompt :: Text
prompt = "(PROMPT:) "

sorryUnableToCalculate :: Text
sorryUnableToCalculate = "Sorry, unable to calculate answer with that input."

typeAPhoneme :: Text
typeAPhoneme =
  "Type a phoneme using IPA symbols, and then press the enter key, \
  \ and the computer will display"

phonemeToDevoiceMessage,
  phonemeToVoiceMessage,
  phonemeToDescribeMessage,
  phonemeToCalculateSPEMessage,
  pleaseReadReadmeMessage,
  programTerminatedNormallyMessage,
  userSelectedMessage,
  unrecognizedSelectionMessage,
  noAnalysisFoundMessage,
  ipaTextToDivideMessage ::
    Text
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
unrecognizedSelectionMessage = "Unrecognized selection. No action taken."
noAnalysisFoundMessage = "No analysis found!"

noEnglishDescriptionFoundMessage :: Text
noEnglishDescriptionFoundMessage = "(no English description found.)"

consonantUIText,
  vowelUIText,
  frontBacknessUIText,
  centralBacknessUIText,
  backBacknessUIText,
  closeHeightUIText,
  nearCloseHeightUIText,
  closeMidHeightUIText,
  midHeightUIText,
  openMidHeightUIText,
  nearOpenHeightUIText,
  openHeightUIText,
  roundedRoundingUIText,
  unroundedRoundingUIText,
  bilabialPlaceUIText,
  labioDentalPlaceUIText,
  dentalPlaceUIText,
  alveolarPlaceUIText,
  postAlveolarPlaceUIText,
  retroflexPlaceUIText,
  palatalPlaceUIText,
  velarPlaceUIText,
  uvularPlaceUIText,
  pharyngealPlaceUIText,
  glottalPlaceUIText,
  epiglottalPlaceUIText,
  labialVelarPlaceUIText,
  labialPalatalPlaceUIText,
  alveoloPalatalPlaceUIText,
  palatoAlveolarPlaceUIText,
  plosiveMannerUIText,
  nasalMannerUIText,
  trillMannerUIText,
  tapOrFlapMannerUIText,
  approximantMannerUIText,
  fricativeMannerUIText,
  affricateMannerUIText,
  lateralFricativeMannerUIText,
  lateralApproximantMannerUIText,
  lateralFlapMannerUIText,
  lateralMannerUIText,
  pulmonicEgressiveAirstreamUIText,
  clickAirstreamUIText,
  implosiveAirstreamUIText,
  voicedVocalFoldsUIText,
  voicelessVocalFoldsUIText,
  voicedAspiratedVocalFoldsUIText,
  voicelessAspiratedVocalFoldsUIText,
  creakyVoicedVocalFoldsUIText,
  syllabicPhonemeFeatureUIText,
  consonantalPhonemeFeatureUIText,
  sonorantPhonemeFeatureUIText,
  continuantPhonemeFeatureUIText,
  voicePhonemeFeatureUIText,
  atrPhonemeFeatureUIText,
  nasalPhonemeFeatureUIText,
  lateralPhonemeFeatureUIText,
  delayedReleasePhonemeFeatureUIText,
  spreadGlottisPhonemeFeatureUIText,
  constrictedGlottisPhonemeFeatureUIText,
  labialPhonemeFeatureUIText,
  coronalPhonemeFeatureUIText,
  dorsalPhonemeFeatureUIText,
  pharyngealPhonemeFeatureUIText,
  laryngealPhonemeFeatureUIText,
  roundPhonemeFeatureUIText,
  anteriorPhonemeFeatureUIText,
  distributedPhonemeFeatureUIText,
  stridentPhonemeFeatureUIText,
  highPhonemeFeatureUIText,
  lowPhonemeFeatureUIText,
  backPhonemeFeatureUIText ::
    Text
consonantUIText = "consonant"
vowelUIText = "vowel"
frontBacknessUIText = "front"
centralBacknessUIText = "central"
backBacknessUIText = "back"
closeHeightUIText = "close"
nearCloseHeightUIText = "near-close"
closeMidHeightUIText = "close-mid"
midHeightUIText = "mid"
openMidHeightUIText = "open-mid"
nearOpenHeightUIText = "near-open"
openHeightUIText = "open"
roundedRoundingUIText = "rounded"
unroundedRoundingUIText = "unrounded"
bilabialPlaceUIText = "bilabial"
labioDentalPlaceUIText = "labio-dental"
dentalPlaceUIText = "dental"
alveolarPlaceUIText = "alveolar"
postAlveolarPlaceUIText = "post-alveolar"
retroflexPlaceUIText = "retroflex"
palatalPlaceUIText = "palatal"
velarPlaceUIText = "velar"
uvularPlaceUIText = "uvular"
pharyngealPlaceUIText = "pharyngeal"
glottalPlaceUIText = "glottal"
epiglottalPlaceUIText = "epiglottal"
labialVelarPlaceUIText = "labial-velar"
labialPalatalPlaceUIText = "labial-palatal"
alveoloPalatalPlaceUIText = "alveolo-palatal"
palatoAlveolarPlaceUIText = "palato-alveolar"
plosiveMannerUIText = "plosive"
nasalMannerUIText = "nasal"
trillMannerUIText = "trill"
tapOrFlapMannerUIText = "tap or flap"
approximantMannerUIText = "approximant"
fricativeMannerUIText = "fricative"
affricateMannerUIText = "affricate"
lateralFricativeMannerUIText = "lateral fricative"
lateralApproximantMannerUIText = "lateral approximant"
lateralFlapMannerUIText = "lateral flap"
lateralMannerUIText = "lateral"
pulmonicEgressiveAirstreamUIText = "pulmonic egressive"
clickAirstreamUIText = "click"
implosiveAirstreamUIText = "implosive"
voicedVocalFoldsUIText = "voiced"
voicelessVocalFoldsUIText = "voiceless"
voicedAspiratedVocalFoldsUIText = "voiced aspirated"
voicelessAspiratedVocalFoldsUIText = "voiceless aspirated"
creakyVoicedVocalFoldsUIText = "creaky voiced"
syllabicPhonemeFeatureUIText = "syllabic"
consonantalPhonemeFeatureUIText = "consonantal"
sonorantPhonemeFeatureUIText = "sonorant"
continuantPhonemeFeatureUIText = "continuant"
voicePhonemeFeatureUIText = "voice"
atrPhonemeFeatureUIText = "ATR"
nasalPhonemeFeatureUIText = "nasal"
lateralPhonemeFeatureUIText = "lateral"
delayedReleasePhonemeFeatureUIText = "delayed release"
spreadGlottisPhonemeFeatureUIText = "spread glottis"
constrictedGlottisPhonemeFeatureUIText = "constricted glottis"
labialPhonemeFeatureUIText = "labial"
coronalPhonemeFeatureUIText = "coronal"
dorsalPhonemeFeatureUIText = "dorsal"
pharyngealPhonemeFeatureUIText = "pharyngeal"
laryngealPhonemeFeatureUIText = "laryngeal"
roundPhonemeFeatureUIText = "round"
anteriorPhonemeFeatureUIText = "anterior"
distributedPhonemeFeatureUIText = "distributed"
stridentPhonemeFeatureUIText = "strident"
highPhonemeFeatureUIText = "high"
lowPhonemeFeatureUIText = "low"
backPhonemeFeatureUIText = "back"
