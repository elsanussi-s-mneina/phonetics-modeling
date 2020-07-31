{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module DiagnosticText where

import           Relude  ((<>), Text)
import qualified EnglishUSText

applicationTitle :: Text
applicationTitle = "Modèle Phonétique "


menu :: Text
menu =
  "Qu'est-ce que vous voulez accomplir? \n\
  \ 1) voir le inventaire de phonèmes de l'anglais (comme graphèmes d'API). \n\
  \ 2) faire un phonème voisée. \n\
  \ 3) faire un phonème sourde. \n\
  \ 4) décrire un phonème en anglais. \n\
  \ 5) décrire un phonème selon Modèle sonore de l'anglais. \n\
  \ 6) diviser texte API en phonèmes \n\
  \ 7) ouvrir la fenêtre principale (interface graphique) \n\
  \ 8) démarrer le serveur Web \n\
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
  userInputOpenWindow,
  userInputStartServer ::
    Text
userInputViewEnglishPhonemeInventory = "1"
userInputMakeAPhonemeVoiced = "2"
userInputMakeAPhonemeUnvoiced = "3"
userInputDescribeAPhonemeInEnglish = "4"
userInputDescribeAPhonemeInSPE = "5"
userInputChunkIPAByPhoneme = "6"
userInputOpenWindow = "7"
userInputStartServer = "8"

prompt :: Text
prompt = tag EnglishUSText.prompt

sorryUnableToCalculate :: Text
sorryUnableToCalculate = tag EnglishUSText.sorryUnableToCalculate

typeAPhoneme :: Text
typeAPhoneme = tag EnglishUSText.typeAPhoneme

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
phonemeToDevoiceMessage = phonemeToDevoiceMessage
phonemeToVoiceMessage = phonemeToVoiceMessage
phonemeToDescribeMessage = phonemeToDescribeMessage
phonemeToCalculateSPEMessage = phonemeToCalculateSPEMessage
ipaTextToDivideMessage = ipaTextToDivideMessage
pleaseReadReadmeMessage = pleaseReadReadmeMessage
programTerminatedNormallyMessage = programTerminatedNormallyMessage
userSelectedMessage = userSelectedMessage
unrecognizedSelectionMessage = unrecognizedSelectionMessage
noAnalysisFoundMessage = noAnalysisFoundMessage

noEnglishDescriptionFoundMessage :: Text
noEnglishDescriptionFoundMessage = noEnglishDescriptionFoundMessage

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
consonantUIText = consonantUIText
vowelUIText = vowelUIText

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

showPhonemeInventoryUIText, makeAPhonemeVoicedUIText,
  quitUIText, makeAPhonemeUnvoicedUIText, describePhonemeUIText,
  getFeaturesOfPhonemeUIText, splitTranscriptionUIText :: Text
showPhonemeInventoryUIText = "Show English phoneme inventory"
makeAPhonemeVoicedUIText = "Make a phoneme voiced…"
quitUIText = "Quit"
makeAPhonemeUnvoicedUIText = "Make a phoneme unvoiced…"
describePhonemeUIText = "Describe a phoneme…"
getFeaturesOfPhonemeUIText =
    "Get sound patterns of English features of IPA transcription…"
splitTranscriptionUIText = "Split IPA transcription text…"

-- Headers:
resultHeader, voicedPhonemeHeader, unvoicedPhonemeHeader,
  phonemeDescriptionHeader, featuresHeader, phonemesSplitHeader,
  englishPhonemeInventoryHeader, inputHeader :: Text
resultHeader = "Result:"
voicedPhonemeHeader = "Voiced Phoneme"
unvoicedPhonemeHeader = "Unvoiced Phoneme"
phonemeDescriptionHeader = "Description of Phoneme"
featuresHeader = "SPE Features of Phoneme"
phonemesSplitHeader = "Phonemes After Being Split"
englishPhonemeInventoryHeader = "English Phoneme Inventory"
inputHeader = "Input:"

beforeServerStartMessage :: Text
beforeServerStartMessage =
  "Starting server...\n\
  \Server running... at port 8080 locally.\n\
  \Browse to http://localhost:8080/voice_phoneme/s\n\
  \to get the voiced phoneme alternative of [s].\n\
  \Browse to http://localhost:8080/devoice_phoneme/z\n\
  \to get the voiceless phoneme alternative of [z]."
