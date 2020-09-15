module EnglishUSText where

import Prelude ()
import Data.Text (Text, pack)


applicationTitle :: Text
applicationTitle = pack "Phonetics Modeling Program"


menu :: Text
menu =
  pack "What do you want to accomplish? \n\
  \ 1) view the English phoneme inventory (as IPA graphemes). \n\
  \ 2) make a phoneme voiced. \n\
  \ 3) make a phoneme unvoiced. \n\
  \ 4) describe a phoneme in English. \n\
  \ 5) describe a phoneme in SPE Features. \n\
  \ 6) divide IPA text into phonemes \n\
  \ 7) view the Arabic phoneme inventory (as IPA graphemes). \n\
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
  userInputViewArabicPhonemeInventory
  ::
    Text
userInputViewEnglishPhonemeInventory = pack "1"
userInputMakeAPhonemeVoiced = pack "2"
userInputMakeAPhonemeUnvoiced = pack "3"
userInputDescribeAPhonemeInEnglish = pack "4"
userInputDescribeAPhonemeInSPE = pack "5"
userInputChunkIPAByPhoneme = pack "6"
userInputViewArabicPhonemeInventory = pack "7"


prompt :: Text
prompt = pack "(PROMPT:) "

sorryUnableToCalculate :: Text
sorryUnableToCalculate = pack "Sorry, unable to calculate answer with that input."

typeAPhoneme :: Text
typeAPhoneme =
  pack "Type a phoneme using IPA symbols, and then press the enter key, \
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
  pack "Type a phoneme using IPA symbols, and then press the enter key,\
  \ and the computer will display\
  \ the devoiced counterpart (on the subsequent line):"
phonemeToVoiceMessage =
  pack "Type a phoneme using IPA symbols, and then press the enter key,\
  \ and the computer will display\
  \ the voiced counterpart (on the subsequent line):"
phonemeToDescribeMessage =
  pack "Type a phoneme using IPA symbols, and then press the enter key,\
  \ and the computer will display\
  \ its English description (on the subsequent line):"
phonemeToCalculateSPEMessage =
  pack "Type a phoneme using IPA symbols, and then press the enter key,\
  \ and the computer will display\
  \ its SPE features (on the subsequent line):"
ipaTextToDivideMessage =
  pack "Type text using IPA symbols, and then press the enter key,\
  \ and the computer will display\
  \ the text you entered with separate phonemes on separate lines:"
pleaseReadReadmeMessage = pack "Please read README.md file for instructions on how to use."
programTerminatedNormallyMessage = pack "Program terminated normally."
userSelectedMessage = pack "The user selected:"
unrecognizedSelectionMessage = pack "Unrecognized selection. No action taken."
noAnalysisFoundMessage = pack "No analysis found!"

noEnglishDescriptionFoundMessage :: Text
noEnglishDescriptionFoundMessage = pack "(no English description found.)"

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
consonantUIText = pack "consonant"
vowelUIText = pack "vowel"

frontBacknessUIText = pack "front"
centralBacknessUIText = pack "central"
backBacknessUIText = pack "back"

closeHeightUIText = pack "close"
nearCloseHeightUIText = pack "near-close"
closeMidHeightUIText = pack "close-mid"
midHeightUIText = pack "mid"
openMidHeightUIText = pack "open-mid"
nearOpenHeightUIText = pack "near-open"
openHeightUIText = pack "open"

roundedRoundingUIText = pack "rounded"
unroundedRoundingUIText = pack "unrounded"

extraShortUIText :: Text
extraShortUIText = pack "extra-short"

halfLongUIText :: Text
halfLongUIText = pack "half-long"

longUIText :: Text
longUIText = pack "long"



bilabialPlaceUIText = pack "bilabial"
labioDentalPlaceUIText = pack "labio-dental"
dentalPlaceUIText = pack "dental"
alveolarPlaceUIText = pack "alveolar"
postAlveolarPlaceUIText = pack "post-alveolar"
retroflexPlaceUIText = pack "retroflex"
palatalPlaceUIText = pack "palatal"
velarPlaceUIText = pack "velar"
uvularPlaceUIText = pack "uvular"
pharyngealPlaceUIText = pack "pharyngeal"
glottalPlaceUIText = pack "glottal"
epiglottalPlaceUIText = pack "epiglottal"
labialVelarPlaceUIText = pack "labial-velar"
labialPalatalPlaceUIText = pack "labial-palatal"
alveoloPalatalPlaceUIText = pack "alveolo-palatal"
palatoAlveolarPlaceUIText = pack "palato-alveolar"

plosiveMannerUIText = pack "plosive"
nasalMannerUIText = pack "nasal"
trillMannerUIText = pack "trill"
tapOrFlapMannerUIText = pack "tap or flap"
approximantMannerUIText = pack "approximant"
fricativeMannerUIText = pack "fricative"
affricateMannerUIText = pack "affricate"
lateralFricativeMannerUIText = pack "lateral fricative"
lateralApproximantMannerUIText = pack "lateral approximant"
lateralFlapMannerUIText = pack "lateral flap"
lateralMannerUIText = pack "lateral"

pulmonicEgressiveAirstreamUIText = pack "pulmonic egressive"
clickAirstreamUIText = pack "click"
implosiveAirstreamUIText = pack "implosive"

voicedVocalFoldsUIText = pack "voiced"
voicelessVocalFoldsUIText = pack "voiceless"
voicedAspiratedVocalFoldsUIText = pack "voiced aspirated"
voicelessAspiratedVocalFoldsUIText = pack "voiceless aspirated"
creakyVoicedVocalFoldsUIText = pack "creaky voiced"

syllabicPhonemeFeatureUIText = pack "syllabic"
consonantalPhonemeFeatureUIText = pack "consonantal"
sonorantPhonemeFeatureUIText = pack "sonorant"
continuantPhonemeFeatureUIText = pack "continuant"
voicePhonemeFeatureUIText = pack "voice"
atrPhonemeFeatureUIText = pack "ATR"
nasalPhonemeFeatureUIText = pack "nasal"
lateralPhonemeFeatureUIText = pack "lateral"
delayedReleasePhonemeFeatureUIText = pack "delayed release"
spreadGlottisPhonemeFeatureUIText = pack "spread glottis"
constrictedGlottisPhonemeFeatureUIText = pack "constricted glottis"
labialPhonemeFeatureUIText = pack "labial"
coronalPhonemeFeatureUIText = pack "coronal"
dorsalPhonemeFeatureUIText = pack "dorsal"
pharyngealPhonemeFeatureUIText = pack "pharyngeal"
laryngealPhonemeFeatureUIText = pack "laryngeal"
roundPhonemeFeatureUIText = pack "round"
anteriorPhonemeFeatureUIText = pack "anterior"
distributedPhonemeFeatureUIText = pack "distributed"
stridentPhonemeFeatureUIText = pack "strident"
highPhonemeFeatureUIText = pack "high"
lowPhonemeFeatureUIText = pack "low"
backPhonemeFeatureUIText = pack "back"

-- Secondary Articulations:
labializedUIText :: Text
labializedUIText = pack "labialized"

palatalizedUIText :: Text
palatalizedUIText = pack "palatalized"

velarizedUIText :: Text
velarizedUIText = pack "velarized"

pharyngealizedUIText :: Text
pharyngealizedUIText = pack "pharyngealized"


showPhonemeInventoryUIText, makeAPhonemeVoicedUIText,
  quitUIText, makeAPhonemeUnvoicedUIText, describePhonemeUIText,
  getFeaturesOfPhonemeUIText, splitTranscriptionUIText :: Text
showPhonemeInventoryUIText = pack "Show English phoneme inventory"
makeAPhonemeVoicedUIText = pack "Make a phoneme voiced…"
quitUIText = pack "Quit"
makeAPhonemeUnvoicedUIText = pack "Make a phoneme unvoiced…"
describePhonemeUIText = pack "Describe a phoneme…"
getFeaturesOfPhonemeUIText =
    pack "Get sound patterns of English features of IPA transcription…"
splitTranscriptionUIText = pack "Split IPA transcription text…"

-- Headers:
resultHeader, voicedPhonemeHeader, unvoicedPhonemeHeader,
  phonemeDescriptionHeader, featuresHeader, phonemesSplitHeader,
  englishPhonemeInventoryHeader, inputHeader :: Text
resultHeader = pack "Result:"
voicedPhonemeHeader = pack "Voiced Phoneme"
unvoicedPhonemeHeader = pack "Unvoiced Phoneme"
phonemeDescriptionHeader = pack "Description of Phoneme"
featuresHeader = pack "SPE Features of Phoneme"
phonemesSplitHeader = pack "Phonemes After Being Split"
englishPhonemeInventoryHeader = pack "English Phoneme Inventory"
inputHeader = pack "Input:"

beforeServerStartMessage :: Text
beforeServerStartMessage =
  pack "Starting server...\n\
  \Server running... at port 8080 locally.\n\
  \Browse to http://localhost:8080/voice_phoneme/s\n\
  \to get the voiced phoneme alternative of [s].\n\
  \Browse to http://localhost:8080/devoice_phoneme/z\n\
  \to get the voiceless phoneme alternative of [z].\n\
  \Browse to http://localhost:8080/describe_phoneme/s\n\
  \to get the English language description of [s]."
