{-|
How to use:

 You will need the following two lines.

 import SpecGeneratorForPython

 putTextLn SpecGeneratorForPython.generatedTestCode

 Usually I put it in the main for spec, so that it
 generates the test code as part of the unit tests.
 Look for two commented out lines for this in Spec.hs.

 The following command redirects the output from
 running the HSpec unit tests to a file.

 stack test > test_ipa_test_to_transcription.py

 Then I delete the part that is not Python code from the
 generated file.
-}
{-# LANGUAGE OverloadedStrings #-}
module SpecGeneratorForPython where

import           Data.Maybe    (fromJust, Maybe(Just, Nothing))
import           Test.Hspec    (Spec, describe, hspec, it, shouldBe)
import           IPA (analyzeIPA, voicedIPA, devoicedIPA, analyzeIPAToSPE, describeIPA)
import PhonetInventory (ipaTextToPhonetListReport)
import           PhoneticFeatures (isGlide)
import Prelude ((.))
import Data.Semigroup (Semigroup((<>)))
import Data.Text (Text)
import qualified Data.Text as T

replaceSpacesWithUnderscores :: Text -> Text
replaceSpacesWithUnderscores x =
  T.replace " " "_" x

replaceDashesWithUnderscores :: Text -> Text
replaceDashesWithUnderscores x =
  T.replace "-" "_" x

replaceIllegalIdentifierChars :: Text -> Text
replaceIllegalIdentifierChars = replaceSpacesWithUnderscores . replaceDashesWithUnderscores

putQuotesAround :: Text -> Text
putQuotesAround x = "\"" <> x <> "\""


isDescribedAsGen
  :: Text -- ^ IPA e.g. "pʰ"
  -> Text -- ^ expected english description e.g. "voiceless aspirated bilabial plosive pulmonic egressive consonant"
  -> Text
(isDescribedAsGen) ipaText description =
    "    def " <> replaceIllegalIdentifierChars ("test_" <> ipaText <> " is the representation of the " <> description <> "(self):")
    <> "\n"
    <> "        " <> "expected = " <> putQuotesAround description <> "\n"
    <> "        " <> "actual = " <> "describe_transcription(" <> putQuotesAround ipaText <> ")" <> "\n"
    <> "        self.assertEqual(actual, expected)\n\n"


emptyIfEmpty :: Text -> Text
emptyIfEmpty x =
  if T.null x then "" else x <> " "

type SecondArticulationTestGenFunction
  = (  Text       -- ^ articulation name, e.g. "palatalized"
    -> Text       -- ^ superscript, e.g. "ʲ"
    -> Maybe Text -- ^ IPA representation of voiceless phoneme "t"
    -> Maybe Text -- ^ IPA representation of voiced phoneme "d"
    -> Text       -- ^ place of articulation (as text) e.g. "alveolar"
    -> Text       -- ^ manner of articulation (as text) e.g. "alveolar"
    -> Text
    )

voiced2ndArticulationTestsGen :: SecondArticulationTestGenFunction
voiced2ndArticulationTestsGen articulation2Name superscript voicelessIPA voicedIPA place manner =
  let theDescription = "voiced " <> emptyIfEmpty articulation2Name
                     <> place <> " " <> manner
                     <> " pulmonic egressive consonant"
  in "# " <> theDescription <> "\n" <>
       case voicedIPA of
         Just b  -> (b <> superscript) `isDescribedAsGen` theDescription
         Nothing -> ""
       <>
       case voicelessIPA of
         Just p -> (p <> "̬" <> superscript) `isDescribedAsGen` theDescription <>
                   (p <> "̬" <> superscript) `isDescribedAsGen` theDescription
         Nothing -> ""


voicedAspirated2ndArticulationGen :: SecondArticulationTestGenFunction
voicedAspirated2ndArticulationGen articulation2Name superscript voicelessIPA voicedIPA place manner =
  let theDescription = "voiced aspirated " <> emptyIfEmpty articulation2Name
                     <> place <> " "
                     <> manner <> " pulmonic egressive consonant"
  in "# " <> theDescription <> "\n" <>
       case voicedIPA of
         Just b -> (b <> "ʰ" <> superscript) `isDescribedAsGen` theDescription <>
                   (b <> "̬ʰ" <> superscript) `isDescribedAsGen` theDescription
         Nothing -> ""
       <>
       case voicelessIPA
         of Nothing -> ""
            Just p -> (p <> "̬ʰ" <> superscript) `isDescribedAsGen` theDescription



voicelessAspirated2ndArticulationTestsGen :: SecondArticulationTestGenFunction
voicelessAspirated2ndArticulationTestsGen articulation2Name superscript voicelessIPA voicedIPA place manner =
  let theDescription = "voiceless aspirated " <> emptyIfEmpty articulation2Name
                       <> place <>
                       " " <> manner <> " pulmonic egressive consonant"
  in "# " <> theDescription <> "\n" <>
       case voicelessIPA of
         Just p -> (p <> "ʰ" <> superscript) `isDescribedAsGen` theDescription
         Nothing -> ""
       <>
       case voicedIPA of
         Just b ->
           (b <> "̥ʰ" <> superscript) `isDescribedAsGen` theDescription <>
           (b <> "̊ʰ" <> superscript) `isDescribedAsGen` theDescription
         Nothing -> ""


voiceless2ndArticulationTestsGen :: SecondArticulationTestGenFunction
voiceless2ndArticulationTestsGen articulation2Name superscript voicelessIPA voicedIPA place manner =
  let theDescription = "voiceless " <> emptyIfEmpty articulation2Name <> place <> " "
                     <> manner <> " pulmonic egressive consonant"
  in "# " <> theDescription <> "\n"
     <> case voicelessIPA
          of Nothing -> ""
             Just p  -> ((p <> superscript) `isDescribedAsGen` theDescription)
                        <> ((p <> "̊" <> superscript) `isDescribedAsGen` theDescription)
                        <> ((p <> "̥" <> superscript) `isDescribedAsGen` theDescription)
     <> case voicedIPA of
          Nothing -> ""
          Just b ->
            (b <> "̊" <> superscript) `isDescribedAsGen` theDescription
            <> (b <> "̥" <> superscript) `isDescribedAsGen` theDescription



allSecondary
  :: SecondArticulationTestGenFunction
  -> Maybe Text -- ^ IPA representation of voiceless phoneme "t"
  -> Maybe Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
  -> Text -- ^ specifications of voiced, voiceless, and aspirated behaviour.
allSecondary func voicelessIPA voicedIPA place manner =
  func ""               ""  voicelessIPA voicedIPA place manner <>
  func "labialized"     "ʷ" voicelessIPA voicedIPA place manner <>
  func "palatalized"    "ʲ" voicelessIPA voicedIPA place manner <>
  func "velarized"      "ˠ" voicelessIPA voicedIPA place manner <>
  func "pharyngealized" "ˤ" voicelessIPA voicedIPA place manner


voicelessTestsGen
  :: Maybe Text -- ^ IPA representation of voiceless phoneme "t"
  -> Maybe Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
  -> Text -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicelessTestsGen =
  allSecondary voiceless2ndArticulationTestsGen



voicelessAspiratedTestsGen
  :: Maybe Text -- ^ IPA representation of voiceless phoneme "t"
  -> Maybe Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
  -> Text -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicelessAspiratedTestsGen =
  allSecondary voicelessAspirated2ndArticulationTestsGen

voicedTestsGen
  :: Maybe Text -- ^ IPA representation of voiceless phoneme "t"
  -> Maybe Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
  -> Text -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicedTestsGen =
  allSecondary voiced2ndArticulationTestsGen

voicedAspiratedTestsGen
  :: Maybe Text -- ^ IPA representation of voiceless phoneme "t"
  -> Maybe Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
  -> Text -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicedAspiratedTestsGen  =
  allSecondary voicedAspirated2ndArticulationGen

module_comment :: Text
module_comment = "\"\"\"\nunit tests for testing describing a phoneme given its \n"
               <> "representation in the international phonetic alphabet\n\"\"\""

imports :: Text
imports = "\nimport unittest\n\nfrom ipa import describe_transcription"

classDefinition :: Text
classDefinition = "\n\nclass IPATextToDescription(unittest.TestCase):\n"

preamble :: Text
preamble = module_comment <> imports <> classDefinition

generateConsonantTests
   :: Maybe Text -- ^ IPA representation of voiceless phoneme "t"
   -> Maybe Text -- ^ IPA representation of voiced phoneme "d"
   -> Text       -- ^ place of articulation (as text) e.g. "alveolar"
   -> Text       -- ^ manner of articulation (as text) e.g. "alveolar"
   -> Text       -- ^ specifications of voiced, voiceless, and aspirated behaviour.
generateConsonantTests voicelessIPA voicedIPA place manner =
   voicelessTestsGen voicelessIPA voicedIPA place manner <>
   voicelessAspiratedTestsGen voicelessIPA voicedIPA place manner <>
   voicedTestsGen voicelessIPA voicedIPA place manner <>
   voicedAspiratedTestsGen voicelessIPA voicedIPA place manner
   
pulmonicEgressiveConsonantGen :: Text
pulmonicEgressiveConsonantGen =
   generateConsonantTests (Just "p") (Just "b") "bilabial"      "plosive"             <>
   generateConsonantTests (Just "t") (Just "d") "alveolar"      "plosive"             <>
   generateConsonantTests (Just "ʈ") (Just "ɖ") "retroflex"     "plosive"             <>
   generateConsonantTests (Just "c") (Just "ɟ") "palatal"       "plosive"             <>
   generateConsonantTests (Just "k") (Just "g") "velar"         "plosive"             <>
   generateConsonantTests (Just "q") (Just "ɢ") "uvular"        "plosive"             <>
   generateConsonantTests (Just "ʔ") Nothing    "glottal"       "plosive"             <>
   generateConsonantTests (Just "ɸ") (Just "β") "bilabial"      "fricative"           <>
   generateConsonantTests (Just "f") (Just "v") "labio-dental"  "fricative"           <>
   generateConsonantTests (Just "θ") (Just "ð") "dental"        "fricative"           <>
   generateConsonantTests (Just "s") (Just "z") "alveolar"      "fricative"           <>
   generateConsonantTests (Just "ʃ") (Just "ʒ") "post-alveolar" "fricative"           <>
   generateConsonantTests (Just "ʂ") (Just "ʐ") "retroflex"     "fricative"           <>
   generateConsonantTests (Just "ç") (Just "ʝ") "palatal"       "fricative"           <>
   generateConsonantTests (Just "x") (Just "ɣ") "velar"         "fricative"           <>
   generateConsonantTests (Just "χ") (Just "ʁ") "uvular"        "fricative"           <>
   generateConsonantTests (Just "ħ") (Just "ʕ") "pharyngeal"    "fricative"           <>
   generateConsonantTests (Just "h") (Just "ɦ") "glottal"       "fricative"           <>
   generateConsonantTests (Just "ɬ") (Just "ɮ") "alveolar"      "lateral fricative"   <>
   generateConsonantTests Nothing    (Just "m") "bilabial"      "nasal"               <>
   generateConsonantTests Nothing    (Just "n") "alveolar"      "nasal"               <>
   generateConsonantTests Nothing    (Just "ɲ") "palatal"       "nasal"               <>
   generateConsonantTests Nothing    (Just "ɳ") "retroflex"     "nasal"               <>
   generateConsonantTests Nothing    (Just "ŋ") "velar"         "nasal"               <>
   generateConsonantTests Nothing    (Just "ɴ") "uvular"        "nasal"               <>
   generateConsonantTests Nothing    (Just "ʙ") "bilabial"      "trill"               <>
   generateConsonantTests Nothing    (Just "r") "alveolar"      "trill"               <>
   generateConsonantTests Nothing    (Just "ʀ") "uvular"        "trill"               <>
   generateConsonantTests Nothing    (Just "ⱱ") "labio-dental"  "tap or flap"         <>
   generateConsonantTests Nothing    (Just "ɾ") "alveolar"      "tap or flap"         <>
   generateConsonantTests Nothing    (Just "ɽ") "retroflex"     "tap or flap"         <>
   generateConsonantTests Nothing    (Just "ʋ") "labio-dental"  "approximant"         <>
   generateConsonantTests Nothing    (Just "ɹ") "alveolar"      "approximant"         <>
   generateConsonantTests Nothing    (Just "ɭ") "retroflex"     "lateral approximant" <>
   generateConsonantTests Nothing    (Just "ʎ") "palatal"       "lateral approximant" <>
   generateConsonantTests Nothing    (Just "ʟ") "velar"         "lateral approximant" <>

   generateConsonantTests Nothing    (Just "w") "labial-velar"    "approximant" <>
   generateConsonantTests (Just "ʍ") Nothing    "labial-velar"    "fricative"   <>
   generateConsonantTests (Just "ʡ") Nothing    "epiglottal"      "plosive"     <>
   generateConsonantTests (Just "ʜ") (Just "ʢ") "epiglottal"      "fricative"   <>
   generateConsonantTests (Just "ɕ") (Just "ʑ") "alveolo-palatal" "fricative"


generatedTestCode :: Text
generatedTestCode =
  "### Beginning of generated code.\n" <>
  preamble <>
  pulmonicEgressiveConsonantGen <>
  "### End of generated code.\n"
