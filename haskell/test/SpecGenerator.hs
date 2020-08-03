{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SpecGenerator where

import           Data.Maybe    (fromJust)
import           Test.Hspec    (Spec, describe, hspec, it, shouldBe)
import           IPA (analyzeIPA, ipaTextToPhonetListReport, voicedIPA, devoicedIPA, analyzeIPAToSPE, describeIPA)
import           PhoneticFeatures (isGlide)
import           Relude

import qualified Data.Text as T


isDescribedAsGen
  :: Text -- ^ IPA e.g. "pʰ"
  -> Text -- ^ expected english description e.g. "voiceless aspirated bilabial plosive pulmonic egressive consonant"
  -> Text
(isDescribedAsGen) ipaText description =
    "  it \"should be that: [" <> ipaText <> "]\\\n     \\is the representation of the\\\n     \\" <> description <> "\" $\n"
    <> "    describeIPA \"" <> ipaText <> "\"\n      `shouldBe` \n      \"" <> description <> "\"\n"


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
  in "describe \"" <> theDescription <> "\" $\n  do\n" <>
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
  in "describe \"" <> theDescription <> "\" $\n  do\n" <>
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
  in "describe \"" <> theDescription <> "\" $\n  do\n" <>
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
  in "describe \"" <> theDescription <> "\" $\n  do\n"
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
   generateConsonantTests Nothing    (Just "ʟ") "velar"         "lateral approximant"

generatedTestCode :: Text
generatedTestCode =
  pulmonicEgressiveConsonantGen

