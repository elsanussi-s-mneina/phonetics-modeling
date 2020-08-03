{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SpecGenerator where

import           Data.Maybe    (fromJust)
import           Test.Hspec    (Spec, describe, hspec, it, shouldBe)
import           IPA (analyzeIPA, ipaTextToPhonetListReport, voicedIPA, devoicedIPA, analyzeIPAToSPE, describeIPA)
import           PhoneticFeatures (isGlide)
import           Relude

import qualified Data.Text as T


generatedTestCode :: Text
generatedTestCode =
  pulmonicEgressiveConsonantGen



pulmonicEgressiveConsonantGen :: Text
pulmonicEgressiveConsonantGen =
      voicedVoicelessAspiratedTestsGen "p" "b" "bilabial" "plosive"
      <> voicedVoicelessAspiratedTestsGen "t" "d" "alveolar" "plosive"
      <> voicedVoicelessAspiratedTestsGen "ʈ" "ɖ" "retroflex" "plosive"
      <> voicedVoicelessAspiratedTestsGen "c" "ɟ" "palatal" "plosive"
      <> voicedVoicelessAspiratedTestsGen "k" "g" "velar" "plosive"
      <> voicedVoicelessAspiratedTestsGen "q" "ɢ" "uvular" "plosive"
      <> voicedVoicelessAspiratedTestsGen "ɸ" "β" "bilabial" "fricative"
      <> voicedVoicelessAspiratedTestsGen "f" "v" "labio-dental" "fricative"
      <> voicedVoicelessAspiratedTestsGen "θ" "ð" "dental" "fricative"
      <> voicedVoicelessAspiratedTestsGen "s" "z" "alveolar" "fricative"
      <> voicedVoicelessAspiratedTestsGen "ʃ" "ʒ" "post-alveolar" "fricative"
      <> voicedVoicelessAspiratedTestsGen "ʂ" "ʐ" "retroflex" "fricative"
      <> voicedVoicelessAspiratedTestsGen "ç" "ʝ" "palatal" "fricative"
      <> voicedVoicelessAspiratedTestsGen "x" "ɣ" "velar" "fricative"
      <> voicedVoicelessAspiratedTestsGen "χ" "ʁ" "uvular" "fricative"
      <> voicedVoicelessAspiratedTestsGen "ħ" "ʕ" "pharyngeal" "fricative"
      <> voicedVoicelessAspiratedTestsGen "h" "ɦ" "glottal" "fricative"
      <> voicedVoicelessAspiratedTestsGen "ɬ" "ɮ" "alveolar" "lateral fricative"


      <> voicedAndAspiratedTestsGen "m" "bilabial" "nasal"
      <> voicedAndAspiratedTestsGen "n" "alveolar" "nasal"
      <> voicedAndAspiratedTestsGen "ɲ" "palatal" "nasal"
      <> voicedAndAspiratedTestsGen "ɳ" "retroflex" "nasal"
      <> voicedAndAspiratedTestsGen "ŋ" "velar" "nasal"
      <> voicedAndAspiratedTestsGen "ɴ" "uvular" "nasal"

      <> voicedAndAspiratedTestsGen "ʙ" "bilabial" "trill"
      <> voicedAndAspiratedTestsGen "r" "alveolar" "trill"
      <> voicedAndAspiratedTestsGen "ʀ" "uvular" "trill"
      <> voicedAndAspiratedTestsGen "ⱱ" "labio-dental" "tap or flap"
      <> voicedAndAspiratedTestsGen "ɾ" "alveolar" "tap or flap"
      <> voicedAndAspiratedTestsGen "ɽ" "retroflex" "tap or flap"

      <> voicedAndAspiratedTestsGen "ʋ" "labio-dental" "approximant"
      <> voicedAndAspiratedTestsGen "ɹ" "alveolar" "approximant"
      <> voicedAndAspiratedTestsGen "ɭ" "retroflex" "lateral approximant"
      <> voicedAndAspiratedTestsGen "ʎ" "palatal" "lateral approximant"
      <> voicedAndAspiratedTestsGen "ʟ" "velar" "lateral approximant"

voicelessSecondaryArticulationTestsOneSymbolGen
  :: Text
  -> Text
  -> Text -- ^ IPA representation of voiced phoneme "m"
                                      -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                                      -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                                      -> Text
voicelessSecondaryArticulationTestsOneSymbolGen articulation2Text superscript basicVoicedIPA placeNameText mannerText =
  let theDescription = "voiceless " <> (if T.null articulation2Text then "" else articulation2Text <> " ") <>
                        placeNameText <> " " <> mannerText <> " pulmonic egressive consonant"
  in "describe (\"" <> theDescription <> "\") $ do\n"
     <> (basicVoicedIPA <> "̊" <> superscript) `isDescribedAsGen` theDescription
     <> (basicVoicedIPA <> "̥" <> superscript) `isDescribedAsGen` theDescription




voicelessTestsOneSymbolGen :: Text -- ^ IPA representation of voiced phoneme "m"
                        -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                        -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                        -> Text
voicelessTestsOneSymbolGen basicVoicedIPA placeNameText mannerText =
      voicelessSecondaryArticulationTestsOneSymbolGen "" "" basicVoicedIPA placeNameText mannerText
      <> voicelessSecondaryArticulationTestsOneSymbolGen "labialized" "ʷ" basicVoicedIPA placeNameText mannerText
      <> voicelessSecondaryArticulationTestsOneSymbolGen "palatalized" "ʲ" basicVoicedIPA placeNameText mannerText
      <> voicelessSecondaryArticulationTestsOneSymbolGen "velarized" "ˠ" basicVoicedIPA placeNameText mannerText
      <> voicelessSecondaryArticulationTestsOneSymbolGen "pharyngealized" "ˤ" basicVoicedIPA placeNameText mannerText


voicelessAspiratedSecondaryArticulationTestsOneSymbolGen :: Text -> Text
                                 -> Text -- ^ IPA representation of voiced phoneme "m"
                                 -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                                 -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                                 -> Text
voicelessAspiratedSecondaryArticulationTestsOneSymbolGen articulation2Name superscript basicVoicedIPA placeNameText mannerText =
  let theDescription = "voiceless aspirated " <> (if T.null articulation2Name then "" else articulation2Name <> " ") <> placeNameText <> " "
                     <> mannerText <> " pulmonic egressive consonant"
  in "describe (\"" <> theDescription <> "\") $ do\n" <>
       ((basicVoicedIPA <> "̥ʰ" <> superscript) `isDescribedAsGen` theDescription) <>
       ((basicVoicedIPA <> "̊ʰ" <> superscript) `isDescribedAsGen` theDescription)

voicelessAspiratedTestsOneSymbolGen
  :: 
     Text -- ^ IPA representation of voiced phoneme "m"
  -> Text -- ^ place of articulation (as text) e.g. "bilabial"
  -> Text -- ^ manner of articulation (as text) e.g. "plosive"
  -> Text
voicelessAspiratedTestsOneSymbolGen basicVoicedIPA placeNameText mannerText =
  voicelessAspiratedSecondaryArticulationTestsOneSymbolGen "" ""  basicVoicedIPA placeNameText mannerText
  <> voicelessAspiratedSecondaryArticulationTestsOneSymbolGen "labialized" "ʷ" basicVoicedIPA placeNameText mannerText
  <> voicelessAspiratedSecondaryArticulationTestsOneSymbolGen "palatalized" "ʲ" basicVoicedIPA placeNameText mannerText
  <> voicelessAspiratedSecondaryArticulationTestsOneSymbolGen "velarized" "ˠ" basicVoicedIPA placeNameText mannerText
  <> voicelessAspiratedSecondaryArticulationTestsOneSymbolGen "pharyngealized" "ˤ" basicVoicedIPA placeNameText mannerText

isDescribedAsGen
  :: Text -- ^ IPA e.g. "pʰ"
  -> Text -- ^ expected english description e.g. "voiceless aspirated bilabial plosive pulmonic egressive consonant"
  -> Text
(isDescribedAsGen) ipaText description =
    "  it (\"should be that: [" <> ipaText <> "] is the representation of the " <> description <> "\") $\n"
    <> "    describeIPA \"" <> ipaText <> "\" `shouldBe` \"" <> description <> "\"\n"

voicedSecondaryArticulationTestsOneSymbolGen
                     :: Text -- ^ name of secondary articulation e.g. "palatalized"
                     -> Text -- ^ superscript for secondary articulation e.g. "ʲ"
                     -> Text -- ^ IPA representation of voiced phoneme "m"
                     -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                     -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                     -> Text
voicedSecondaryArticulationTestsOneSymbolGen articulation2Name superscript  basicVoicedIPA placeNameText mannerText =
  let theDescription = "voiced " <> (if T.null articulation2Name then "" else articulation2Name <> " ") <> placeNameText <>
                       " " <> mannerText <> " pulmonic egressive consonant"
  in "describe (\"" <> theDescription <> "\") $ do\n"
     <> (basicVoicedIPA <> superscript) `isDescribedAsGen` theDescription


voicedTestsOneSymbolGen :: Text -- ^ IPA representation of voiced phoneme "m"
                     -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                     -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                     -> Text
voicedTestsOneSymbolGen basicVoicedIPA placeNameText mannerText =
  voicedSecondaryArticulationTestsOneSymbolGen "" ""  basicVoicedIPA placeNameText mannerText
  <> voicedSecondaryArticulationTestsOneSymbolGen "labialized" "ʷ" basicVoicedIPA placeNameText mannerText
  <> voicedSecondaryArticulationTestsOneSymbolGen "palatalized" "ʲ" basicVoicedIPA placeNameText mannerText
  <> voicedSecondaryArticulationTestsOneSymbolGen "velarized" "ˠ" basicVoicedIPA placeNameText mannerText
  <> voicedSecondaryArticulationTestsOneSymbolGen "pharyngealized" "ˤ" basicVoicedIPA placeNameText mannerText



voicedAspiratedTestsOneSymbolGen :: Text -- ^ IPA representation of voiced phoneme "m"
                                 -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                                 -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                                 -> Text
voicedAspiratedTestsOneSymbolGen basicVoicedIPA placeNameText mannerText =
  let theDescription = "voiced aspirated " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant"
  in "describe (\"" <> theDescription <> "\") $ do\n" <>
       (basicVoicedIPA <> "ʰ") `isDescribedAsGen` theDescription  <>
       (basicVoicedIPA <> "̬ʰ") `isDescribedAsGen` theDescription



-- | Use for phonemes that only have a character for the voiced phoneme,
-- and not for its voiceless counterpart
voicedAndAspiratedTestsGen :: Text -- ^ IPA representation of voiced phoneme "m"
                           -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                           -> Text -- ^ manner of articulation (as text) e.g. "nasal"
                           -> Text
voicedAndAspiratedTestsGen basicVoicedIPA placeNameText mannerText =
       voicelessTestsOneSymbolGen basicVoicedIPA placeNameText mannerText
      <> voicelessAspiratedTestsOneSymbolGen basicVoicedIPA placeNameText mannerText
      <> voicedTestsOneSymbolGen basicVoicedIPA placeNameText mannerText
      <> voicedAspiratedTestsOneSymbolGen basicVoicedIPA placeNameText mannerText




voicelessSecondaryArticulationTestsTwoSymbolsGen
                     :: Text -- ^ name of secondary articulation e.g. "palatalized"
                     -> Text -- ^ superscript for secondary articulation e.g. "ʲ"
                     -> Text -- ^ IPA representation of voiced phoneme "p"
                     -> Text -- ^ IPA representation of voiced phoneme "b"
                     -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                     -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                     -> Text
voicelessSecondaryArticulationTestsTwoSymbolsGen articulation2Name superscript basicVoicelessIPA basicVoicedIPA placeNameText mannerText =
  let theDescription = "voiceless " <> articulation2Name <>
                       (if T.null articulation2Name then "" else " ") <> placeNameText <> " "
                     <> mannerText <> " pulmonic egressive consonant"
  in "describe (\"" <> theDescription <> "\") $ do\n"
     <> (basicVoicelessIPA <> superscript)       `isDescribedAsGen` theDescription
     <> (basicVoicedIPA <> "̊" <> superscript)    `isDescribedAsGen` theDescription
     <> (basicVoicedIPA <> "̥" <> superscript)    `isDescribedAsGen` theDescription
     <> (basicVoicelessIPA <> "̊" <> superscript) `isDescribedAsGen` theDescription
     <> (basicVoicelessIPA <> "̥" <> superscript) `isDescribedAsGen` theDescription

voicelessTestsTwoSymbolsGen
  :: Text -- ^ IPA representation of voiceless phoneme "t"
                              -> Text -- ^ IPA representation of voiced phoneme "d"
                              -> Text -- ^ place of articulation (as text) e.g. "alveolar"
                              -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
                              -> Text -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicelessTestsTwoSymbolsGen basicVoicelessIPA basicVoicedIPA placeNameText mannerText =
  voicelessSecondaryArticulationTestsTwoSymbolsGen "" "" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <>
  voicelessSecondaryArticulationTestsTwoSymbolsGen "labialized" "ʷ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <>
  voicelessSecondaryArticulationTestsTwoSymbolsGen "palatalized" "ʲ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <>
  voicelessSecondaryArticulationTestsTwoSymbolsGen "velarized" "ˠ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <>
  voicelessSecondaryArticulationTestsTwoSymbolsGen "pharyngealized" "ˤ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText

voicelessSecondaryArticulationAspiratedTestsTwoSymbolsGen
  :: Text -- ^ articulation name, e.g. "palatalized"
  -> Text -- ^ superscript, e.g. "ʲ"
  -> Text -- ^ IPA representation of voiceless phoneme "t"
  -> Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "plosive"
  -> Text -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicelessSecondaryArticulationAspiratedTestsTwoSymbolsGen articulation2Name superscript basicVoicelessIPA basicVoicedIPA placeNameText mannerText =
  let theDescription = "voiceless aspirated " <> (if T.null articulation2Name then "" else articulation2Name <> " ")
                       <> placeNameText <>
                       " " <> mannerText <> " pulmonic egressive consonant"
  in "describe (\"" <> theDescription <> "\") $ do\n" <>
       (basicVoicelessIPA <> "ʰ" <> superscript) `isDescribedAsGen` theDescription <>
       (basicVoicedIPA    <> "̥ʰ" <> superscript) `isDescribedAsGen` theDescription <>
       (basicVoicedIPA    <> "̊ʰ" <> superscript) `isDescribedAsGen` theDescription


voicelessAspiratedTestsTwoSymbolsGen
  :: Text -- ^ IPA representation of voiceless phoneme "t"
  -> Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
  -> Text -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicelessAspiratedTestsTwoSymbolsGen basicVoicelessIPA basicVoicedIPA placeNameText mannerText =
  voicelessSecondaryArticulationAspiratedTestsTwoSymbolsGen "" "" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <> voicelessSecondaryArticulationAspiratedTestsTwoSymbolsGen "labialized" "ʷ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <> voicelessSecondaryArticulationAspiratedTestsTwoSymbolsGen "palatalized" "ʲ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <> voicelessSecondaryArticulationAspiratedTestsTwoSymbolsGen "velarized" "ˠ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <> voicelessSecondaryArticulationAspiratedTestsTwoSymbolsGen "pharyngealized" "ˤ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText


voicedSecondaryArticulationTestsTwoSymbolsGen
  :: Text -- ^ articulation name, e.g. "palatalized"
  -> Text -- ^ superscript, e.g. "ʲ"
  -> Text -- ^ IPA representation of voiceless phoneme "t"
  -> Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
  -> Text -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicedSecondaryArticulationTestsTwoSymbolsGen articulation2Name superscript basicVoicelessIPA basicVoicedIPA placeNameText mannerText =
  let theDescription = "voiced " <> articulation2Name <> (if T.null articulation2Name then "" else " ") <> placeNameText <> " " <> mannerText
                     <> " pulmonic egressive consonant"
  in "describe (\"" <> theDescription <> "\") $ do\n" <>
       (basicVoicedIPA <> superscript) `isDescribedAsGen` theDescription <>
       (basicVoicelessIPA <> "̬" <> superscript) `isDescribedAsGen` theDescription <>
       (basicVoicelessIPA <> "̬" <> superscript) `isDescribedAsGen` theDescription


voicedTestsTwoSymbolsGen
  :: Text -- ^ IPA representation of voiceless phoneme "t"
  -> Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
  -> Text -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicedTestsTwoSymbolsGen basicVoicelessIPA basicVoicedIPA placeNameText mannerText = do
  voicedSecondaryArticulationTestsTwoSymbolsGen "" "" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <>
  voicedSecondaryArticulationTestsTwoSymbolsGen "labialized" "ʷ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <>
  voicedSecondaryArticulationTestsTwoSymbolsGen "palatalized" "ʲ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <>
  voicedSecondaryArticulationTestsTwoSymbolsGen "velarized" "ˠ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <>
  voicedSecondaryArticulationTestsTwoSymbolsGen "pharyngealized" "ˤ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText


voicedAspiratedSecondaryArticulationTwoSymbolsGen
  :: Text -- ^ articulation name, e.g. "palatalized"
  -> Text -- ^ superscript, e.g. "ʲ"
  -> Text -- ^ IPA representation of voiceless phoneme "t"
  -> Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
  -> Text -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicedAspiratedSecondaryArticulationTwoSymbolsGen articulation2Name superscript basicVoicelessIPA basicVoicedIPA placeNameText mannerText =
  let theDescription = "voiced aspirated " <> articulation2Name <> (if T.null articulation2Name then "" else " ")
                     <> placeNameText <> " "
                     <> mannerText <> " pulmonic egressive consonant"
  in "describe (\"" <> theDescription <> "\") $ do\n" <>
       (basicVoicedIPA <> "ʰ" <> superscript) `isDescribedAsGen` theDescription <>
       (basicVoicedIPA <> "̬ʰ" <> superscript) `isDescribedAsGen` theDescription <>
       (basicVoicelessIPA <> "̬ʰ" <> superscript) `isDescribedAsGen` theDescription


voicedAspiratedTwoSymbolsGen
  :: Text -- ^ IPA representation of voiceless phoneme "t"
                              -> Text -- ^ IPA representation of voiced phoneme "d"
                              -> Text -- ^ place of articulation (as text) e.g. "alveolar"
                              -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
                              -> Text -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicedAspiratedTwoSymbolsGen basicVoicelessIPA basicVoicedIPA placeNameText mannerText =
  voicedAspiratedSecondaryArticulationTwoSymbolsGen "" "" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <>
  voicedAspiratedSecondaryArticulationTwoSymbolsGen "labialized" "ʷ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <>
  voicedAspiratedSecondaryArticulationTwoSymbolsGen "palatalized" "ʲ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <>
  voicedAspiratedSecondaryArticulationTwoSymbolsGen "velarized" "ˠ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  <>
  voicedAspiratedSecondaryArticulationTwoSymbolsGen "pharyngealized" "ˤ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText



voicedVoicelessAspiratedTestsGen :: Text -- ^ IPA representation of voiceless phoneme "t"
                                 -> Text -- ^ IPA representation of voiced phoneme "d"
                                 -> Text -- ^ place of articulation (as text) e.g. "alveolar"
                                 -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
                                 -> Text -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicedVoicelessAspiratedTestsGen basicVoicelessIPA basicVoicedIPA placeNameText mannerText =
      voicelessTestsTwoSymbolsGen basicVoicelessIPA basicVoicedIPA placeNameText mannerText
      <>
      voicelessAspiratedTestsTwoSymbolsGen basicVoicelessIPA basicVoicedIPA placeNameText mannerText
      <>
      voicedTestsTwoSymbolsGen basicVoicelessIPA basicVoicedIPA placeNameText mannerText
      <>
      voicedAspiratedTwoSymbolsGen basicVoicelessIPA basicVoicedIPA placeNameText mannerText


