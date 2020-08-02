{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Spec(main) where

import           Data.Maybe    (fromJust)
import           Test.Hspec    (Spec, describe, hspec, it, shouldBe)
import           IPA (analyzeIPA, ipaTextToPhonetListReport, voicedIPA, devoicedIPA, analyzeIPAToSPE, describeIPA)
import           PhoneticFeatures (isGlide)
import           Relude

main = do
  hspec pulmonicEgressiveConsonantSpec
  hspec glideSpec
  hspec ipaTextToPhonetListReportSpec
  hspec voicingSpec
  hspec analyzeIPAToSPESpec
  hspec secondaryArticulationSpec
  hspec vowelLengthSpec
  hspec pulmonicEgressiveConsonantSpec_oldStyle

pulmonicEgressiveConsonantSpec_oldStyle :: Spec
pulmonicEgressiveConsonantSpec_oldStyle = do
  describe "pulmonic egressive consonants (old way):" $ do
    "p" `isDescribedAs` "voiceless bilabial plosive pulmonic egressive consonant"
    "b̥" `isDescribedAs` "voiceless bilabial plosive pulmonic egressive consonant"
    "b̊" `isDescribedAs` "voiceless bilabial plosive pulmonic egressive consonant"
    "pʷ" `isDescribedAs` "voiceless labialized bilabial plosive pulmonic egressive consonant"
    "b̥ʷ" `isDescribedAs` "voiceless labialized bilabial plosive pulmonic egressive consonant"
    "b̊ʷ" `isDescribedAs` "voiceless labialized bilabial plosive pulmonic egressive consonant"
    "pʰ" `isDescribedAs` "voiceless aspirated bilabial plosive pulmonic egressive consonant"
    "b̥ʰ" `isDescribedAs` "voiceless aspirated bilabial plosive pulmonic egressive consonant"
    "b̊ʰ" `isDescribedAs` "voiceless aspirated bilabial plosive pulmonic egressive consonant"


pulmonicEgressiveConsonantSpec :: Spec
pulmonicEgressiveConsonantSpec = do
  describe "pulmonic egressive consonants:" $ do
      voicedVoicelessAspiratedTests "p" "b" "bilabial" "plosive"
      voicedVoicelessAspiratedTests "t" "d" "alveolar" "plosive"
      voicedVoicelessAspiratedTests "ʈ" "ɖ" "retroflex" "plosive"
      voicedVoicelessAspiratedTests "c" "ɟ" "palatal" "plosive"
      voicedVoicelessAspiratedTests "k" "g" "velar" "plosive"
      voicedVoicelessAspiratedTests "q" "ɢ" "uvular" "plosive"
      describe "voiceless glottal plosive in International Phonetic Alphabet" $ do
        "ʔ" `isDescribedAs` "voiceless glottal plosive pulmonic egressive consonant"
        "ʔ̊" `isDescribedAs` "voiceless glottal plosive pulmonic egressive consonant"
        "ʔ̥" `isDescribedAs` "voiceless glottal plosive pulmonic egressive consonant"
      describe "voiceless aspirated glottal plosive in International Phonetic Alphabet" $ do
        "ʔʰ" `isDescribedAs` "voiceless aspirated glottal plosive pulmonic egressive consonant"
        "ʔ̥ʰ" `isDescribedAs` "voiceless aspirated glottal plosive pulmonic egressive consonant"
        "ʔ̊ʰ" `isDescribedAs` "voiceless aspirated glottal plosive pulmonic egressive consonant"
        "ʔ̬" `isDescribedAs` "voiced glottal plosive pulmonic egressive consonant"
      describe "voiced aspirated glottal plosive in International Phonetic Alphabet" $ do
        "ʔ̬ʰ" `isDescribedAs` "voiced aspirated glottal plosive pulmonic egressive consonant"

      voicedVoicelessAspiratedTests "ɸ" "β" "bilabial" "fricative"
      voicedVoicelessAspiratedTests "f" "v" "labio-dental" "fricative"
      voicedVoicelessAspiratedTests "θ" "ð" "dental" "fricative"
      voicedVoicelessAspiratedTests "s" "z" "alveolar" "fricative"
      voicedVoicelessAspiratedTests "ʃ" "ʒ" "post-alveolar" "fricative"
      voicedVoicelessAspiratedTests "ʂ" "ʐ" "retroflex" "fricative"
      voicedVoicelessAspiratedTests "ç" "ʝ" "palatal" "fricative"
      voicedVoicelessAspiratedTests "x" "ɣ" "velar" "fricative"
      voicedVoicelessAspiratedTests "χ" "ʁ" "uvular" "fricative"
      voicedVoicelessAspiratedTests "ħ" "ʕ" "pharyngeal" "fricative"
      voicedVoicelessAspiratedTests "h" "ɦ" "glottal" "fricative"
      voicedVoicelessAspiratedTests "ɬ" "ɮ" "alveolar" "lateral fricative"


      voicedAndAspiratedTests "m" "bilabial" "nasal"
      voicedAndAspiratedTests "n" "alveolar" "nasal"
      voicedAndAspiratedTests "ɲ" "palatal" "nasal"
      voicedAndAspiratedTests "ɳ" "retroflex" "nasal"
      voicedAndAspiratedTests "ŋ" "velar" "nasal"
      voicedAndAspiratedTests "ɴ" "uvular" "nasal"

      voicedAndAspiratedTests "ʙ" "bilabial" "trill"
      voicedAndAspiratedTests "r" "alveolar" "trill"
      voicedAndAspiratedTests "ʀ" "uvular" "trill"
      voicedAndAspiratedTests "ⱱ" "labio-dental" "tap or flap"
      voicedAndAspiratedTests "ɾ" "alveolar" "tap or flap"
      voicedAndAspiratedTests "ɽ" "retroflex" "tap or flap"

      voicedAndAspiratedTests "ʋ" "labio-dental" "approximant"
      voicedAndAspiratedTests "ɹ" "alveolar" "approximant"
      voicedAndAspiratedTests "ɭ" "retroflex" "lateral approximant"
      voicedAndAspiratedTests "ʎ" "palatal" "lateral approximant"
      voicedAndAspiratedTests "ʟ" "velar" "lateral approximant"


voicelessPlainTestsOneSymbol :: Text -- ^ IPA representation of voiced phoneme "m"
                        -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                        -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                        -> Spec
voicelessPlainTestsOneSymbol basicVoicedIPA placeNameText mannerText = do
  describe ("voiceless " ++ toString placeNameText ++ " " <> toString mannerText <> " in International Phonetic Alphabet") $ do
    (basicVoicedIPA <> "̊") `isDescribedAs` ("voiceless " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")
    (basicVoicedIPA <> "̥") `isDescribedAs` ("voiceless " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")


voicelessLabializedTestsOneSymbol :: Text -- ^ IPA representation of voiced phoneme "m"
                        -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                        -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                        -> Spec
voicelessLabializedTestsOneSymbol basicVoicedIPA placeNameText mannerText = do
  describe ("voiceless labialized" ++ toString placeNameText ++ " " <> toString mannerText <> " in International Phonetic Alphabet") $ do
    (basicVoicedIPA <> "̊" <> "ʷ") `isDescribedAs` ("voiceless labialized " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")
    (basicVoicedIPA <> "̥" <> "ʷ") `isDescribedAs` ("voiceless labialized " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")

voicelessPharyngealizedTestsOneSymbol :: Text -- ^ IPA representation of voiced phoneme "m"
                                      -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                                      -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                                      -> Spec
voicelessPharyngealizedTestsOneSymbol basicVoicedIPA placeNameText mannerText = do
  describe ("voiceless pharyngealized" ++ toString placeNameText ++ " " <> toString mannerText <> " in International Phonetic Alphabet") $ do
    (basicVoicedIPA <> "̊" <> "ˤ") `isDescribedAs` ("voiceless pharyngealized " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")
    (basicVoicedIPA <> "̥" <> "ˤ") `isDescribedAs` ("voiceless pharyngealized " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")


voicelessPalatalizedTestsOneSymbol :: Text -- ^ IPA representation of voiced phoneme "m"
                                      -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                                      -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                                      -> Spec
voicelessPalatalizedTestsOneSymbol basicVoicedIPA placeNameText mannerText = do
  describe ("voiceless palatalized" ++ toString placeNameText ++ " " <> toString mannerText <> " in International Phonetic Alphabet") $ do
     (basicVoicedIPA <> "̊" <> "ʲ") `isDescribedAs` ("voiceless palatalized " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")
     (basicVoicedIPA <> "̥" <> "ʲ") `isDescribedAs` ("voiceless palatalized " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")

voicelessVelarizedTestsOneSymbol :: Text -- ^ IPA representation of voiced phoneme "m"
                                      -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                                      -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                                      -> Spec
voicelessVelarizedTestsOneSymbol basicVoicedIPA placeNameText mannerText = do
  describe ("voiceless velarized" ++ toString placeNameText ++ " " <> toString mannerText <> " in International Phonetic Alphabet") $ do
    (basicVoicedIPA <> "̊" <> "ˠ") `isDescribedAs` ("voiceless velarized " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")
    (basicVoicedIPA <> "̥" <> "ˠ") `isDescribedAs` ("voiceless velarized " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")

voicelessTestsOneSymbol :: Text -- ^ IPA representation of voiced phoneme "m"
                        -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                        -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                        -> Spec
voicelessTestsOneSymbol basicVoicedIPA placeNameText mannerText = do
      voicelessPlainTestsOneSymbol basicVoicedIPA placeNameText mannerText
      voicelessLabializedTestsOneSymbol basicVoicedIPA placeNameText mannerText
      voicelessPalatalizedTestsOneSymbol basicVoicedIPA placeNameText mannerText
      voicelessVelarizedTestsOneSymbol basicVoicedIPA placeNameText mannerText
      voicelessPharyngealizedTestsOneSymbol basicVoicedIPA placeNameText mannerText


voicelessAspiratedPlainTestsOneSymbol :: Text -- ^ IPA representation of voiced phoneme "m"
                                 -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                                 -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                                 -> Spec
voicelessAspiratedPlainTestsOneSymbol basicVoicedIPA placeNameText mannerText = do
  describe ("voiceless aspirated " <> toString placeNameText <> " " <> toString mannerText <> " in International Phonetic Alphabet") $ do
      (basicVoicedIPA <> "̥ʰ") `isDescribedAs` ("voiceless aspirated " <> placeNameText <>
                                   " " <> mannerText <> " pulmonic egressive consonant")
      (basicVoicedIPA <> "̊ʰ") `isDescribedAs` ("voiceless aspirated " <> placeNameText <>
                                   " " <> mannerText <> " pulmonic egressive consonant")


voicelessAspiratedLabializedTestsOneSymbol :: Text -- ^ IPA representation of voiced phoneme "m"
                                 -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                                 -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                                 -> Spec
voicelessAspiratedLabializedTestsOneSymbol basicVoicedIPA placeNameText mannerText = do
  describe ("voiceless aspirated labialized " <> toString placeNameText <> " " <> toString mannerText <> " in International Phonetic Alphabet") $ do
      (basicVoicedIPA <> "̥ʰ" <> "ʷ") `isDescribedAs` ("voiceless aspirated labialized " <> placeNameText <>
                                   " " <> mannerText <> " pulmonic egressive consonant")
      (basicVoicedIPA <> "̊ʰ" <> "ʷ") `isDescribedAs` ("voiceless aspirated labialized " <> placeNameText <>
                                   " " <> mannerText <> " pulmonic egressive consonant")


voicelessAspiratedPalatalizedTestsOneSymbol :: Text -- ^ IPA representation of voiced phoneme "m"
                                 -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                                 -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                                 -> Spec
voicelessAspiratedPalatalizedTestsOneSymbol basicVoicedIPA placeNameText mannerText = do
  describe ("voiceless aspirated palatalized " <> toString placeNameText <> " " <> toString mannerText <> " in International Phonetic Alphabet") $ do
      (basicVoicedIPA <> "̥ʰ" <> "ʲ") `isDescribedAs` ("voiceless aspirated palatalized " <> placeNameText <>
                                   " " <> mannerText <> " pulmonic egressive consonant")
      (basicVoicedIPA <> "̊ʰ" <> "ʲ") `isDescribedAs` ("voiceless aspirated palatalized " <> placeNameText <>
                                   " " <> mannerText <> " pulmonic egressive consonant")


voicelessAspiratedVelarizedTestsOneSymbol :: Text -- ^ IPA representation of voiced phoneme "m"
                                 -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                                 -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                                 -> Spec
voicelessAspiratedVelarizedTestsOneSymbol basicVoicedIPA placeNameText mannerText = do
  let theDescription = "voiceless aspirated velarized " <> placeNameText <>
                                                          " " <> mannerText <> " pulmonic egressive consonant"
  in describe ("voiceless aspirated velarized " <> toString placeNameText <> " " <> toString mannerText <> " in International Phonetic Alphabet") $ do
       (basicVoicedIPA <> "̥ʰ" <> "ˠ") `isDescribedAs` theDescription
       (basicVoicedIPA <> "̊ʰ" <> "ˠ") `isDescribedAs` theDescription

voicelessAspiratedPharyngealizedTestsOneSymbol :: Text -- ^ IPA representation of voiced phoneme "m"
                                 -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                                 -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                                 -> Spec
voicelessAspiratedPharyngealizedTestsOneSymbol basicVoicedIPA placeNameText mannerText = do
  let theDescription = "voiceless aspirated pharyngealized " <> placeNameText <>
                                                        " " <> mannerText <> " pulmonic egressive consonant"
  in describe ("voiceless aspirated pharyngealized " <> toString placeNameText <> " " <> toString mannerText <> " in International Phonetic Alphabet") $ do
       (basicVoicedIPA <> "̥ʰ" <> "ˤ") `isDescribedAs` theDescription
       (basicVoicedIPA <> "̊ʰ" <> "ˤ") `isDescribedAs` theDescription


voicelessAspiratedTestsOneSymbol :: Text -- ^ IPA representation of voiced phoneme "m"
                                 -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                                 -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                                 -> Spec
voicelessAspiratedTestsOneSymbol basicVoicedIPA placeNameText mannerText = do
  voicelessAspiratedPlainTestsOneSymbol basicVoicedIPA placeNameText mannerText
  voicelessAspiratedLabializedTestsOneSymbol basicVoicedIPA placeNameText mannerText
  voicelessAspiratedPalatalizedTestsOneSymbol basicVoicedIPA placeNameText mannerText
  voicelessAspiratedVelarizedTestsOneSymbol basicVoicedIPA placeNameText mannerText
  voicelessAspiratedPharyngealizedTestsOneSymbol basicVoicedIPA placeNameText mannerText

voicedPlainTestsOneSymbol :: Text -- ^ IPA representation of voiced phoneme "m"
                     -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                     -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                     -> Spec
voicedPlainTestsOneSymbol basicVoicedIPA placeNameText mannerText = do
  let theDescription = "voiced " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant"
  in describe (theDescription <> " in International Phonetic Alphabet") $ do
       basicVoicedIPA `isDescribedAs` theDescription

isDescribedAs
  :: Text -- ^ IPA e.g. "pʰ"
  -> Text -- ^ expected english description e.g. "voiceless aspirated bilabial plosive pulmonic egressive consonant"
  -> Spec
(isDescribedAs) ipaText description =
    it ("should be that: [" <> toString ipaText <> "] is the representation of the " <> toString description) $
       describeIPA ipaText `shouldBe` description

voicedSecondaryArticulationTestsOneSymbol
                     :: Text -- ^ name of secondary articulation e.g. "palatalized"
                     -> Text -- ^ superscript for secondary articulation e.g. "ʲ"
                     -> Text -- ^ IPA representation of voiced phoneme "m"
                     -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                     -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                     -> Spec
voicedSecondaryArticulationTestsOneSymbol articulation2Name superscript  basicVoicedIPA placeNameText mannerText = do
  let theDescription = "voiced " <> articulation2Name <> " " <> placeNameText <>
                       " " <> mannerText <> " pulmonic egressive consonant"
  in describe (toString theDescription <> " in International Phonetic Alphabet") $ do
      (basicVoicedIPA <> superscript) `isDescribedAs` theDescription


voicedTestsOneSymbol :: Text -- ^ IPA representation of voiced phoneme "m"
                     -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                     -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                     -> Spec
voicedTestsOneSymbol basicVoicedIPA placeNameText mannerText = do
  voicedPlainTestsOneSymbol basicVoicedIPA placeNameText mannerText
  voicedSecondaryArticulationTestsOneSymbol "labialized" "ʷ" basicVoicedIPA placeNameText mannerText
  voicedSecondaryArticulationTestsOneSymbol "palatalized" "ʲ" basicVoicedIPA placeNameText mannerText
  voicedSecondaryArticulationTestsOneSymbol "velarized" "ˠ" basicVoicedIPA placeNameText mannerText
  voicedSecondaryArticulationTestsOneSymbol "pharyngealized" "ˤ" basicVoicedIPA placeNameText mannerText




voicedAspiratedTestsOneSymbol :: Text -- ^ IPA representation of voiced phoneme "m"
                              -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                              -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                              -> Spec
voicedAspiratedTestsOneSymbol basicVoicedIPA placeNameText mannerText = do
  let theDescription = "voiced aspirated " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant"
  in describe ("voiced aspirated " <> toString placeNameText <> " plosive in International Phonetic Alphabet") $ do
       (basicVoicedIPA <> "ʰ") `isDescribedAs` theDescription
       (basicVoicedIPA <> "̬ʰ") `isDescribedAs` theDescription


-- | Use for phonemes that only have a character for the voiced phoneme,
-- and not for its voiceless counterpart
voicedAndAspiratedTests :: Text -- ^ IPA representation of voiced phoneme "m"
                        -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                        -> Text -- ^ manner of articulation (as text) e.g. "nasal"
                        -> Spec
voicedAndAspiratedTests basicVoicedIPA placeNameText mannerText = do
   describe (toString placeNameText) $ do
      voicelessTestsOneSymbol basicVoicedIPA placeNameText mannerText
      voicelessAspiratedTestsOneSymbol basicVoicedIPA placeNameText mannerText
      voicedTestsOneSymbol basicVoicedIPA placeNameText mannerText
      voicedAspiratedTestsOneSymbol basicVoicedIPA placeNameText mannerText




voicelessSecondaryArticulationTestsTwoSymbols
                     :: Text -- ^ name of secondary articulation e.g. "palatalized"
                     -> Text -- ^ superscript for secondary articulation e.g. "ʲ"
                     -> Text -- ^ IPA representation of voiced phoneme "p"
                     -> Text -- ^ IPA representation of voiced phoneme "b"
                     -> Text -- ^ place of articulation (as text) e.g. "bilabial"
                     -> Text -- ^ manner of articulation (as text) e.g. "plosive"
                     -> Spec
voicelessSecondaryArticulationTestsTwoSymbols articulation2Name superscript basicVoicelessIPA basicVoicedIPA placeNameText mannerText = do
  let theDescription = "voiceless " <> articulation2Name <> " " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant"
  in describe ("voiceless " ++ toString placeNameText ++ " " <> toString mannerText <>
               " in International Phonetic Alphabet") $ do
       (basicVoicelessIPA <> superscript) `isDescribedAs` theDescription
       (basicVoicedIPA <> "̊" <> superscript) `isDescribedAs` theDescription
       (basicVoicedIPA <> "̥" <> superscript) `isDescribedAs` theDescription
       (basicVoicelessIPA <> "̊" <> superscript) `isDescribedAs` theDescription
       (basicVoicelessIPA <> "̥" <> superscript) `isDescribedAs` theDescription

voicelessPlainTestsTwoSymbols
  :: Text -- ^ IPA representation of voiceless phoneme "t"
                              -> Text -- ^ IPA representation of voiced phoneme "d"
                              -> Text -- ^ place of articulation (as text) e.g. "alveolar"
                              -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
                              -> Spec -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicelessPlainTestsTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText = do
  let theDescription = "voiceless " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant"

  in describe ("voiceless " ++ toString placeNameText ++ " " <> toString mannerText <>
               " in International Phonetic Alphabet") $ do
       basicVoicelessIPA `isDescribedAs` theDescription
       (basicVoicedIPA <> "̊") `isDescribedAs` theDescription
       (basicVoicedIPA <> "̥") `isDescribedAs` theDescription
       (basicVoicelessIPA <> "̊") `isDescribedAs` theDescription
       (basicVoicelessIPA <> "̥") `isDescribedAs` theDescription

voicelessTestsTwoSymbols
  :: Text -- ^ IPA representation of voiceless phoneme "t"
                              -> Text -- ^ IPA representation of voiced phoneme "d"
                              -> Text -- ^ place of articulation (as text) e.g. "alveolar"
                              -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
                              -> Spec -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicelessTestsTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText = do
  voicelessPlainTestsTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicelessSecondaryArticulationTestsTwoSymbols "labialized" "ʷ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicelessSecondaryArticulationTestsTwoSymbols "palatalized" "ʲ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicelessSecondaryArticulationTestsTwoSymbols "velarized" "ˠ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicelessSecondaryArticulationTestsTwoSymbols "pharyngealized" "ˤ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText

voicelessAspiratedPlainTestsTwoSymbols
  :: Text -- ^ IPA representation of voiceless phoneme "t"
                              -> Text -- ^ IPA representation of voiced phoneme "d"
                              -> Text -- ^ place of articulation (as text) e.g. "alveolar"
                              -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
                              -> Spec -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicelessAspiratedPlainTestsTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText =
  let theDescription = "voiceless aspirated " <> placeNameText <>
                       " " <> mannerText <> " pulmonic egressive consonant"
  in describe (toString theDescription <> " in International Phonetic Alphabet") $ do
          (basicVoicelessIPA <> "ʰ") `isDescribedAs` theDescription
          (basicVoicedIPA <> "̥ʰ") `isDescribedAs` theDescription
          (basicVoicedIPA <> "̊ʰ") `isDescribedAs` theDescription

voicelessSecondaryArticulationAspiratedTestsTwoSymbols
  :: Text -- ^ articulation name, e.g. "palatalized"
  -> Text -- ^ superscript, e.g. "ʲ"
  -> Text -- ^ IPA representation of voiceless phoneme "t"
  -> Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "plosive"
  -> Spec -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicelessSecondaryArticulationAspiratedTestsTwoSymbols articulation2Name superscript basicVoicelessIPA basicVoicedIPA placeNameText mannerText =
  let theDescription = "voiceless aspirated " <> articulation2Name <> " " <> placeNameText <>
                       " " <> mannerText <> " pulmonic egressive consonant"
  in describe (toString theDescription <> " in International Phonetic Alphabet") $ do
       (basicVoicelessIPA <> "ʰ" <> superscript) `isDescribedAs` theDescription
       (basicVoicedIPA    <> "̥ʰ" <> superscript) `isDescribedAs` theDescription
       (basicVoicedIPA    <> "̊ʰ" <> superscript) `isDescribedAs` theDescription

voicelessAspiratedTestsTwoSymbols
  :: Text -- ^ IPA representation of voiceless phoneme "t"
  -> Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
  -> Spec -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicelessAspiratedTestsTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText = do
  voicelessAspiratedPlainTestsTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicelessSecondaryArticulationAspiratedTestsTwoSymbols "labialized" "ʷ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicelessSecondaryArticulationAspiratedTestsTwoSymbols "palatalized" "ʲ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicelessSecondaryArticulationAspiratedTestsTwoSymbols "velarized" "ˠ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicelessSecondaryArticulationAspiratedTestsTwoSymbols "pharyngealized" "ˤ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText

voicedPlainTestsTwoSymbols
  :: Text -- ^ IPA representation of voiceless phoneme "t"
                              -> Text -- ^ IPA representation of voiced phoneme "d"
                              -> Text -- ^ place of articulation (as text) e.g. "alveolar"
                              -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
                              -> Spec -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicedPlainTestsTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText =
      describe ("voiced " <> toString placeNameText <> " " <> toString mannerText <> " in International Phonetic Alphabet") $ do
           basicVoicedIPA `isDescribedAs` ("voiced " <> placeNameText <>
                                                  " " <> mannerText <> " pulmonic egressive consonant")
           (basicVoicelessIPA <> "̬") `isDescribedAs` ("voiced " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")
           (basicVoicedIPA <> "̬") `isDescribedAs` ("voiced " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")


voicedSecondaryArticulationTestsTwoSymbols
  :: Text -- ^ articulation name, e.g. "palatalized"
  -> Text -- ^ superscript, e.g. "ʲ"
  -> Text -- ^ IPA representation of voiceless phoneme "t"
  -> Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
  -> Spec -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicedSecondaryArticulationTestsTwoSymbols articulation2Name superscript basicVoicelessIPA basicVoicedIPA placeNameText mannerText =
      describe ("voiced " <> toString articulation2Name <> " " <> toString placeNameText <> " " <> toString mannerText <> " in International Phonetic Alphabet") $ do
           (basicVoicedIPA <> superscript) `isDescribedAs` ("voiced " <> articulation2Name <> " " <> placeNameText <>
                                                  " " <> mannerText <> " pulmonic egressive consonant")
           (basicVoicelessIPA <> "̬" <> superscript) `isDescribedAs` ("voiced " <> articulation2Name <> " " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")
           (basicVoicelessIPA <> "̬" <> superscript) `isDescribedAs` ("voiced " <> articulation2Name <> " " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")

voicedTestsTwoSymbols
  :: Text -- ^ IPA representation of voiceless phoneme "t"
  -> Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
  -> Spec -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicedTestsTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText = do
  voicedPlainTestsTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicedSecondaryArticulationTestsTwoSymbols "labialized" "ʷ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicedSecondaryArticulationTestsTwoSymbols "palatalized" "ʲ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicedSecondaryArticulationTestsTwoSymbols "velarized" "ˠ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicedSecondaryArticulationTestsTwoSymbols "pharyngealized" "ˤ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText

voicedAspiratedPlainTwoSymbols
  :: Text -- ^ IPA representation of voiceless phoneme "t"
  -> Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
  -> Spec -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicedAspiratedPlainTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText =
  describe ("voiced aspirated " <> toString placeNameText <> " plosive in International Phonetic Alphabet") $ do
      (basicVoicedIPA <> "ʰ") `isDescribedAs` ("voiced aspirated " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")
      (basicVoicedIPA <> "̬ʰ") `isDescribedAs` ("voiced aspirated " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")
      (basicVoicelessIPA <> "̬ʰ") `isDescribedAs` ("voiced aspirated " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")

voicedAspiratedSecondaryArticulationTwoSymbols
  :: Text -- ^ articulation name, e.g. "palatalized"
  -> Text -- ^ superscript, e.g. "ʲ"
  -> Text -- ^ IPA representation of voiceless phoneme "t"
  -> Text -- ^ IPA representation of voiced phoneme "d"
  -> Text -- ^ place of articulation (as text) e.g. "alveolar"
  -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
  -> Spec -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicedAspiratedSecondaryArticulationTwoSymbols articulation2Name superscript basicVoicelessIPA basicVoicedIPA placeNameText mannerText =
      describe (" voiced aspirated " <> toString articulation2Name <> " " <> toString placeNameText <> " plosive in International Phonetic Alphabet") $ do
          (basicVoicedIPA <> "ʰ" <> superscript) `isDescribedAs` ("voiced aspirated " <> articulation2Name <> " " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")
          (basicVoicedIPA <> "̬ʰ" <> superscript) `isDescribedAs` ("voiced aspirated " <> articulation2Name <> " " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")
          (basicVoicelessIPA <> "̬ʰ" <> superscript) `isDescribedAs` ("voiced aspirated " <> articulation2Name <> " " <> placeNameText <> " " <> mannerText <> " pulmonic egressive consonant")

voicedAspiratedTwoSymbols
  :: Text -- ^ IPA representation of voiceless phoneme "t"
                              -> Text -- ^ IPA representation of voiced phoneme "d"
                              -> Text -- ^ place of articulation (as text) e.g. "alveolar"
                              -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
                              -> Spec -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicedAspiratedTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText = do
  voicedAspiratedPlainTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicedAspiratedSecondaryArticulationTwoSymbols "labialized" "ʷ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicedAspiratedSecondaryArticulationTwoSymbols "palatalized" "ʲ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicedAspiratedSecondaryArticulationTwoSymbols "velarized" "ˠ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText
  voicedAspiratedSecondaryArticulationTwoSymbols "pharyngealized" "ˤ" basicVoicelessIPA basicVoicedIPA placeNameText mannerText

voicedVoicelessAspiratedTests :: Text -- ^ IPA representation of voiceless phoneme "t"
                              -> Text -- ^ IPA representation of voiced phoneme "d"
                              -> Text -- ^ place of articulation (as text) e.g. "alveolar"
                              -> Text -- ^ manner of articulation (as text) e.g. "alveolar"
                              -> Spec -- ^ specifications of voiced, voiceless, and aspirated behaviour.
voicedVoicelessAspiratedTests basicVoicelessIPA basicVoicedIPA placeNameText mannerText = do
   describe (toString placeNameText) $ do
      voicelessTestsTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText
      voicelessAspiratedTestsTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText
      voicedTestsTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText
      voicedAspiratedTwoSymbols basicVoicelessIPA basicVoicedIPA placeNameText mannerText

glideSpec :: Spec
glideSpec =
  describe "recognizing a glide" $ do
    it "should be that: [j] the voiced palatal approximant is a glide." $
          (isGlide . fromJust . analyzeIPA) "j" `shouldBe` True
    it "should be that: [ʝ] the voiced palatal fricative is not a glide." $
          (isGlide . fromJust . analyzeIPA) "ʝ" `shouldBe` False
    it "should be that: [w] is a glide." $
          (isGlide . fromJust . analyzeIPA) "w" `shouldBe` True
    it "should be that: [c] is not a glide." $
          (isGlide . fromJust . analyzeIPA) "c" `shouldBe` False
    it "should be that: [ɥ] is a glide." $
          (isGlide . fromJust . analyzeIPA) "ɥ" `shouldBe` True

ipaTextToPhonetListReportSpec :: Spec
ipaTextToPhonetListReportSpec =
  describe "one phoneme test" $ do
    it "should be that [j] is the voiced palatal approximant pulmonic egressive consonant" $
        ipaTextToPhonetListReport "j" `shouldBe` "/j/ voiced palatal approximant pulmonic egressive consonant\n"
    it "should be that [kc] has two lines" $ 
        ipaTextToPhonetListReport "kc" `shouldBe` ("/k/ voiceless velar plosive pulmonic egressive consonant\n"
          <> "/c/ voiceless palatal plosive pulmonic egressive consonant\n")
    it "should be that [t͡ʃ] is the voiceless post-alveolar affricate pulmonic egressive consonant" $
        ipaTextToPhonetListReport "t͡ʃ" `shouldBe` "/t͡ʃ/ voiceless post-alveolar affricate pulmonic egressive consonant\n"
    it "should be that [t͜ʃ] is also the voiceless post-alveolar affricate pulmonic egressive consonant" $
        ipaTextToPhonetListReport "t͜ʃ" `shouldBe` "/t͜ʃ/ voiceless post-alveolar affricate pulmonic egressive consonant\n"
    it "should be that (tʃ) with no tie bar is 2 phonemes." $
        ipaTextToPhonetListReport "tʃ" `shouldBe` "/t/ voiceless alveolar plosive pulmonic egressive consonant\n/ʃ/ voiceless post-alveolar fricative pulmonic egressive consonant\n"

    it "should be that (t͜ʃdd͜ʒ) is properly split into 3 phonemes" $
        ipaTextToPhonetListReport "t͜ʃdd͜ʒ" `shouldBe` "/t͜ʃ/ voiceless post-alveolar affricate pulmonic egressive consonant\n/d/ voiced alveolar plosive pulmonic egressive consonant\n/d͜ʒ/ voiced post-alveolar affricate pulmonic egressive consonant\n"
    it "should be that (t͜ʃdd͜ʒʒ) is properly split into 4 phonemes" $
        ipaTextToPhonetListReport "t͜ʃdd͜ʒʒ" `shouldBe` "/t͜ʃ/ voiceless post-alveolar affricate pulmonic egressive consonant\n/d/ voiced alveolar plosive pulmonic egressive consonant\n/d͜ʒ/ voiced post-alveolar affricate pulmonic egressive consonant\n/ʒ/ voiced post-alveolar fricative pulmonic egressive consonant\n"

xVoicedIsY :: Text -> Text -> Spec
xVoicedIsY unvoicedPhoneme voicedPhoneme =
  describe ("voicing [" ++ toString unvoicedPhoneme ++ "]") $ do
    it ("should be that: [" ++ toString unvoicedPhoneme ++ "] voiced is [" ++ toString voicedPhoneme ++ "]") $
      voicedIPA unvoicedPhoneme `shouldBe` voicedPhoneme

xDevoicedIsY :: Text -> Text -> Spec
xDevoicedIsY unvoicedPhoneme voicedPhoneme =
  describe ("devoicing [" ++ toString voicedPhoneme ++ "]") $ do
    it ("should be that: [" ++ toString voicedPhoneme ++ "] devoiced is [" ++ toString unvoicedPhoneme ++ "]") $
      devoicedIPA voicedPhoneme `shouldBe` unvoicedPhoneme

isVoicelessCounterpartOf :: Text -> Text -> Spec
isVoicelessCounterpartOf unvoicedPhoneme voicedPhoneme =
  do
  xVoicedIsY unvoicedPhoneme voicedPhoneme
  xDevoicedIsY unvoicedPhoneme voicedPhoneme

voicingSpec :: Spec
voicingSpec = do
  describe "voicing and devoicing a phoneme (no diacritics)" $ do
    "t" `isVoicelessCounterpartOf`"d"
    "p" `isVoicelessCounterpartOf`"b"
    "ʈ" `isVoicelessCounterpartOf`"ɖ"
    "c" `isVoicelessCounterpartOf`"ɟ"
    "ʈ" `isVoicelessCounterpartOf`"ɖ"
    "k" `isVoicelessCounterpartOf`"g"
    "q" `isVoicelessCounterpartOf`"ɢ"
    "ɸ" `isVoicelessCounterpartOf`"β"
    "f" `isVoicelessCounterpartOf`"v"
    "θ" `isVoicelessCounterpartOf`"ð"
    "s" `isVoicelessCounterpartOf`"z"
    "ʃ" `isVoicelessCounterpartOf`"ʒ"

    "ʂ" `isVoicelessCounterpartOf`"ʐ"
    "ç" `isVoicelessCounterpartOf`"ʝ"
    "ɕ" `isVoicelessCounterpartOf`"ʑ"
    "x" `isVoicelessCounterpartOf`"ɣ"
    "x" `isVoicelessCounterpartOf`"ɣ"
    "χ" `isVoicelessCounterpartOf`"ʁ"
    "ħ" `isVoicelessCounterpartOf`"ʕ"
    "h" `isVoicelessCounterpartOf`"ɦ"
    "ɬ" `isVoicelessCounterpartOf`"ɮ"

  describe "voicing and devoicing a phoneme (with voiceless diacritic)" $ do
   {-
   Test that phonemes that in IPA require the diacritic symbol
   to express voicelessness are handled correctly
   -}
    -- Nasal consonants:
    "m̥" `isVoicelessCounterpartOf`"m"
    "ɱ̊" `isVoicelessCounterpartOf`"ɱ"
    "n̥" `isVoicelessCounterpartOf`"n"
    "ɲ̊" `isVoicelessCounterpartOf`"ɲ"
    "ɳ̊" `isVoicelessCounterpartOf`"ɳ"
    "ŋ̊" `isVoicelessCounterpartOf`"ŋ"
    "ɴ̥" `isVoicelessCounterpartOf`"ɴ"

    -- Trill consonants:
    "ʙ̥" `isVoicelessCounterpartOf`"ʙ"
    "r̥" `isVoicelessCounterpartOf`"r"
    "ʀ̥" `isVoicelessCounterpartOf`"ʀ"

    -- Tap or flap consonants:
    "ⱱ̥" `isVoicelessCounterpartOf`"ⱱ"
    "ɾ̥" `isVoicelessCounterpartOf`"ɾ"
    "ɽ̊" `isVoicelessCounterpartOf`"ɽ"

    -- Approximant consonants:
    "ʋ̥" `isVoicelessCounterpartOf`"ʋ"
    "ɹ̥" `isVoicelessCounterpartOf`"ɹ"
    "ɻ̊" `isVoicelessCounterpartOf`"ɻ"
    "j̊" `isVoicelessCounterpartOf`"j"
    "ɰ̊" `isVoicelessCounterpartOf`"ɰ"

    -- Lateral approximants:
    "l̥" `isVoicelessCounterpartOf`"l"
    "ɭ̥" `isVoicelessCounterpartOf`"ɭ"
    "ʎ̥" `isVoicelessCounterpartOf`"ʎ"
    "ʟ̥" `isVoicelessCounterpartOf`"ʟ"

    -- Vowels
    "i̥" `isVoicelessCounterpartOf`"i"
    "ẙ" `isVoicelessCounterpartOf`"y"
    "ɨ̥" `isVoicelessCounterpartOf`"ɨ"
    "ʉ̥" `isVoicelessCounterpartOf`"ʉ"
    "ɯ̥" `isVoicelessCounterpartOf`"ɯ"
    "u̥" `isVoicelessCounterpartOf`"u"
    "ɪ̥" `isVoicelessCounterpartOf`"ɪ"
    "ʏ̥" `isVoicelessCounterpartOf`"ʏ"
    "ʊ̥" `isVoicelessCounterpartOf`"ʊ"
    "e̥" `isVoicelessCounterpartOf`"e"
    "ø̥" `isVoicelessCounterpartOf`"ø"
    "ɘ̥" `isVoicelessCounterpartOf`"ɘ"
    "ɵ̥" `isVoicelessCounterpartOf`"ɵ"
    "ɤ̥" `isVoicelessCounterpartOf`"ɤ"
    "o̥" `isVoicelessCounterpartOf`"o"
    "ə̥" `isVoicelessCounterpartOf`"ə"
    "ɛ̥" `isVoicelessCounterpartOf`"ɛ"
    "œ̥" `isVoicelessCounterpartOf`"œ"
    "ɜ̥" `isVoicelessCounterpartOf`"ɜ"
    "ɞ̥" `isVoicelessCounterpartOf`"ɞ"
    "ʌ̥" `isVoicelessCounterpartOf`"ʌ"
    "ɔ̥" `isVoicelessCounterpartOf`"ɔ"
    "æ̥" `isVoicelessCounterpartOf`"æ"
    "ɐ̥" `isVoicelessCounterpartOf`"ɐ"
    "ḁ" `isVoicelessCounterpartOf`"a"
    "ɶ̥" `isVoicelessCounterpartOf`"ɶ"
    "ɑ̥" `isVoicelessCounterpartOf`"ɑ"
    "ɒ̥" `isVoicelessCounterpartOf`"ɒ"
    "w̥" `isVoicelessCounterpartOf`"w"
    "ɥ̊" `isVoicelessCounterpartOf`"ɥ"
    "ɕ" `isVoicelessCounterpartOf`"ʑ"
    "ɺ̥" `isVoicelessCounterpartOf`"ɺ"

  describe "voicing a phoneme (with voiceless diacritic above or below)" $ do
    -- Nasal consonants:
    "m̥" `xVoicedIsY` "m"
    "m̊" `xVoicedIsY` "m"

    "ɱ̥" `xVoicedIsY` "ɱ"
    "ɱ̊" `xVoicedIsY` "ɱ"

    "n̥" `xVoicedIsY` "n"
    "n̊" `xVoicedIsY` "n"

    "ɲ̥" `xVoicedIsY` "ɲ"
    "ɲ̊" `xVoicedIsY` "ɲ"

    "ɳ̥" `xVoicedIsY` "ɳ"
    "ɳ̊" `xVoicedIsY` "ɳ"

    "ŋ̥" `xVoicedIsY` "ŋ"
    "ŋ̊" `xVoicedIsY` "ŋ"

    "ɴ̥" `xVoicedIsY` "ɴ"
    "ɴ̊" `xVoicedIsY` "ɴ"

    -- Trill consonants:
    "ʙ̊" `xVoicedIsY` "ʙ"
    "ʙ̥" `xVoicedIsY` "ʙ"
    "r̊" `xVoicedIsY` "r"
    "r̥" `xVoicedIsY` "r"
    "ʀ̊" `xVoicedIsY` "ʀ"
    "ʀ̥" `xVoicedIsY` "ʀ"

    -- Tap or flap consonants:
    "ⱱ̥" `xVoicedIsY` "ⱱ"
    "ⱱ̊" `xVoicedIsY` "ⱱ"
    "ɾ̥" `xVoicedIsY` "ɾ"
    "ɾ̊" `xVoicedIsY` "ɾ"
    "ɽ̥" `xVoicedIsY` "ɽ"
    "ɽ̊" `xVoicedIsY` "ɽ"

    -- Approximant consonants:
    "ʋ̥" `xVoicedIsY` "ʋ"
    "ʋ̊" `xVoicedIsY` "ʋ"
    "ɹ̥" `xVoicedIsY` "ɹ"
    "ɹ̊" `xVoicedIsY` "ɹ"
    "ɻ̥" `xVoicedIsY` "ɻ"
    "ɻ̊" `xVoicedIsY` "ɻ"
    "j̥" `xVoicedIsY` "j"
    "j̊" `xVoicedIsY` "j"
    "ɰ̥" `xVoicedIsY` "ɰ"
    "ɰ̊" `xVoicedIsY` "ɰ"

    -- Lateral approximants:
    "l̥" `xVoicedIsY` "l"
    "l̊" `xVoicedIsY` "l"

    "ɭ̥" `xVoicedIsY` "ɭ"
    "ɭ̊" `xVoicedIsY` "ɭ"


    "ʎ̥" `xVoicedIsY` "ʎ"
    "ʎ̊" `xVoicedIsY` "ʎ"

    "ʟ̥" `xVoicedIsY` "ʟ"
    "ʟ̊" `xVoicedIsY` "ʟ"

    -- Vowels
    "i̥" `xVoicedIsY` "i"
    "i̊" `xVoicedIsY` "i"

    "y̥" `xVoicedIsY` "y"
    "ẙ" `xVoicedIsY` "y"

    "ɨ̥" `xVoicedIsY` "ɨ"
    "ɨ̊" `xVoicedIsY` "ɨ"

    "ʉ̥" `xVoicedIsY` "ʉ"
    "ʉ̊" `xVoicedIsY` "ʉ"

    "ɯ̥" `xVoicedIsY` "ɯ"
    "ɯ̊" `xVoicedIsY` "ɯ"

    "u̥" `xVoicedIsY` "u"
    "ů" `xVoicedIsY` "u"

    "ɪ̥" `xVoicedIsY` "ɪ"
    "ɪ̊" `xVoicedIsY` "ɪ"

    "ʏ̥" `xVoicedIsY` "ʏ"
    "ʏ̊" `xVoicedIsY` "ʏ"

    "ʊ̥" `xVoicedIsY` "ʊ"
    "ʊ̊" `xVoicedIsY` "ʊ"

    "e̥" `xVoicedIsY` "e"
    "e̊" `xVoicedIsY` "e"

    "ø̥" `xVoicedIsY` "ø"
    "ø̊" `xVoicedIsY` "ø"

    "ɘ̥" `xVoicedIsY` "ɘ"
    "ɘ̊" `xVoicedIsY` "ɘ"

    "ɵ̥" `xVoicedIsY` "ɵ"
    "ɵ̊" `xVoicedIsY` "ɵ"

    "ɤ̥" `xVoicedIsY` "ɤ"
    "ɤ̊" `xVoicedIsY` "ɤ"

    "o̥" `xVoicedIsY` "o"
    "o̊" `xVoicedIsY` "o"

    "ə̥" `xVoicedIsY` "ə"
    "ə̊" `xVoicedIsY` "ə"

    "ɛ̥" `xVoicedIsY` "ɛ"
    "ɛ̊" `xVoicedIsY` "ɛ"

    "œ̥" `xVoicedIsY` "œ"
    "œ̊" `xVoicedIsY` "œ"

    "ɜ̥" `xVoicedIsY` "ɜ"
    "ɜ̊" `xVoicedIsY` "ɜ"

    "ɞ̥" `xVoicedIsY` "ɞ"
    "ɞ̊" `xVoicedIsY` "ɞ"

    "ʌ̥" `xVoicedIsY` "ʌ"
    "ʌ̊" `xVoicedIsY` "ʌ"

    "ɔ̥" `xVoicedIsY` "ɔ"
    "ɔ̊" `xVoicedIsY` "ɔ"

    "æ̥" `xVoicedIsY` "æ"
    "æ̊" `xVoicedIsY` "æ"

    "ɐ̥" `xVoicedIsY` "ɐ"
    "ɐ̊" `xVoicedIsY` "ɐ"

    "ḁ" `xVoicedIsY` "a"
    "å" `xVoicedIsY` "a"

    "ɶ̥" `xVoicedIsY` "ɶ"
    "ɶ̊" `xVoicedIsY` "ɶ"

    "ɑ̥" `xVoicedIsY` "ɑ"
    "ɑ̊" `xVoicedIsY` "ɑ"

    "ɒ̥" `xVoicedIsY` "ɒ"
    "ɒ̊" `xVoicedIsY` "ɒ"

    "w̥" `xVoicedIsY` "w"
    "ẘ" `xVoicedIsY` "w"

    "ɥ̥" `xVoicedIsY` "ɥ"
    "ɥ̊" `xVoicedIsY` "ɥ"

    "ɕ" `xVoicedIsY` "ʑ"
    "ɕ̥" `xVoicedIsY` "ʑ"
    "ɕ̊" `xVoicedIsY` "ʑ"

    "ɺ̥" `xVoicedIsY` "ɺ"
    "ɺ̊" `xVoicedIsY` "ɺ"


  describe "voicing and devoicing a phoneme (with voiced diacritic)" $ do
    "ʔ" `isVoicelessCounterpartOf`"ʔ̬"
    "ʡ" `isVoicelessCounterpartOf`"ʡ̬"
    "ʍ" `isVoicelessCounterpartOf`"ʍ̬"


  describe "voicing and devoicing a phoneme (when no change (idempotency))" $ do
    it "should be that: [q] devoiced is the same as itself" $
      devoicedIPA "q" `shouldBe` "q"
    it "should be that: [ɢ] voiced is the same as itself" $
      voicedIPA "ɢ" `shouldBe` "ɢ"
  describe "voicing something twice is the same as voicing it once" $ do
    it "case: [k]" $
      voicedIPA (voicedIPA "k") `shouldBe` voicedIPA "k"
    it "case: [g]" $
      voicedIPA (voicedIPA "g") `shouldBe` voicedIPA "g"
  describe "devoicing something twice is the same as devoicing it once" $ do
    it "case: [k]" $
      devoicedIPA (devoicedIPA "k") `shouldBe` devoicedIPA "k"

analyzeIPAToSPESpec :: Spec
analyzeIPAToSPESpec =
  describe "calculating sound patterns of English features" $ do
    it "case: [t]" $
      analyzeIPAToSPE "t" `shouldBe` "[+consonantal; -syllabic; -continuant; -sonorant; +anterior; -distributed; coronal; -round; -voice]"
    it "case: [d]" $
      analyzeIPAToSPE "d" `shouldBe` "[+consonantal; -syllabic; -continuant; -sonorant; +anterior; -distributed; coronal; -round; +voice]"


secondaryArticulationSpec :: Spec
secondaryArticulationSpec = do
  describe "labialization" $ do
    it "case: t labialized" $
      describeIPA "tʷ" `shouldBe` "voiceless labialized alveolar plosive pulmonic egressive consonant"
    it "case: r labialized" $
      describeIPA "rʷ" `shouldBe` "voiced labialized alveolar trill pulmonic egressive consonant"
  describe "palatalization" $ do
    it "case: t palatalized" $
      describeIPA "tʲ" `shouldBe` "voiceless palatalized alveolar plosive pulmonic egressive consonant"
    it "case: r palatalized" $
      describeIPA "rʲ" `shouldBe` "voiced palatalized alveolar trill pulmonic egressive consonant"
  describe "velarization" $ do
    it "case: t velarized" $
      describeIPA "tˠ" `shouldBe` "voiceless velarized alveolar plosive pulmonic egressive consonant"
    it "case: r velarized" $
      describeIPA "rˠ" `shouldBe` "voiced velarized alveolar trill pulmonic egressive consonant"
  describe "palatalization" $ do
    it "case: t pharyngealized" $
      describeIPA "tˤ" `shouldBe` "voiceless pharyngealized alveolar plosive pulmonic egressive consonant"
    it "case: r pharyngealized" $
      describeIPA "rˤ" `shouldBe` "voiced pharyngealized alveolar trill pulmonic egressive consonant"

vowelLengthSpec :: Spec
vowelLengthSpec = do
  describe "vowel length" $ do
    it "test_normal_a_vowel case: [a]" $
      describeIPA "a" `shouldBe` "voiced unrounded open front vowel"
    it "test_long_a_vowel case: [aː]" $
      describeIPA "aː" `shouldBe` "voiced unrounded open front long vowel"
    it "test_half_long_a_vowel case: [aˑ]" $
      describeIPA "aˑ" `shouldBe` "voiced unrounded open front half-long vowel"
    it "test_extra_short_a_vowel case: [ă]" $
      describeIPA "ă" `shouldBe` "voiced unrounded open front extra-short vowel"
    it "test_voiceless_long_i_vowel case [i̥ː]" $
       describeIPA "i̥ː" `shouldBe` "voiceless unrounded close front long vowel"
    it "test_voiceless_half_long_i_vowel case [i̥ˑ]" $
       describeIPA "i̥ˑ" `shouldBe` "voiceless unrounded close front half-long vowel"
    it "test_voiceless_half_long_i_vowel case [ĭ̥]" $
      describeIPA "ĭ̥" `shouldBe` "voiceless unrounded close front extra-short vowel"
