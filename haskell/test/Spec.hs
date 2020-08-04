{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Spec(main) where

import           Data.Maybe    (fromJust)
import           Test.Hspec    (Spec, describe, hspec, it, shouldBe)
import           IPA (analyzeIPA, ipaTextToPhonetListReport, voicedIPA, devoicedIPA, analyzeIPAToSPE, describeIPA)
import           PhoneticFeatures (isGlide)
import           Relude

import qualified Data.Text as T

-- import SpecGenerator (generatedTestCode) -- If we need to generate the test code


main = do
--  putTextLn generatedTestCode -- If we need to generate the test code
  hspec glideSpec
  hspec ipaTextToPhonetListReportSpec
  hspec voicingSpec
  hspec analyzeIPAToSPESpec
  hspec secondaryArticulationSpec
  hspec vowelLengthSpec
  hspec pulmonicEgressiveConsonantSpec

isDescribedAs
  :: Text -- ^ IPA e.g. "pʰ"
  -> Text -- ^ expected english description e.g. "voiceless aspirated bilabial plosive pulmonic egressive consonant"
  -> Spec
(isDescribedAs) ipaText description =
    it ("should be that: [" <> toString ipaText <> "] is the representation of the " <> toString description) $
       describeIPA ipaText `shouldBe` description

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

pulmonicEgressiveConsonantSpec :: Spec
pulmonicEgressiveConsonantSpec = do
  describe "voiceless bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [p] \
       \is the representation of the \
       \voiceless bilabial plosive pulmonic egressive consonant" $
      describeIPA "p"
        `shouldBe`
        "voiceless bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̊] \
       \is the representation of the \
       \voiceless bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̊"
        `shouldBe`
        "voiceless bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̥] \
       \is the representation of the \
       \voiceless bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̥"
        `shouldBe`
        "voiceless bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊] \
       \is the representation of the \
       \voiceless bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊"
        `shouldBe`
        "voiceless bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥] \
       \is the representation of the \
       \voiceless bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥"
        `shouldBe`
        "voiceless bilabial plosive pulmonic egressive consonant"
  describe "voiceless labialized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pʷ] \
       \is the representation of the \
       \voiceless labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pʷ"
        `shouldBe`
        "voiceless labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̊ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̊ʷ"
        `shouldBe`
        "voiceless labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̥ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̥ʷ"
        `shouldBe`
        "voiceless labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ʷ"
        `shouldBe`
        "voiceless labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ʷ"
        `shouldBe`
        "voiceless labialized bilabial plosive pulmonic egressive consonant"
  describe "voiceless palatalized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pʲ"
        `shouldBe`
        "voiceless palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̊ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̊ʲ"
        `shouldBe`
        "voiceless palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̥ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̥ʲ"
        `shouldBe`
        "voiceless palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ʲ"
        `shouldBe`
        "voiceless palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ʲ"
        `shouldBe`
        "voiceless palatalized bilabial plosive pulmonic egressive consonant"
  describe "voiceless velarized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pˠ] \
       \is the representation of the \
       \voiceless velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pˠ"
        `shouldBe`
        "voiceless velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̊ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̊ˠ"
        `shouldBe`
        "voiceless velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̥ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̥ˠ"
        `shouldBe`
        "voiceless velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ˠ"
        `shouldBe`
        "voiceless velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ˠ"
        `shouldBe`
        "voiceless velarized bilabial plosive pulmonic egressive consonant"
  describe "voiceless pharyngealized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̊ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̥ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial plosive pulmonic egressive consonant"
  describe "voiceless aspirated bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pʰ] \
       \is the representation of the \
       \voiceless aspirated bilabial plosive pulmonic egressive consonant" $
      describeIPA "pʰ"
        `shouldBe`
        "voiceless aspirated bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ʰ] \
       \is the representation of the \
       \voiceless aspirated bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ʰ"
        `shouldBe`
        "voiceless aspirated bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ʰ] \
       \is the representation of the \
       \voiceless aspirated bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ʰ"
        `shouldBe`
        "voiceless aspirated bilabial plosive pulmonic egressive consonant"
  describe "voiceless aspirated labialized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pʰʷ"
        `shouldBe`
        "voiceless aspirated labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized bilabial plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatalized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized bilabial plosive pulmonic egressive consonant"
  describe "voiceless aspirated velarized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pʰˠ"
        `shouldBe`
        "voiceless aspirated velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized bilabial plosive pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized bilabial plosive pulmonic egressive consonant"
  describe "voiced bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [b] \
       \is the representation of the \
       \voiced bilabial plosive pulmonic egressive consonant" $
      describeIPA "b"
        `shouldBe`
        "voiced bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬] \
       \is the representation of the \
       \voiced bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬"
        `shouldBe`
        "voiced bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬] \
       \is the representation of the \
       \voiced bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬"
        `shouldBe`
        "voiced bilabial plosive pulmonic egressive consonant"
  describe "voiced labialized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bʷ] \
       \is the representation of the \
       \voiced labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bʷ"
        `shouldBe`
        "voiced labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʷ] \
       \is the representation of the \
       \voiced labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʷ"
        `shouldBe`
        "voiced labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʷ] \
       \is the representation of the \
       \voiced labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʷ"
        `shouldBe`
        "voiced labialized bilabial plosive pulmonic egressive consonant"
  describe "voiced palatalized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bʲ] \
       \is the representation of the \
       \voiced palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bʲ"
        `shouldBe`
        "voiced palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʲ] \
       \is the representation of the \
       \voiced palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʲ"
        `shouldBe`
        "voiced palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʲ] \
       \is the representation of the \
       \voiced palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʲ"
        `shouldBe`
        "voiced palatalized bilabial plosive pulmonic egressive consonant"
  describe "voiced velarized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bˠ] \
       \is the representation of the \
       \voiced velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bˠ"
        `shouldBe`
        "voiced velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ˠ] \
       \is the representation of the \
       \voiced velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ˠ"
        `shouldBe`
        "voiced velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ˠ] \
       \is the representation of the \
       \voiced velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ˠ"
        `shouldBe`
        "voiced velarized bilabial plosive pulmonic egressive consonant"
  describe "voiced pharyngealized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bˤ] \
       \is the representation of the \
       \voiced pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bˤ"
        `shouldBe`
        "voiced pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ˤ"
        `shouldBe`
        "voiced pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ˤ"
        `shouldBe`
        "voiced pharyngealized bilabial plosive pulmonic egressive consonant"
  describe "voiced aspirated bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bʰ] \
       \is the representation of the \
       \voiced aspirated bilabial plosive pulmonic egressive consonant" $
      describeIPA "bʰ"
        `shouldBe`
        "voiced aspirated bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̬ʰ] \
       \is the representation of the \
       \voiced aspirated bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̬ʰ"
        `shouldBe`
        "voiced aspirated bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʰ] \
       \is the representation of the \
       \voiced aspirated bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʰ"
        `shouldBe`
        "voiced aspirated bilabial plosive pulmonic egressive consonant"
  describe "voiced aspirated labialized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bʰʷ"
        `shouldBe`
        "voiced aspirated labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized bilabial plosive pulmonic egressive consonant"
  describe "voiced aspirated palatalized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bʰʲ"
        `shouldBe`
        "voiced aspirated palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized bilabial plosive pulmonic egressive consonant"
  describe "voiced aspirated velarized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bʰˠ"
        `shouldBe`
        "voiced aspirated velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized bilabial plosive pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized bilabial plosive pulmonic egressive consonant"
  describe "voiceless alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [t] \
       \is the representation of the \
       \voiceless alveolar plosive pulmonic egressive consonant" $
      describeIPA "t"
        `shouldBe`
        "voiceless alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̊] \
       \is the representation of the \
       \voiceless alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̊"
        `shouldBe`
        "voiceless alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̥] \
       \is the representation of the \
       \voiceless alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̥"
        `shouldBe`
        "voiceless alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊] \
       \is the representation of the \
       \voiceless alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊"
        `shouldBe`
        "voiceless alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥] \
       \is the representation of the \
       \voiceless alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥"
        `shouldBe`
        "voiceless alveolar plosive pulmonic egressive consonant"
  describe "voiceless labialized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tʷ] \
       \is the representation of the \
       \voiceless labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tʷ"
        `shouldBe`
        "voiceless labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̊ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̊ʷ"
        `shouldBe`
        "voiceless labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̥ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̥ʷ"
        `shouldBe`
        "voiceless labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ʷ"
        `shouldBe`
        "voiceless labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ʷ"
        `shouldBe`
        "voiceless labialized alveolar plosive pulmonic egressive consonant"
  describe "voiceless palatalized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tʲ"
        `shouldBe`
        "voiceless palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̊ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̊ʲ"
        `shouldBe`
        "voiceless palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̥ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̥ʲ"
        `shouldBe`
        "voiceless palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ʲ"
        `shouldBe`
        "voiceless palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ʲ"
        `shouldBe`
        "voiceless palatalized alveolar plosive pulmonic egressive consonant"
  describe "voiceless velarized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tˠ] \
       \is the representation of the \
       \voiceless velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tˠ"
        `shouldBe`
        "voiceless velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̊ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̊ˠ"
        `shouldBe`
        "voiceless velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̥ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̥ˠ"
        `shouldBe`
        "voiceless velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ˠ"
        `shouldBe`
        "voiceless velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ˠ"
        `shouldBe`
        "voiceless velarized alveolar plosive pulmonic egressive consonant"
  describe "voiceless pharyngealized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̊ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̥ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar plosive pulmonic egressive consonant"
  describe "voiceless aspirated alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar plosive pulmonic egressive consonant" $
      describeIPA "tʰ"
        `shouldBe`
        "voiceless aspirated alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ʰ"
        `shouldBe`
        "voiceless aspirated alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ʰ"
        `shouldBe`
        "voiceless aspirated alveolar plosive pulmonic egressive consonant"
  describe "voiceless aspirated labialized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatalized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar plosive pulmonic egressive consonant"
  describe "voiceless aspirated velarized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar plosive pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar plosive pulmonic egressive consonant"
  describe "voiced alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [d] \
       \is the representation of the \
       \voiced alveolar plosive pulmonic egressive consonant" $
      describeIPA "d"
        `shouldBe`
        "voiced alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬] \
       \is the representation of the \
       \voiced alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬"
        `shouldBe`
        "voiced alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬] \
       \is the representation of the \
       \voiced alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬"
        `shouldBe`
        "voiced alveolar plosive pulmonic egressive consonant"
  describe "voiced labialized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dʷ] \
       \is the representation of the \
       \voiced labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dʷ"
        `shouldBe`
        "voiced labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʷ] \
       \is the representation of the \
       \voiced labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʷ"
        `shouldBe`
        "voiced labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʷ] \
       \is the representation of the \
       \voiced labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʷ"
        `shouldBe`
        "voiced labialized alveolar plosive pulmonic egressive consonant"
  describe "voiced palatalized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dʲ] \
       \is the representation of the \
       \voiced palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dʲ"
        `shouldBe`
        "voiced palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʲ] \
       \is the representation of the \
       \voiced palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʲ"
        `shouldBe`
        "voiced palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʲ] \
       \is the representation of the \
       \voiced palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʲ"
        `shouldBe`
        "voiced palatalized alveolar plosive pulmonic egressive consonant"
  describe "voiced velarized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dˠ] \
       \is the representation of the \
       \voiced velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dˠ"
        `shouldBe`
        "voiced velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ˠ] \
       \is the representation of the \
       \voiced velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ˠ"
        `shouldBe`
        "voiced velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ˠ] \
       \is the representation of the \
       \voiced velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ˠ"
        `shouldBe`
        "voiced velarized alveolar plosive pulmonic egressive consonant"
  describe "voiced pharyngealized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dˤ"
        `shouldBe`
        "voiced pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ˤ"
        `shouldBe`
        "voiced pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ˤ"
        `shouldBe`
        "voiced pharyngealized alveolar plosive pulmonic egressive consonant"
  describe "voiced aspirated alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dʰ] \
       \is the representation of the \
       \voiced aspirated alveolar plosive pulmonic egressive consonant" $
      describeIPA "dʰ"
        `shouldBe`
        "voiced aspirated alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̬ʰ] \
       \is the representation of the \
       \voiced aspirated alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̬ʰ"
        `shouldBe`
        "voiced aspirated alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʰ] \
       \is the representation of the \
       \voiced aspirated alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʰ"
        `shouldBe`
        "voiced aspirated alveolar plosive pulmonic egressive consonant"
  describe "voiced aspirated labialized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar plosive pulmonic egressive consonant"
  describe "voiced aspirated palatalized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar plosive pulmonic egressive consonant"
  describe "voiced aspirated velarized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar plosive pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar plosive pulmonic egressive consonant"
  describe "voiceless retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈ] \
       \is the representation of the \
       \voiceless retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ"
        `shouldBe`
        "voiceless retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̊] \
       \is the representation of the \
       \voiceless retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̊"
        `shouldBe`
        "voiceless retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̥] \
       \is the representation of the \
       \voiceless retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̥"
        `shouldBe`
        "voiceless retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊] \
       \is the representation of the \
       \voiceless retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊"
        `shouldBe`
        "voiceless retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥] \
       \is the representation of the \
       \voiceless retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥"
        `shouldBe`
        "voiceless retroflex plosive pulmonic egressive consonant"
  describe "voiceless labialized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈʷ] \
       \is the representation of the \
       \voiceless labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈʷ"
        `shouldBe`
        "voiceless labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̊ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̊ʷ"
        `shouldBe`
        "voiceless labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̥ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̥ʷ"
        `shouldBe`
        "voiceless labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ʷ"
        `shouldBe`
        "voiceless labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ʷ"
        `shouldBe`
        "voiceless labialized retroflex plosive pulmonic egressive consonant"
  describe "voiceless palatalized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈʲ"
        `shouldBe`
        "voiceless palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̊ʲ"
        `shouldBe`
        "voiceless palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̥ʲ"
        `shouldBe`
        "voiceless palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ʲ"
        `shouldBe`
        "voiceless palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ʲ"
        `shouldBe`
        "voiceless palatalized retroflex plosive pulmonic egressive consonant"
  describe "voiceless velarized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈˠ] \
       \is the representation of the \
       \voiceless velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈˠ"
        `shouldBe`
        "voiceless velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̊ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̊ˠ"
        `shouldBe`
        "voiceless velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̥ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̥ˠ"
        `shouldBe`
        "voiceless velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ˠ"
        `shouldBe`
        "voiceless velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ˠ"
        `shouldBe`
        "voiceless velarized retroflex plosive pulmonic egressive consonant"
  describe "voiceless pharyngealized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex plosive pulmonic egressive consonant"
  describe "voiceless aspirated retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈʰ"
        `shouldBe`
        "voiceless aspirated retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ʰ"
        `shouldBe`
        "voiceless aspirated retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ʰ"
        `shouldBe`
        "voiceless aspirated retroflex plosive pulmonic egressive consonant"
  describe "voiceless aspirated labialized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatalized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex plosive pulmonic egressive consonant"
  describe "voiceless aspirated velarized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex plosive pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex plosive pulmonic egressive consonant"
  describe "voiced retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖ] \
       \is the representation of the \
       \voiced retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ"
        `shouldBe`
        "voiced retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬] \
       \is the representation of the \
       \voiced retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬"
        `shouldBe`
        "voiced retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬] \
       \is the representation of the \
       \voiced retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬"
        `shouldBe`
        "voiced retroflex plosive pulmonic egressive consonant"
  describe "voiced labialized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖʷ] \
       \is the representation of the \
       \voiced labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖʷ"
        `shouldBe`
        "voiced labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʷ] \
       \is the representation of the \
       \voiced labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʷ"
        `shouldBe`
        "voiced labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʷ] \
       \is the representation of the \
       \voiced labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʷ"
        `shouldBe`
        "voiced labialized retroflex plosive pulmonic egressive consonant"
  describe "voiced palatalized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖʲ] \
       \is the representation of the \
       \voiced palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖʲ"
        `shouldBe`
        "voiced palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʲ] \
       \is the representation of the \
       \voiced palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʲ"
        `shouldBe`
        "voiced palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʲ] \
       \is the representation of the \
       \voiced palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʲ"
        `shouldBe`
        "voiced palatalized retroflex plosive pulmonic egressive consonant"
  describe "voiced velarized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖˠ] \
       \is the representation of the \
       \voiced velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖˠ"
        `shouldBe`
        "voiced velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ˠ] \
       \is the representation of the \
       \voiced velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ˠ"
        `shouldBe`
        "voiced velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ˠ] \
       \is the representation of the \
       \voiced velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ˠ"
        `shouldBe`
        "voiced velarized retroflex plosive pulmonic egressive consonant"
  describe "voiced pharyngealized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖˤ] \
       \is the representation of the \
       \voiced pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖˤ"
        `shouldBe`
        "voiced pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ˤ"
        `shouldBe`
        "voiced pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ˤ"
        `shouldBe`
        "voiced pharyngealized retroflex plosive pulmonic egressive consonant"
  describe "voiced aspirated retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖʰ] \
       \is the representation of the \
       \voiced aspirated retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖʰ"
        `shouldBe`
        "voiced aspirated retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̬ʰ] \
       \is the representation of the \
       \voiced aspirated retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̬ʰ"
        `shouldBe`
        "voiced aspirated retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʰ] \
       \is the representation of the \
       \voiced aspirated retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʰ"
        `shouldBe`
        "voiced aspirated retroflex plosive pulmonic egressive consonant"
  describe "voiced aspirated labialized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex plosive pulmonic egressive consonant"
  describe "voiced aspirated palatalized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex plosive pulmonic egressive consonant"
  describe "voiced aspirated velarized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex plosive pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex plosive pulmonic egressive consonant"
  describe "voiceless palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [c] \
       \is the representation of the \
       \voiceless palatal plosive pulmonic egressive consonant" $
      describeIPA "c"
        `shouldBe`
        "voiceless palatal plosive pulmonic egressive consonant"
    it "should be that: [c̊] \
       \is the representation of the \
       \voiceless palatal plosive pulmonic egressive consonant" $
      describeIPA "c̊"
        `shouldBe`
        "voiceless palatal plosive pulmonic egressive consonant"
    it "should be that: [c̥] \
       \is the representation of the \
       \voiceless palatal plosive pulmonic egressive consonant" $
      describeIPA "c̥"
        `shouldBe`
        "voiceless palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊] \
       \is the representation of the \
       \voiceless palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊"
        `shouldBe`
        "voiceless palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥] \
       \is the representation of the \
       \voiceless palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥"
        `shouldBe`
        "voiceless palatal plosive pulmonic egressive consonant"
  describe "voiceless labialized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cʷ] \
       \is the representation of the \
       \voiceless labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "cʷ"
        `shouldBe`
        "voiceless labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̊ʷ] \
       \is the representation of the \
       \voiceless labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̊ʷ"
        `shouldBe`
        "voiceless labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̥ʷ] \
       \is the representation of the \
       \voiceless labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̥ʷ"
        `shouldBe`
        "voiceless labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ʷ] \
       \is the representation of the \
       \voiceless labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ʷ"
        `shouldBe`
        "voiceless labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ʷ] \
       \is the representation of the \
       \voiceless labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ʷ"
        `shouldBe`
        "voiceless labialized palatal plosive pulmonic egressive consonant"
  describe "voiceless palatalized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cʲ] \
       \is the representation of the \
       \voiceless palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "cʲ"
        `shouldBe`
        "voiceless palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̊ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̊ʲ"
        `shouldBe`
        "voiceless palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̥ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̥ʲ"
        `shouldBe`
        "voiceless palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ʲ"
        `shouldBe`
        "voiceless palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ʲ"
        `shouldBe`
        "voiceless palatalized palatal plosive pulmonic egressive consonant"
  describe "voiceless velarized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cˠ] \
       \is the representation of the \
       \voiceless velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "cˠ"
        `shouldBe`
        "voiceless velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̊ˠ] \
       \is the representation of the \
       \voiceless velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̊ˠ"
        `shouldBe`
        "voiceless velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̥ˠ] \
       \is the representation of the \
       \voiceless velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̥ˠ"
        `shouldBe`
        "voiceless velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ˠ] \
       \is the representation of the \
       \voiceless velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ˠ"
        `shouldBe`
        "voiceless velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ˠ] \
       \is the representation of the \
       \voiceless velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ˠ"
        `shouldBe`
        "voiceless velarized palatal plosive pulmonic egressive consonant"
  describe "voiceless pharyngealized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "cˤ"
        `shouldBe`
        "voiceless pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̊ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̥ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cʰ] \
       \is the representation of the \
       \voiceless aspirated palatal plosive pulmonic egressive consonant" $
      describeIPA "cʰ"
        `shouldBe`
        "voiceless aspirated palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ʰ"
        `shouldBe`
        "voiceless aspirated palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ʰ"
        `shouldBe`
        "voiceless aspirated palatal plosive pulmonic egressive consonant"
  describe "voiceless aspirated labialized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "cʰʷ"
        `shouldBe`
        "voiceless aspirated labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized palatal plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatalized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "cʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized palatal plosive pulmonic egressive consonant"
  describe "voiceless aspirated velarized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "cʰˠ"
        `shouldBe`
        "voiceless aspirated velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized palatal plosive pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "cʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized palatal plosive pulmonic egressive consonant"
  describe "voiced palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟ] \
       \is the representation of the \
       \voiced palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ"
        `shouldBe`
        "voiced palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬] \
       \is the representation of the \
       \voiced palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬"
        `shouldBe`
        "voiced palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬] \
       \is the representation of the \
       \voiced palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬"
        `shouldBe`
        "voiced palatal plosive pulmonic egressive consonant"
  describe "voiced labialized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟʷ] \
       \is the representation of the \
       \voiced labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟʷ"
        `shouldBe`
        "voiced labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʷ] \
       \is the representation of the \
       \voiced labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʷ"
        `shouldBe`
        "voiced labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʷ] \
       \is the representation of the \
       \voiced labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʷ"
        `shouldBe`
        "voiced labialized palatal plosive pulmonic egressive consonant"
  describe "voiced palatalized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟʲ] \
       \is the representation of the \
       \voiced palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟʲ"
        `shouldBe`
        "voiced palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʲ] \
       \is the representation of the \
       \voiced palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʲ"
        `shouldBe`
        "voiced palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʲ] \
       \is the representation of the \
       \voiced palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʲ"
        `shouldBe`
        "voiced palatalized palatal plosive pulmonic egressive consonant"
  describe "voiced velarized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟˠ] \
       \is the representation of the \
       \voiced velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟˠ"
        `shouldBe`
        "voiced velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ˠ] \
       \is the representation of the \
       \voiced velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ˠ"
        `shouldBe`
        "voiced velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ˠ] \
       \is the representation of the \
       \voiced velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ˠ"
        `shouldBe`
        "voiced velarized palatal plosive pulmonic egressive consonant"
  describe "voiced pharyngealized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟˤ] \
       \is the representation of the \
       \voiced pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟˤ"
        `shouldBe`
        "voiced pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ˤ"
        `shouldBe`
        "voiced pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ˤ"
        `shouldBe`
        "voiced pharyngealized palatal plosive pulmonic egressive consonant"
  describe "voiced aspirated palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟʰ] \
       \is the representation of the \
       \voiced aspirated palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟʰ"
        `shouldBe`
        "voiced aspirated palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̬ʰ] \
       \is the representation of the \
       \voiced aspirated palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̬ʰ"
        `shouldBe`
        "voiced aspirated palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʰ] \
       \is the representation of the \
       \voiced aspirated palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʰ"
        `shouldBe`
        "voiced aspirated palatal plosive pulmonic egressive consonant"
  describe "voiced aspirated labialized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟʰʷ"
        `shouldBe`
        "voiced aspirated labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized palatal plosive pulmonic egressive consonant"
  describe "voiced aspirated palatalized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟʰʲ"
        `shouldBe`
        "voiced aspirated palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized palatal plosive pulmonic egressive consonant"
  describe "voiced aspirated velarized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟʰˠ"
        `shouldBe`
        "voiced aspirated velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized palatal plosive pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized palatal plosive pulmonic egressive consonant"
  describe "voiceless velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [k] \
       \is the representation of the \
       \voiceless velar plosive pulmonic egressive consonant" $
      describeIPA "k"
        `shouldBe`
        "voiceless velar plosive pulmonic egressive consonant"
    it "should be that: [k̊] \
       \is the representation of the \
       \voiceless velar plosive pulmonic egressive consonant" $
      describeIPA "k̊"
        `shouldBe`
        "voiceless velar plosive pulmonic egressive consonant"
    it "should be that: [k̥] \
       \is the representation of the \
       \voiceless velar plosive pulmonic egressive consonant" $
      describeIPA "k̥"
        `shouldBe`
        "voiceless velar plosive pulmonic egressive consonant"
    it "should be that: [g̊] \
       \is the representation of the \
       \voiceless velar plosive pulmonic egressive consonant" $
      describeIPA "g̊"
        `shouldBe`
        "voiceless velar plosive pulmonic egressive consonant"
    it "should be that: [g̥] \
       \is the representation of the \
       \voiceless velar plosive pulmonic egressive consonant" $
      describeIPA "g̥"
        `shouldBe`
        "voiceless velar plosive pulmonic egressive consonant"
  describe "voiceless labialized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kʷ] \
       \is the representation of the \
       \voiceless labialized velar plosive pulmonic egressive consonant" $
      describeIPA "kʷ"
        `shouldBe`
        "voiceless labialized velar plosive pulmonic egressive consonant"
    it "should be that: [k̊ʷ] \
       \is the representation of the \
       \voiceless labialized velar plosive pulmonic egressive consonant" $
      describeIPA "k̊ʷ"
        `shouldBe`
        "voiceless labialized velar plosive pulmonic egressive consonant"
    it "should be that: [k̥ʷ] \
       \is the representation of the \
       \voiceless labialized velar plosive pulmonic egressive consonant" $
      describeIPA "k̥ʷ"
        `shouldBe`
        "voiceless labialized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ʷ] \
       \is the representation of the \
       \voiceless labialized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ʷ"
        `shouldBe`
        "voiceless labialized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ʷ] \
       \is the representation of the \
       \voiceless labialized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ʷ"
        `shouldBe`
        "voiceless labialized velar plosive pulmonic egressive consonant"
  describe "voiceless palatalized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kʲ] \
       \is the representation of the \
       \voiceless palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "kʲ"
        `shouldBe`
        "voiceless palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [k̊ʲ] \
       \is the representation of the \
       \voiceless palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "k̊ʲ"
        `shouldBe`
        "voiceless palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [k̥ʲ] \
       \is the representation of the \
       \voiceless palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "k̥ʲ"
        `shouldBe`
        "voiceless palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ʲ] \
       \is the representation of the \
       \voiceless palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ʲ"
        `shouldBe`
        "voiceless palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ʲ] \
       \is the representation of the \
       \voiceless palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ʲ"
        `shouldBe`
        "voiceless palatalized velar plosive pulmonic egressive consonant"
  describe "voiceless velarized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kˠ] \
       \is the representation of the \
       \voiceless velarized velar plosive pulmonic egressive consonant" $
      describeIPA "kˠ"
        `shouldBe`
        "voiceless velarized velar plosive pulmonic egressive consonant"
    it "should be that: [k̊ˠ] \
       \is the representation of the \
       \voiceless velarized velar plosive pulmonic egressive consonant" $
      describeIPA "k̊ˠ"
        `shouldBe`
        "voiceless velarized velar plosive pulmonic egressive consonant"
    it "should be that: [k̥ˠ] \
       \is the representation of the \
       \voiceless velarized velar plosive pulmonic egressive consonant" $
      describeIPA "k̥ˠ"
        `shouldBe`
        "voiceless velarized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ˠ] \
       \is the representation of the \
       \voiceless velarized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ˠ"
        `shouldBe`
        "voiceless velarized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ˠ] \
       \is the representation of the \
       \voiceless velarized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ˠ"
        `shouldBe`
        "voiceless velarized velar plosive pulmonic egressive consonant"
  describe "voiceless pharyngealized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "kˤ"
        `shouldBe`
        "voiceless pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [k̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "k̊ˤ"
        `shouldBe`
        "voiceless pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [k̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "k̥ˤ"
        `shouldBe`
        "voiceless pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ˤ"
        `shouldBe`
        "voiceless pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ˤ"
        `shouldBe`
        "voiceless pharyngealized velar plosive pulmonic egressive consonant"
  describe "voiceless aspirated velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kʰ] \
       \is the representation of the \
       \voiceless aspirated velar plosive pulmonic egressive consonant" $
      describeIPA "kʰ"
        `shouldBe`
        "voiceless aspirated velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ʰ] \
       \is the representation of the \
       \voiceless aspirated velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ʰ"
        `shouldBe`
        "voiceless aspirated velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ʰ] \
       \is the representation of the \
       \voiceless aspirated velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ʰ"
        `shouldBe`
        "voiceless aspirated velar plosive pulmonic egressive consonant"
  describe "voiceless aspirated labialized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized velar plosive pulmonic egressive consonant" $
      describeIPA "kʰʷ"
        `shouldBe`
        "voiceless aspirated labialized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized velar plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatalized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "kʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized velar plosive pulmonic egressive consonant"
  describe "voiceless aspirated velarized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized velar plosive pulmonic egressive consonant" $
      describeIPA "kʰˠ"
        `shouldBe`
        "voiceless aspirated velarized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized velar plosive pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "kʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized velar plosive pulmonic egressive consonant"
  describe "voiced velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [g] \
       \is the representation of the \
       \voiced velar plosive pulmonic egressive consonant" $
      describeIPA "g"
        `shouldBe`
        "voiced velar plosive pulmonic egressive consonant"
    it "should be that: [k̬] \
       \is the representation of the \
       \voiced velar plosive pulmonic egressive consonant" $
      describeIPA "k̬"
        `shouldBe`
        "voiced velar plosive pulmonic egressive consonant"
    it "should be that: [k̬] \
       \is the representation of the \
       \voiced velar plosive pulmonic egressive consonant" $
      describeIPA "k̬"
        `shouldBe`
        "voiced velar plosive pulmonic egressive consonant"
  describe "voiced labialized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gʷ] \
       \is the representation of the \
       \voiced labialized velar plosive pulmonic egressive consonant" $
      describeIPA "gʷ"
        `shouldBe`
        "voiced labialized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʷ] \
       \is the representation of the \
       \voiced labialized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʷ"
        `shouldBe`
        "voiced labialized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʷ] \
       \is the representation of the \
       \voiced labialized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʷ"
        `shouldBe`
        "voiced labialized velar plosive pulmonic egressive consonant"
  describe "voiced palatalized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gʲ] \
       \is the representation of the \
       \voiced palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "gʲ"
        `shouldBe`
        "voiced palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʲ] \
       \is the representation of the \
       \voiced palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʲ"
        `shouldBe`
        "voiced palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʲ] \
       \is the representation of the \
       \voiced palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʲ"
        `shouldBe`
        "voiced palatalized velar plosive pulmonic egressive consonant"
  describe "voiced velarized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gˠ] \
       \is the representation of the \
       \voiced velarized velar plosive pulmonic egressive consonant" $
      describeIPA "gˠ"
        `shouldBe`
        "voiced velarized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ˠ] \
       \is the representation of the \
       \voiced velarized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ˠ"
        `shouldBe`
        "voiced velarized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ˠ] \
       \is the representation of the \
       \voiced velarized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ˠ"
        `shouldBe`
        "voiced velarized velar plosive pulmonic egressive consonant"
  describe "voiced pharyngealized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gˤ] \
       \is the representation of the \
       \voiced pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "gˤ"
        `shouldBe`
        "voiced pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ˤ"
        `shouldBe`
        "voiced pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ˤ"
        `shouldBe`
        "voiced pharyngealized velar plosive pulmonic egressive consonant"
  describe "voiced aspirated velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gʰ] \
       \is the representation of the \
       \voiced aspirated velar plosive pulmonic egressive consonant" $
      describeIPA "gʰ"
        `shouldBe`
        "voiced aspirated velar plosive pulmonic egressive consonant"
    it "should be that: [g̬ʰ] \
       \is the representation of the \
       \voiced aspirated velar plosive pulmonic egressive consonant" $
      describeIPA "g̬ʰ"
        `shouldBe`
        "voiced aspirated velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʰ] \
       \is the representation of the \
       \voiced aspirated velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʰ"
        `shouldBe`
        "voiced aspirated velar plosive pulmonic egressive consonant"
  describe "voiced aspirated labialized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized velar plosive pulmonic egressive consonant" $
      describeIPA "gʰʷ"
        `shouldBe`
        "voiced aspirated labialized velar plosive pulmonic egressive consonant"
    it "should be that: [g̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized velar plosive pulmonic egressive consonant" $
      describeIPA "g̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized velar plosive pulmonic egressive consonant"
  describe "voiced aspirated palatalized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "gʰʲ"
        `shouldBe`
        "voiced aspirated palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [g̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "g̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized velar plosive pulmonic egressive consonant"
  describe "voiced aspirated velarized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized velar plosive pulmonic egressive consonant" $
      describeIPA "gʰˠ"
        `shouldBe`
        "voiced aspirated velarized velar plosive pulmonic egressive consonant"
    it "should be that: [g̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized velar plosive pulmonic egressive consonant" $
      describeIPA "g̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized velar plosive pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "gʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [g̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "g̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized velar plosive pulmonic egressive consonant"
  describe "voiceless uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [q] \
       \is the representation of the \
       \voiceless uvular plosive pulmonic egressive consonant" $
      describeIPA "q"
        `shouldBe`
        "voiceless uvular plosive pulmonic egressive consonant"
    it "should be that: [q̊] \
       \is the representation of the \
       \voiceless uvular plosive pulmonic egressive consonant" $
      describeIPA "q̊"
        `shouldBe`
        "voiceless uvular plosive pulmonic egressive consonant"
    it "should be that: [q̥] \
       \is the representation of the \
       \voiceless uvular plosive pulmonic egressive consonant" $
      describeIPA "q̥"
        `shouldBe`
        "voiceless uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊] \
       \is the representation of the \
       \voiceless uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊"
        `shouldBe`
        "voiceless uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥] \
       \is the representation of the \
       \voiceless uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥"
        `shouldBe`
        "voiceless uvular plosive pulmonic egressive consonant"
  describe "voiceless labialized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qʷ] \
       \is the representation of the \
       \voiceless labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "qʷ"
        `shouldBe`
        "voiceless labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̊ʷ] \
       \is the representation of the \
       \voiceless labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̊ʷ"
        `shouldBe`
        "voiceless labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̥ʷ] \
       \is the representation of the \
       \voiceless labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̥ʷ"
        `shouldBe`
        "voiceless labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ʷ] \
       \is the representation of the \
       \voiceless labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ʷ"
        `shouldBe`
        "voiceless labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ʷ] \
       \is the representation of the \
       \voiceless labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ʷ"
        `shouldBe`
        "voiceless labialized uvular plosive pulmonic egressive consonant"
  describe "voiceless palatalized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qʲ] \
       \is the representation of the \
       \voiceless palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "qʲ"
        `shouldBe`
        "voiceless palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̊ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̊ʲ"
        `shouldBe`
        "voiceless palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̥ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̥ʲ"
        `shouldBe`
        "voiceless palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ʲ"
        `shouldBe`
        "voiceless palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ʲ"
        `shouldBe`
        "voiceless palatalized uvular plosive pulmonic egressive consonant"
  describe "voiceless velarized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qˠ] \
       \is the representation of the \
       \voiceless velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "qˠ"
        `shouldBe`
        "voiceless velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̊ˠ] \
       \is the representation of the \
       \voiceless velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̊ˠ"
        `shouldBe`
        "voiceless velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̥ˠ] \
       \is the representation of the \
       \voiceless velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̥ˠ"
        `shouldBe`
        "voiceless velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ˠ] \
       \is the representation of the \
       \voiceless velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ˠ"
        `shouldBe`
        "voiceless velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ˠ] \
       \is the representation of the \
       \voiceless velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ˠ"
        `shouldBe`
        "voiceless velarized uvular plosive pulmonic egressive consonant"
  describe "voiceless pharyngealized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "qˤ"
        `shouldBe`
        "voiceless pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̊ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̥ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular plosive pulmonic egressive consonant"
  describe "voiceless aspirated uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qʰ] \
       \is the representation of the \
       \voiceless aspirated uvular plosive pulmonic egressive consonant" $
      describeIPA "qʰ"
        `shouldBe`
        "voiceless aspirated uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ʰ"
        `shouldBe`
        "voiceless aspirated uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ʰ"
        `shouldBe`
        "voiceless aspirated uvular plosive pulmonic egressive consonant"
  describe "voiceless aspirated labialized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "qʰʷ"
        `shouldBe`
        "voiceless aspirated labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized uvular plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatalized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "qʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized uvular plosive pulmonic egressive consonant"
  describe "voiceless aspirated velarized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "qʰˠ"
        `shouldBe`
        "voiceless aspirated velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized uvular plosive pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "qʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized uvular plosive pulmonic egressive consonant"
  describe "voiced uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢ] \
       \is the representation of the \
       \voiced uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ"
        `shouldBe`
        "voiced uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬] \
       \is the representation of the \
       \voiced uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬"
        `shouldBe`
        "voiced uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬] \
       \is the representation of the \
       \voiced uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬"
        `shouldBe`
        "voiced uvular plosive pulmonic egressive consonant"
  describe "voiced labialized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢʷ] \
       \is the representation of the \
       \voiced labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢʷ"
        `shouldBe`
        "voiced labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʷ] \
       \is the representation of the \
       \voiced labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʷ"
        `shouldBe`
        "voiced labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʷ] \
       \is the representation of the \
       \voiced labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʷ"
        `shouldBe`
        "voiced labialized uvular plosive pulmonic egressive consonant"
  describe "voiced palatalized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢʲ] \
       \is the representation of the \
       \voiced palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢʲ"
        `shouldBe`
        "voiced palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʲ] \
       \is the representation of the \
       \voiced palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʲ"
        `shouldBe`
        "voiced palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʲ] \
       \is the representation of the \
       \voiced palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʲ"
        `shouldBe`
        "voiced palatalized uvular plosive pulmonic egressive consonant"
  describe "voiced velarized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢˠ] \
       \is the representation of the \
       \voiced velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢˠ"
        `shouldBe`
        "voiced velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ˠ] \
       \is the representation of the \
       \voiced velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ˠ"
        `shouldBe`
        "voiced velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ˠ] \
       \is the representation of the \
       \voiced velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ˠ"
        `shouldBe`
        "voiced velarized uvular plosive pulmonic egressive consonant"
  describe "voiced pharyngealized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢˤ] \
       \is the representation of the \
       \voiced pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢˤ"
        `shouldBe`
        "voiced pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ˤ"
        `shouldBe`
        "voiced pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ˤ"
        `shouldBe`
        "voiced pharyngealized uvular plosive pulmonic egressive consonant"
  describe "voiced aspirated uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢʰ] \
       \is the representation of the \
       \voiced aspirated uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢʰ"
        `shouldBe`
        "voiced aspirated uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̬ʰ] \
       \is the representation of the \
       \voiced aspirated uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̬ʰ"
        `shouldBe`
        "voiced aspirated uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʰ] \
       \is the representation of the \
       \voiced aspirated uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʰ"
        `shouldBe`
        "voiced aspirated uvular plosive pulmonic egressive consonant"
  describe "voiced aspirated labialized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢʰʷ"
        `shouldBe`
        "voiced aspirated labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized uvular plosive pulmonic egressive consonant"
  describe "voiced aspirated palatalized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢʰʲ"
        `shouldBe`
        "voiced aspirated palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized uvular plosive pulmonic egressive consonant"
  describe "voiced aspirated velarized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢʰˠ"
        `shouldBe`
        "voiced aspirated velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized uvular plosive pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized uvular plosive pulmonic egressive consonant"
  describe "voiceless glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ] \
       \is the representation of the \
       \voiceless glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ"
        `shouldBe`
        "voiceless glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̊] \
       \is the representation of the \
       \voiceless glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̊"
        `shouldBe`
        "voiceless glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̥] \
       \is the representation of the \
       \voiceless glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̥"
        `shouldBe`
        "voiceless glottal plosive pulmonic egressive consonant"
  describe "voiceless labialized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔʷ] \
       \is the representation of the \
       \voiceless labialized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔʷ"
        `shouldBe`
        "voiceless labialized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̊ʷ] \
       \is the representation of the \
       \voiceless labialized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̊ʷ"
        `shouldBe`
        "voiceless labialized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̥ʷ] \
       \is the representation of the \
       \voiceless labialized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̥ʷ"
        `shouldBe`
        "voiceless labialized glottal plosive pulmonic egressive consonant"
  describe "voiceless palatalized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔʲ] \
       \is the representation of the \
       \voiceless palatalized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔʲ"
        `shouldBe`
        "voiceless palatalized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̊ʲ"
        `shouldBe`
        "voiceless palatalized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̥ʲ"
        `shouldBe`
        "voiceless palatalized glottal plosive pulmonic egressive consonant"
  describe "voiceless velarized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔˠ] \
       \is the representation of the \
       \voiceless velarized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔˠ"
        `shouldBe`
        "voiceless velarized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̊ˠ] \
       \is the representation of the \
       \voiceless velarized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̊ˠ"
        `shouldBe`
        "voiceless velarized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̥ˠ] \
       \is the representation of the \
       \voiceless velarized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̥ˠ"
        `shouldBe`
        "voiceless velarized glottal plosive pulmonic egressive consonant"
  describe "voiceless pharyngealized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔˤ] \
       \is the representation of the \
       \voiceless pharyngealized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔˤ"
        `shouldBe`
        "voiceless pharyngealized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized glottal plosive pulmonic egressive consonant"
  describe "voiceless aspirated glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔʰ] \
       \is the representation of the \
       \voiceless aspirated glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔʰ"
        `shouldBe`
        "voiceless aspirated glottal plosive pulmonic egressive consonant"
  describe "voiceless aspirated labialized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔʰʷ"
        `shouldBe`
        "voiceless aspirated labialized glottal plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatalized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized glottal plosive pulmonic egressive consonant"
  describe "voiceless aspirated velarized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔʰˠ"
        `shouldBe`
        "voiceless aspirated velarized glottal plosive pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized glottal plosive pulmonic egressive consonant"
  describe "voiced glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬] \
       \is the representation of the \
       \voiced glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬"
        `shouldBe`
        "voiced glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̬] \
       \is the representation of the \
       \voiced glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬"
        `shouldBe`
        "voiced glottal plosive pulmonic egressive consonant"
  describe "voiced labialized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ʷ] \
       \is the representation of the \
       \voiced labialized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʷ"
        `shouldBe`
        "voiced labialized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̬ʷ] \
       \is the representation of the \
       \voiced labialized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʷ"
        `shouldBe`
        "voiced labialized glottal plosive pulmonic egressive consonant"
  describe "voiced palatalized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ʲ] \
       \is the representation of the \
       \voiced palatalized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʲ"
        `shouldBe`
        "voiced palatalized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̬ʲ] \
       \is the representation of the \
       \voiced palatalized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʲ"
        `shouldBe`
        "voiced palatalized glottal plosive pulmonic egressive consonant"
  describe "voiced velarized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ˠ] \
       \is the representation of the \
       \voiced velarized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ˠ"
        `shouldBe`
        "voiced velarized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̬ˠ] \
       \is the representation of the \
       \voiced velarized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ˠ"
        `shouldBe`
        "voiced velarized glottal plosive pulmonic egressive consonant"
  describe "voiced pharyngealized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ˤ"
        `shouldBe`
        "voiced pharyngealized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ˤ"
        `shouldBe`
        "voiced pharyngealized glottal plosive pulmonic egressive consonant"
  describe "voiced aspirated glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ʰ] \
       \is the representation of the \
       \voiced aspirated glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʰ"
        `shouldBe`
        "voiced aspirated glottal plosive pulmonic egressive consonant"
  describe "voiced aspirated labialized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized glottal plosive pulmonic egressive consonant"
  describe "voiced aspirated palatalized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized glottal plosive pulmonic egressive consonant"
  describe "voiced aspirated velarized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized glottal plosive pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized glottal plosive pulmonic egressive consonant"
  describe "voiceless bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɸ] \
       \is the representation of the \
       \voiceless bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ"
        `shouldBe`
        "voiceless bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̊] \
       \is the representation of the \
       \voiceless bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̊"
        `shouldBe`
        "voiceless bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̥] \
       \is the representation of the \
       \voiceless bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̥"
        `shouldBe`
        "voiceless bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̊] \
       \is the representation of the \
       \voiceless bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̊"
        `shouldBe`
        "voiceless bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̥] \
       \is the representation of the \
       \voiceless bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̥"
        `shouldBe`
        "voiceless bilabial fricative pulmonic egressive consonant"
  describe "voiceless labialized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɸʷ] \
       \is the representation of the \
       \voiceless labialized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸʷ"
        `shouldBe`
        "voiceless labialized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̊ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̊ʷ"
        `shouldBe`
        "voiceless labialized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̥ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̥ʷ"
        `shouldBe`
        "voiceless labialized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̊ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̊ʷ"
        `shouldBe`
        "voiceless labialized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̥ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̥ʷ"
        `shouldBe`
        "voiceless labialized bilabial fricative pulmonic egressive consonant"
  describe "voiceless palatalized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɸʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸʲ"
        `shouldBe`
        "voiceless palatalized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̊ʲ"
        `shouldBe`
        "voiceless palatalized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̥ʲ"
        `shouldBe`
        "voiceless palatalized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̊ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̊ʲ"
        `shouldBe`
        "voiceless palatalized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̥ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̥ʲ"
        `shouldBe`
        "voiceless palatalized bilabial fricative pulmonic egressive consonant"
  describe "voiceless velarized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɸˠ] \
       \is the representation of the \
       \voiceless velarized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸˠ"
        `shouldBe`
        "voiceless velarized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̊ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̊ˠ"
        `shouldBe`
        "voiceless velarized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̥ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̥ˠ"
        `shouldBe`
        "voiceless velarized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̊ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̊ˠ"
        `shouldBe`
        "voiceless velarized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̥ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̥ˠ"
        `shouldBe`
        "voiceless velarized bilabial fricative pulmonic egressive consonant"
  describe "voiceless pharyngealized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɸˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̊ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̥ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial fricative pulmonic egressive consonant"
  describe "voiceless aspirated bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɸʰ] \
       \is the representation of the \
       \voiceless aspirated bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸʰ"
        `shouldBe`
        "voiceless aspirated bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̥ʰ] \
       \is the representation of the \
       \voiceless aspirated bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̥ʰ"
        `shouldBe`
        "voiceless aspirated bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̊ʰ] \
       \is the representation of the \
       \voiceless aspirated bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̊ʰ"
        `shouldBe`
        "voiceless aspirated bilabial fricative pulmonic egressive consonant"
  describe "voiceless aspirated labialized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɸʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸʰʷ"
        `shouldBe`
        "voiceless aspirated labialized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized bilabial fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatalized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɸʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized bilabial fricative pulmonic egressive consonant"
  describe "voiceless aspirated velarized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɸʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸʰˠ"
        `shouldBe`
        "voiceless aspirated velarized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized bilabial fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɸʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized bilabial fricative pulmonic egressive consonant"
  describe "voiced bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [β] \
       \is the representation of the \
       \voiced bilabial fricative pulmonic egressive consonant" $
      describeIPA "β"
        `shouldBe`
        "voiced bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̬] \
       \is the representation of the \
       \voiced bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̬"
        `shouldBe`
        "voiced bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̬] \
       \is the representation of the \
       \voiced bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̬"
        `shouldBe`
        "voiced bilabial fricative pulmonic egressive consonant"
  describe "voiced labialized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [βʷ] \
       \is the representation of the \
       \voiced labialized bilabial fricative pulmonic egressive consonant" $
      describeIPA "βʷ"
        `shouldBe`
        "voiced labialized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̬ʷ] \
       \is the representation of the \
       \voiced labialized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̬ʷ"
        `shouldBe`
        "voiced labialized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̬ʷ] \
       \is the representation of the \
       \voiced labialized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̬ʷ"
        `shouldBe`
        "voiced labialized bilabial fricative pulmonic egressive consonant"
  describe "voiced palatalized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [βʲ] \
       \is the representation of the \
       \voiced palatalized bilabial fricative pulmonic egressive consonant" $
      describeIPA "βʲ"
        `shouldBe`
        "voiced palatalized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̬ʲ] \
       \is the representation of the \
       \voiced palatalized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̬ʲ"
        `shouldBe`
        "voiced palatalized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̬ʲ] \
       \is the representation of the \
       \voiced palatalized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̬ʲ"
        `shouldBe`
        "voiced palatalized bilabial fricative pulmonic egressive consonant"
  describe "voiced velarized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [βˠ] \
       \is the representation of the \
       \voiced velarized bilabial fricative pulmonic egressive consonant" $
      describeIPA "βˠ"
        `shouldBe`
        "voiced velarized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̬ˠ] \
       \is the representation of the \
       \voiced velarized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̬ˠ"
        `shouldBe`
        "voiced velarized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̬ˠ] \
       \is the representation of the \
       \voiced velarized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̬ˠ"
        `shouldBe`
        "voiced velarized bilabial fricative pulmonic egressive consonant"
  describe "voiced pharyngealized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [βˤ] \
       \is the representation of the \
       \voiced pharyngealized bilabial fricative pulmonic egressive consonant" $
      describeIPA "βˤ"
        `shouldBe`
        "voiced pharyngealized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̬ˤ"
        `shouldBe`
        "voiced pharyngealized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̬ˤ"
        `shouldBe`
        "voiced pharyngealized bilabial fricative pulmonic egressive consonant"
  describe "voiced aspirated bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [βʰ] \
       \is the representation of the \
       \voiced aspirated bilabial fricative pulmonic egressive consonant" $
      describeIPA "βʰ"
        `shouldBe`
        "voiced aspirated bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̬ʰ] \
       \is the representation of the \
       \voiced aspirated bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̬ʰ"
        `shouldBe`
        "voiced aspirated bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̬ʰ] \
       \is the representation of the \
       \voiced aspirated bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̬ʰ"
        `shouldBe`
        "voiced aspirated bilabial fricative pulmonic egressive consonant"
  describe "voiced aspirated labialized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [βʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized bilabial fricative pulmonic egressive consonant" $
      describeIPA "βʰʷ"
        `shouldBe`
        "voiced aspirated labialized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized bilabial fricative pulmonic egressive consonant"
  describe "voiced aspirated palatalized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [βʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized bilabial fricative pulmonic egressive consonant" $
      describeIPA "βʰʲ"
        `shouldBe`
        "voiced aspirated palatalized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized bilabial fricative pulmonic egressive consonant"
  describe "voiced aspirated velarized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [βʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized bilabial fricative pulmonic egressive consonant" $
      describeIPA "βʰˠ"
        `shouldBe`
        "voiced aspirated velarized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized bilabial fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
    do
    it "should be that: [βʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
      describeIPA "βʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized bilabial fricative pulmonic egressive consonant"
    it "should be that: [β̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
      describeIPA "β̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized bilabial fricative pulmonic egressive consonant"
    it "should be that: [ɸ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
      describeIPA "ɸ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized bilabial fricative pulmonic egressive consonant"
  describe "voiceless labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [f] \
       \is the representation of the \
       \voiceless labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f"
        `shouldBe`
        "voiceless labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̊] \
       \is the representation of the \
       \voiceless labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̊"
        `shouldBe`
        "voiceless labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̥] \
       \is the representation of the \
       \voiceless labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̥"
        `shouldBe`
        "voiceless labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̊] \
       \is the representation of the \
       \voiceless labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̊"
        `shouldBe`
        "voiceless labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̥] \
       \is the representation of the \
       \voiceless labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̥"
        `shouldBe`
        "voiceless labio-dental fricative pulmonic egressive consonant"
  describe "voiceless labialized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [fʷ] \
       \is the representation of the \
       \voiceless labialized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "fʷ"
        `shouldBe`
        "voiceless labialized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̊ʷ] \
       \is the representation of the \
       \voiceless labialized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̊ʷ"
        `shouldBe`
        "voiceless labialized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̥ʷ] \
       \is the representation of the \
       \voiceless labialized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̥ʷ"
        `shouldBe`
        "voiceless labialized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̊ʷ] \
       \is the representation of the \
       \voiceless labialized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̊ʷ"
        `shouldBe`
        "voiceless labialized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̥ʷ] \
       \is the representation of the \
       \voiceless labialized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̥ʷ"
        `shouldBe`
        "voiceless labialized labio-dental fricative pulmonic egressive consonant"
  describe "voiceless palatalized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [fʲ] \
       \is the representation of the \
       \voiceless palatalized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "fʲ"
        `shouldBe`
        "voiceless palatalized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̊ʲ] \
       \is the representation of the \
       \voiceless palatalized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̊ʲ"
        `shouldBe`
        "voiceless palatalized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̥ʲ] \
       \is the representation of the \
       \voiceless palatalized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̥ʲ"
        `shouldBe`
        "voiceless palatalized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̊ʲ] \
       \is the representation of the \
       \voiceless palatalized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̊ʲ"
        `shouldBe`
        "voiceless palatalized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̥ʲ] \
       \is the representation of the \
       \voiceless palatalized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̥ʲ"
        `shouldBe`
        "voiceless palatalized labio-dental fricative pulmonic egressive consonant"
  describe "voiceless velarized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [fˠ] \
       \is the representation of the \
       \voiceless velarized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "fˠ"
        `shouldBe`
        "voiceless velarized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̊ˠ] \
       \is the representation of the \
       \voiceless velarized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̊ˠ"
        `shouldBe`
        "voiceless velarized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̥ˠ] \
       \is the representation of the \
       \voiceless velarized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̥ˠ"
        `shouldBe`
        "voiceless velarized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̊ˠ] \
       \is the representation of the \
       \voiceless velarized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̊ˠ"
        `shouldBe`
        "voiceless velarized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̥ˠ] \
       \is the representation of the \
       \voiceless velarized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̥ˠ"
        `shouldBe`
        "voiceless velarized labio-dental fricative pulmonic egressive consonant"
  describe "voiceless pharyngealized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [fˤ] \
       \is the representation of the \
       \voiceless pharyngealized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "fˤ"
        `shouldBe`
        "voiceless pharyngealized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̊ˤ"
        `shouldBe`
        "voiceless pharyngealized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̥ˤ"
        `shouldBe`
        "voiceless pharyngealized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̊ˤ"
        `shouldBe`
        "voiceless pharyngealized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̥ˤ"
        `shouldBe`
        "voiceless pharyngealized labio-dental fricative pulmonic egressive consonant"
  describe "voiceless aspirated labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [fʰ] \
       \is the representation of the \
       \voiceless aspirated labio-dental fricative pulmonic egressive consonant" $
      describeIPA "fʰ"
        `shouldBe`
        "voiceless aspirated labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̥ʰ] \
       \is the representation of the \
       \voiceless aspirated labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̥ʰ"
        `shouldBe`
        "voiceless aspirated labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̊ʰ] \
       \is the representation of the \
       \voiceless aspirated labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̊ʰ"
        `shouldBe`
        "voiceless aspirated labio-dental fricative pulmonic egressive consonant"
  describe "voiceless aspirated labialized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [fʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "fʰʷ"
        `shouldBe`
        "voiceless aspirated labialized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized labio-dental fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [fʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "fʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized labio-dental fricative pulmonic egressive consonant"
  describe "voiceless aspirated velarized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [fʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "fʰˠ"
        `shouldBe`
        "voiceless aspirated velarized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized labio-dental fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [fʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "fʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized labio-dental fricative pulmonic egressive consonant"
  describe "voiced labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [v] \
       \is the representation of the \
       \voiced labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v"
        `shouldBe`
        "voiced labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̬] \
       \is the representation of the \
       \voiced labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̬"
        `shouldBe`
        "voiced labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̬] \
       \is the representation of the \
       \voiced labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̬"
        `shouldBe`
        "voiced labio-dental fricative pulmonic egressive consonant"
  describe "voiced labialized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [vʷ] \
       \is the representation of the \
       \voiced labialized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "vʷ"
        `shouldBe`
        "voiced labialized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̬ʷ] \
       \is the representation of the \
       \voiced labialized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̬ʷ"
        `shouldBe`
        "voiced labialized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̬ʷ] \
       \is the representation of the \
       \voiced labialized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̬ʷ"
        `shouldBe`
        "voiced labialized labio-dental fricative pulmonic egressive consonant"
  describe "voiced palatalized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [vʲ] \
       \is the representation of the \
       \voiced palatalized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "vʲ"
        `shouldBe`
        "voiced palatalized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̬ʲ] \
       \is the representation of the \
       \voiced palatalized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̬ʲ"
        `shouldBe`
        "voiced palatalized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̬ʲ] \
       \is the representation of the \
       \voiced palatalized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̬ʲ"
        `shouldBe`
        "voiced palatalized labio-dental fricative pulmonic egressive consonant"
  describe "voiced velarized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [vˠ] \
       \is the representation of the \
       \voiced velarized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "vˠ"
        `shouldBe`
        "voiced velarized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̬ˠ] \
       \is the representation of the \
       \voiced velarized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̬ˠ"
        `shouldBe`
        "voiced velarized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̬ˠ] \
       \is the representation of the \
       \voiced velarized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̬ˠ"
        `shouldBe`
        "voiced velarized labio-dental fricative pulmonic egressive consonant"
  describe "voiced pharyngealized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [vˤ] \
       \is the representation of the \
       \voiced pharyngealized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "vˤ"
        `shouldBe`
        "voiced pharyngealized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̬ˤ"
        `shouldBe`
        "voiced pharyngealized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̬ˤ"
        `shouldBe`
        "voiced pharyngealized labio-dental fricative pulmonic egressive consonant"
  describe "voiced aspirated labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [vʰ] \
       \is the representation of the \
       \voiced aspirated labio-dental fricative pulmonic egressive consonant" $
      describeIPA "vʰ"
        `shouldBe`
        "voiced aspirated labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̬ʰ] \
       \is the representation of the \
       \voiced aspirated labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̬ʰ"
        `shouldBe`
        "voiced aspirated labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̬ʰ] \
       \is the representation of the \
       \voiced aspirated labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̬ʰ"
        `shouldBe`
        "voiced aspirated labio-dental fricative pulmonic egressive consonant"
  describe "voiced aspirated labialized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [vʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "vʰʷ"
        `shouldBe`
        "voiced aspirated labialized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized labio-dental fricative pulmonic egressive consonant"
  describe "voiced aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [vʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "vʰʲ"
        `shouldBe`
        "voiced aspirated palatalized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized labio-dental fricative pulmonic egressive consonant"
  describe "voiced aspirated velarized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [vʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "vʰˠ"
        `shouldBe`
        "voiced aspirated velarized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized labio-dental fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [vʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "vʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [v̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "v̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized labio-dental fricative pulmonic egressive consonant"
    it "should be that: [f̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
      describeIPA "f̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized labio-dental fricative pulmonic egressive consonant"
  describe "voiceless dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [θ] \
       \is the representation of the \
       \voiceless dental fricative pulmonic egressive consonant" $
      describeIPA "θ"
        `shouldBe`
        "voiceless dental fricative pulmonic egressive consonant"
    it "should be that: [θ̊] \
       \is the representation of the \
       \voiceless dental fricative pulmonic egressive consonant" $
      describeIPA "θ̊"
        `shouldBe`
        "voiceless dental fricative pulmonic egressive consonant"
    it "should be that: [θ̥] \
       \is the representation of the \
       \voiceless dental fricative pulmonic egressive consonant" $
      describeIPA "θ̥"
        `shouldBe`
        "voiceless dental fricative pulmonic egressive consonant"
    it "should be that: [ð̊] \
       \is the representation of the \
       \voiceless dental fricative pulmonic egressive consonant" $
      describeIPA "ð̊"
        `shouldBe`
        "voiceless dental fricative pulmonic egressive consonant"
    it "should be that: [ð̥] \
       \is the representation of the \
       \voiceless dental fricative pulmonic egressive consonant" $
      describeIPA "ð̥"
        `shouldBe`
        "voiceless dental fricative pulmonic egressive consonant"
  describe "voiceless labialized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [θʷ] \
       \is the representation of the \
       \voiceless labialized dental fricative pulmonic egressive consonant" $
      describeIPA "θʷ"
        `shouldBe`
        "voiceless labialized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̊ʷ] \
       \is the representation of the \
       \voiceless labialized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̊ʷ"
        `shouldBe`
        "voiceless labialized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̥ʷ] \
       \is the representation of the \
       \voiceless labialized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̥ʷ"
        `shouldBe`
        "voiceless labialized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̊ʷ] \
       \is the representation of the \
       \voiceless labialized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̊ʷ"
        `shouldBe`
        "voiceless labialized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̥ʷ] \
       \is the representation of the \
       \voiceless labialized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̥ʷ"
        `shouldBe`
        "voiceless labialized dental fricative pulmonic egressive consonant"
  describe "voiceless palatalized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [θʲ] \
       \is the representation of the \
       \voiceless palatalized dental fricative pulmonic egressive consonant" $
      describeIPA "θʲ"
        `shouldBe`
        "voiceless palatalized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̊ʲ"
        `shouldBe`
        "voiceless palatalized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̥ʲ"
        `shouldBe`
        "voiceless palatalized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̊ʲ] \
       \is the representation of the \
       \voiceless palatalized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̊ʲ"
        `shouldBe`
        "voiceless palatalized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̥ʲ] \
       \is the representation of the \
       \voiceless palatalized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̥ʲ"
        `shouldBe`
        "voiceless palatalized dental fricative pulmonic egressive consonant"
  describe "voiceless velarized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [θˠ] \
       \is the representation of the \
       \voiceless velarized dental fricative pulmonic egressive consonant" $
      describeIPA "θˠ"
        `shouldBe`
        "voiceless velarized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̊ˠ] \
       \is the representation of the \
       \voiceless velarized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̊ˠ"
        `shouldBe`
        "voiceless velarized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̥ˠ] \
       \is the representation of the \
       \voiceless velarized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̥ˠ"
        `shouldBe`
        "voiceless velarized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̊ˠ] \
       \is the representation of the \
       \voiceless velarized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̊ˠ"
        `shouldBe`
        "voiceless velarized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̥ˠ] \
       \is the representation of the \
       \voiceless velarized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̥ˠ"
        `shouldBe`
        "voiceless velarized dental fricative pulmonic egressive consonant"
  describe "voiceless pharyngealized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [θˤ] \
       \is the representation of the \
       \voiceless pharyngealized dental fricative pulmonic egressive consonant" $
      describeIPA "θˤ"
        `shouldBe`
        "voiceless pharyngealized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̊ˤ"
        `shouldBe`
        "voiceless pharyngealized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̥ˤ"
        `shouldBe`
        "voiceless pharyngealized dental fricative pulmonic egressive consonant"
  describe "voiceless aspirated dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [θʰ] \
       \is the representation of the \
       \voiceless aspirated dental fricative pulmonic egressive consonant" $
      describeIPA "θʰ"
        `shouldBe`
        "voiceless aspirated dental fricative pulmonic egressive consonant"
    it "should be that: [ð̥ʰ] \
       \is the representation of the \
       \voiceless aspirated dental fricative pulmonic egressive consonant" $
      describeIPA "ð̥ʰ"
        `shouldBe`
        "voiceless aspirated dental fricative pulmonic egressive consonant"
    it "should be that: [ð̊ʰ] \
       \is the representation of the \
       \voiceless aspirated dental fricative pulmonic egressive consonant" $
      describeIPA "ð̊ʰ"
        `shouldBe`
        "voiceless aspirated dental fricative pulmonic egressive consonant"
  describe "voiceless aspirated labialized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [θʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized dental fricative pulmonic egressive consonant" $
      describeIPA "θʰʷ"
        `shouldBe`
        "voiceless aspirated labialized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized dental fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatalized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [θʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized dental fricative pulmonic egressive consonant" $
      describeIPA "θʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized dental fricative pulmonic egressive consonant"
  describe "voiceless aspirated velarized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [θʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized dental fricative pulmonic egressive consonant" $
      describeIPA "θʰˠ"
        `shouldBe`
        "voiceless aspirated velarized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized dental fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [θʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized dental fricative pulmonic egressive consonant" $
      describeIPA "θʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized dental fricative pulmonic egressive consonant"
  describe "voiced dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [ð] \
       \is the representation of the \
       \voiced dental fricative pulmonic egressive consonant" $
      describeIPA "ð"
        `shouldBe`
        "voiced dental fricative pulmonic egressive consonant"
    it "should be that: [θ̬] \
       \is the representation of the \
       \voiced dental fricative pulmonic egressive consonant" $
      describeIPA "θ̬"
        `shouldBe`
        "voiced dental fricative pulmonic egressive consonant"
    it "should be that: [θ̬] \
       \is the representation of the \
       \voiced dental fricative pulmonic egressive consonant" $
      describeIPA "θ̬"
        `shouldBe`
        "voiced dental fricative pulmonic egressive consonant"
  describe "voiced labialized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [ðʷ] \
       \is the representation of the \
       \voiced labialized dental fricative pulmonic egressive consonant" $
      describeIPA "ðʷ"
        `shouldBe`
        "voiced labialized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̬ʷ] \
       \is the representation of the \
       \voiced labialized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̬ʷ"
        `shouldBe`
        "voiced labialized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̬ʷ] \
       \is the representation of the \
       \voiced labialized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̬ʷ"
        `shouldBe`
        "voiced labialized dental fricative pulmonic egressive consonant"
  describe "voiced palatalized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [ðʲ] \
       \is the representation of the \
       \voiced palatalized dental fricative pulmonic egressive consonant" $
      describeIPA "ðʲ"
        `shouldBe`
        "voiced palatalized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̬ʲ] \
       \is the representation of the \
       \voiced palatalized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̬ʲ"
        `shouldBe`
        "voiced palatalized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̬ʲ] \
       \is the representation of the \
       \voiced palatalized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̬ʲ"
        `shouldBe`
        "voiced palatalized dental fricative pulmonic egressive consonant"
  describe "voiced velarized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [ðˠ] \
       \is the representation of the \
       \voiced velarized dental fricative pulmonic egressive consonant" $
      describeIPA "ðˠ"
        `shouldBe`
        "voiced velarized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̬ˠ] \
       \is the representation of the \
       \voiced velarized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̬ˠ"
        `shouldBe`
        "voiced velarized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̬ˠ] \
       \is the representation of the \
       \voiced velarized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̬ˠ"
        `shouldBe`
        "voiced velarized dental fricative pulmonic egressive consonant"
  describe "voiced pharyngealized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [ðˤ] \
       \is the representation of the \
       \voiced pharyngealized dental fricative pulmonic egressive consonant" $
      describeIPA "ðˤ"
        `shouldBe`
        "voiced pharyngealized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̬ˤ"
        `shouldBe`
        "voiced pharyngealized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̬ˤ"
        `shouldBe`
        "voiced pharyngealized dental fricative pulmonic egressive consonant"
  describe "voiced aspirated dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [ðʰ] \
       \is the representation of the \
       \voiced aspirated dental fricative pulmonic egressive consonant" $
      describeIPA "ðʰ"
        `shouldBe`
        "voiced aspirated dental fricative pulmonic egressive consonant"
    it "should be that: [ð̬ʰ] \
       \is the representation of the \
       \voiced aspirated dental fricative pulmonic egressive consonant" $
      describeIPA "ð̬ʰ"
        `shouldBe`
        "voiced aspirated dental fricative pulmonic egressive consonant"
    it "should be that: [θ̬ʰ] \
       \is the representation of the \
       \voiced aspirated dental fricative pulmonic egressive consonant" $
      describeIPA "θ̬ʰ"
        `shouldBe`
        "voiced aspirated dental fricative pulmonic egressive consonant"
  describe "voiced aspirated labialized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [ðʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized dental fricative pulmonic egressive consonant" $
      describeIPA "ðʰʷ"
        `shouldBe`
        "voiced aspirated labialized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized dental fricative pulmonic egressive consonant"
  describe "voiced aspirated palatalized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [ðʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized dental fricative pulmonic egressive consonant" $
      describeIPA "ðʰʲ"
        `shouldBe`
        "voiced aspirated palatalized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized dental fricative pulmonic egressive consonant"
  describe "voiced aspirated velarized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [ðʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized dental fricative pulmonic egressive consonant" $
      describeIPA "ðʰˠ"
        `shouldBe`
        "voiced aspirated velarized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized dental fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized dental fricative pulmonic egressive consonant" $
    do
    it "should be that: [ðʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized dental fricative pulmonic egressive consonant" $
      describeIPA "ðʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized dental fricative pulmonic egressive consonant"
    it "should be that: [ð̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized dental fricative pulmonic egressive consonant" $
      describeIPA "ð̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized dental fricative pulmonic egressive consonant"
    it "should be that: [θ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized dental fricative pulmonic egressive consonant" $
      describeIPA "θ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized dental fricative pulmonic egressive consonant"
  describe "voiceless alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [s] \
       \is the representation of the \
       \voiceless alveolar fricative pulmonic egressive consonant" $
      describeIPA "s"
        `shouldBe`
        "voiceless alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̊] \
       \is the representation of the \
       \voiceless alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̊"
        `shouldBe`
        "voiceless alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̥] \
       \is the representation of the \
       \voiceless alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̥"
        `shouldBe`
        "voiceless alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̊] \
       \is the representation of the \
       \voiceless alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̊"
        `shouldBe`
        "voiceless alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̥] \
       \is the representation of the \
       \voiceless alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̥"
        `shouldBe`
        "voiceless alveolar fricative pulmonic egressive consonant"
  describe "voiceless labialized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [sʷ] \
       \is the representation of the \
       \voiceless labialized alveolar fricative pulmonic egressive consonant" $
      describeIPA "sʷ"
        `shouldBe`
        "voiceless labialized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̊ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̊ʷ"
        `shouldBe`
        "voiceless labialized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̥ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̥ʷ"
        `shouldBe`
        "voiceless labialized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̊ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̊ʷ"
        `shouldBe`
        "voiceless labialized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̥ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̥ʷ"
        `shouldBe`
        "voiceless labialized alveolar fricative pulmonic egressive consonant"
  describe "voiceless palatalized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [sʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar fricative pulmonic egressive consonant" $
      describeIPA "sʲ"
        `shouldBe`
        "voiceless palatalized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̊ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̊ʲ"
        `shouldBe`
        "voiceless palatalized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̥ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̥ʲ"
        `shouldBe`
        "voiceless palatalized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̊ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̊ʲ"
        `shouldBe`
        "voiceless palatalized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̥ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̥ʲ"
        `shouldBe`
        "voiceless palatalized alveolar fricative pulmonic egressive consonant"
  describe "voiceless velarized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [sˠ] \
       \is the representation of the \
       \voiceless velarized alveolar fricative pulmonic egressive consonant" $
      describeIPA "sˠ"
        `shouldBe`
        "voiceless velarized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̊ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̊ˠ"
        `shouldBe`
        "voiceless velarized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̥ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̥ˠ"
        `shouldBe`
        "voiceless velarized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̊ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̊ˠ"
        `shouldBe`
        "voiceless velarized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̥ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̥ˠ"
        `shouldBe`
        "voiceless velarized alveolar fricative pulmonic egressive consonant"
  describe "voiceless pharyngealized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [sˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar fricative pulmonic egressive consonant" $
      describeIPA "sˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̊ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̥ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̊ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̥ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar fricative pulmonic egressive consonant"
  describe "voiceless aspirated alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [sʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar fricative pulmonic egressive consonant" $
      describeIPA "sʰ"
        `shouldBe`
        "voiceless aspirated alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̥ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̥ʰ"
        `shouldBe`
        "voiceless aspirated alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̊ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̊ʰ"
        `shouldBe`
        "voiceless aspirated alveolar fricative pulmonic egressive consonant"
  describe "voiceless aspirated labialized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [sʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar fricative pulmonic egressive consonant" $
      describeIPA "sʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatalized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [sʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar fricative pulmonic egressive consonant" $
      describeIPA "sʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar fricative pulmonic egressive consonant"
  describe "voiceless aspirated velarized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [sʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar fricative pulmonic egressive consonant" $
      describeIPA "sʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [sʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
      describeIPA "sʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar fricative pulmonic egressive consonant"
  describe "voiced alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [z] \
       \is the representation of the \
       \voiced alveolar fricative pulmonic egressive consonant" $
      describeIPA "z"
        `shouldBe`
        "voiced alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̬] \
       \is the representation of the \
       \voiced alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̬"
        `shouldBe`
        "voiced alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̬] \
       \is the representation of the \
       \voiced alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̬"
        `shouldBe`
        "voiced alveolar fricative pulmonic egressive consonant"
  describe "voiced labialized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [zʷ] \
       \is the representation of the \
       \voiced labialized alveolar fricative pulmonic egressive consonant" $
      describeIPA "zʷ"
        `shouldBe`
        "voiced labialized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̬ʷ] \
       \is the representation of the \
       \voiced labialized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̬ʷ"
        `shouldBe`
        "voiced labialized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̬ʷ] \
       \is the representation of the \
       \voiced labialized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̬ʷ"
        `shouldBe`
        "voiced labialized alveolar fricative pulmonic egressive consonant"
  describe "voiced palatalized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [zʲ] \
       \is the representation of the \
       \voiced palatalized alveolar fricative pulmonic egressive consonant" $
      describeIPA "zʲ"
        `shouldBe`
        "voiced palatalized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̬ʲ] \
       \is the representation of the \
       \voiced palatalized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̬ʲ"
        `shouldBe`
        "voiced palatalized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̬ʲ] \
       \is the representation of the \
       \voiced palatalized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̬ʲ"
        `shouldBe`
        "voiced palatalized alveolar fricative pulmonic egressive consonant"
  describe "voiced velarized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [zˠ] \
       \is the representation of the \
       \voiced velarized alveolar fricative pulmonic egressive consonant" $
      describeIPA "zˠ"
        `shouldBe`
        "voiced velarized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̬ˠ] \
       \is the representation of the \
       \voiced velarized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̬ˠ"
        `shouldBe`
        "voiced velarized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̬ˠ] \
       \is the representation of the \
       \voiced velarized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̬ˠ"
        `shouldBe`
        "voiced velarized alveolar fricative pulmonic egressive consonant"
  describe "voiced pharyngealized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [zˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar fricative pulmonic egressive consonant" $
      describeIPA "zˤ"
        `shouldBe`
        "voiced pharyngealized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̬ˤ"
        `shouldBe`
        "voiced pharyngealized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̬ˤ"
        `shouldBe`
        "voiced pharyngealized alveolar fricative pulmonic egressive consonant"
  describe "voiced aspirated alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [zʰ] \
       \is the representation of the \
       \voiced aspirated alveolar fricative pulmonic egressive consonant" $
      describeIPA "zʰ"
        `shouldBe`
        "voiced aspirated alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̬ʰ] \
       \is the representation of the \
       \voiced aspirated alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̬ʰ"
        `shouldBe`
        "voiced aspirated alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̬ʰ] \
       \is the representation of the \
       \voiced aspirated alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̬ʰ"
        `shouldBe`
        "voiced aspirated alveolar fricative pulmonic egressive consonant"
  describe "voiced aspirated labialized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [zʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar fricative pulmonic egressive consonant" $
      describeIPA "zʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar fricative pulmonic egressive consonant"
  describe "voiced aspirated palatalized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [zʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar fricative pulmonic egressive consonant" $
      describeIPA "zʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar fricative pulmonic egressive consonant"
  describe "voiced aspirated velarized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [zʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar fricative pulmonic egressive consonant" $
      describeIPA "zʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [zʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
      describeIPA "zʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar fricative pulmonic egressive consonant"
    it "should be that: [z̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
      describeIPA "z̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar fricative pulmonic egressive consonant"
    it "should be that: [s̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
      describeIPA "s̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar fricative pulmonic egressive consonant"
  describe "voiceless post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʃ] \
       \is the representation of the \
       \voiceless post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ"
        `shouldBe`
        "voiceless post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̊] \
       \is the representation of the \
       \voiceless post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̊"
        `shouldBe`
        "voiceless post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̥] \
       \is the representation of the \
       \voiceless post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̥"
        `shouldBe`
        "voiceless post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̊] \
       \is the representation of the \
       \voiceless post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̊"
        `shouldBe`
        "voiceless post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̥] \
       \is the representation of the \
       \voiceless post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̥"
        `shouldBe`
        "voiceless post-alveolar fricative pulmonic egressive consonant"
  describe "voiceless labialized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʃʷ] \
       \is the representation of the \
       \voiceless labialized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃʷ"
        `shouldBe`
        "voiceless labialized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̊ʷ] \
       \is the representation of the \
       \voiceless labialized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̊ʷ"
        `shouldBe`
        "voiceless labialized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̥ʷ] \
       \is the representation of the \
       \voiceless labialized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̥ʷ"
        `shouldBe`
        "voiceless labialized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̊ʷ] \
       \is the representation of the \
       \voiceless labialized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̊ʷ"
        `shouldBe`
        "voiceless labialized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̥ʷ] \
       \is the representation of the \
       \voiceless labialized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̥ʷ"
        `shouldBe`
        "voiceless labialized post-alveolar fricative pulmonic egressive consonant"
  describe "voiceless palatalized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʃʲ] \
       \is the representation of the \
       \voiceless palatalized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃʲ"
        `shouldBe`
        "voiceless palatalized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̊ʲ"
        `shouldBe`
        "voiceless palatalized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̥ʲ"
        `shouldBe`
        "voiceless palatalized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̊ʲ"
        `shouldBe`
        "voiceless palatalized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̥ʲ"
        `shouldBe`
        "voiceless palatalized post-alveolar fricative pulmonic egressive consonant"
  describe "voiceless velarized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʃˠ] \
       \is the representation of the \
       \voiceless velarized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃˠ"
        `shouldBe`
        "voiceless velarized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̊ˠ] \
       \is the representation of the \
       \voiceless velarized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̊ˠ"
        `shouldBe`
        "voiceless velarized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̥ˠ] \
       \is the representation of the \
       \voiceless velarized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̥ˠ"
        `shouldBe`
        "voiceless velarized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̊ˠ] \
       \is the representation of the \
       \voiceless velarized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̊ˠ"
        `shouldBe`
        "voiceless velarized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̥ˠ] \
       \is the representation of the \
       \voiceless velarized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̥ˠ"
        `shouldBe`
        "voiceless velarized post-alveolar fricative pulmonic egressive consonant"
  describe "voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʃˤ] \
       \is the representation of the \
       \voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃˤ"
        `shouldBe`
        "voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant"
  describe "voiceless aspirated post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʃʰ] \
       \is the representation of the \
       \voiceless aspirated post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃʰ"
        `shouldBe`
        "voiceless aspirated post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̥ʰ"
        `shouldBe`
        "voiceless aspirated post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̊ʰ"
        `shouldBe`
        "voiceless aspirated post-alveolar fricative pulmonic egressive consonant"
  describe "voiceless aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʃʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃʰʷ"
        `shouldBe`
        "voiceless aspirated labialized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized post-alveolar fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʃʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized post-alveolar fricative pulmonic egressive consonant"
  describe "voiceless aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʃʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃʰˠ"
        `shouldBe`
        "voiceless aspirated velarized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized post-alveolar fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʃʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant"
  describe "voiced post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʒ] \
       \is the representation of the \
       \voiced post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ"
        `shouldBe`
        "voiced post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̬] \
       \is the representation of the \
       \voiced post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̬"
        `shouldBe`
        "voiced post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̬] \
       \is the representation of the \
       \voiced post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̬"
        `shouldBe`
        "voiced post-alveolar fricative pulmonic egressive consonant"
  describe "voiced labialized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʒʷ] \
       \is the representation of the \
       \voiced labialized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒʷ"
        `shouldBe`
        "voiced labialized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̬ʷ] \
       \is the representation of the \
       \voiced labialized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̬ʷ"
        `shouldBe`
        "voiced labialized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̬ʷ] \
       \is the representation of the \
       \voiced labialized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̬ʷ"
        `shouldBe`
        "voiced labialized post-alveolar fricative pulmonic egressive consonant"
  describe "voiced palatalized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʒʲ] \
       \is the representation of the \
       \voiced palatalized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒʲ"
        `shouldBe`
        "voiced palatalized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̬ʲ] \
       \is the representation of the \
       \voiced palatalized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̬ʲ"
        `shouldBe`
        "voiced palatalized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̬ʲ] \
       \is the representation of the \
       \voiced palatalized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̬ʲ"
        `shouldBe`
        "voiced palatalized post-alveolar fricative pulmonic egressive consonant"
  describe "voiced velarized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʒˠ] \
       \is the representation of the \
       \voiced velarized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒˠ"
        `shouldBe`
        "voiced velarized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̬ˠ] \
       \is the representation of the \
       \voiced velarized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̬ˠ"
        `shouldBe`
        "voiced velarized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̬ˠ] \
       \is the representation of the \
       \voiced velarized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̬ˠ"
        `shouldBe`
        "voiced velarized post-alveolar fricative pulmonic egressive consonant"
  describe "voiced pharyngealized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʒˤ] \
       \is the representation of the \
       \voiced pharyngealized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒˤ"
        `shouldBe`
        "voiced pharyngealized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̬ˤ"
        `shouldBe`
        "voiced pharyngealized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̬ˤ"
        `shouldBe`
        "voiced pharyngealized post-alveolar fricative pulmonic egressive consonant"
  describe "voiced aspirated post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʒʰ] \
       \is the representation of the \
       \voiced aspirated post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒʰ"
        `shouldBe`
        "voiced aspirated post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̬ʰ] \
       \is the representation of the \
       \voiced aspirated post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̬ʰ"
        `shouldBe`
        "voiced aspirated post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̬ʰ] \
       \is the representation of the \
       \voiced aspirated post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̬ʰ"
        `shouldBe`
        "voiced aspirated post-alveolar fricative pulmonic egressive consonant"
  describe "voiced aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʒʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒʰʷ"
        `shouldBe`
        "voiced aspirated labialized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized post-alveolar fricative pulmonic egressive consonant"
  describe "voiced aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʒʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒʰʲ"
        `shouldBe`
        "voiced aspirated palatalized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized post-alveolar fricative pulmonic egressive consonant"
  describe "voiced aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʒʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒʰˠ"
        `shouldBe`
        "voiced aspirated velarized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized post-alveolar fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʒʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʒ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʒ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant"
    it "should be that: [ʃ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
      describeIPA "ʃ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant"
  describe "voiceless retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʂ] \
       \is the representation of the \
       \voiceless retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ"
        `shouldBe`
        "voiceless retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̊] \
       \is the representation of the \
       \voiceless retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̊"
        `shouldBe`
        "voiceless retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̥] \
       \is the representation of the \
       \voiceless retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̥"
        `shouldBe`
        "voiceless retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̊] \
       \is the representation of the \
       \voiceless retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̊"
        `shouldBe`
        "voiceless retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̥] \
       \is the representation of the \
       \voiceless retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̥"
        `shouldBe`
        "voiceless retroflex fricative pulmonic egressive consonant"
  describe "voiceless labialized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʂʷ] \
       \is the representation of the \
       \voiceless labialized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂʷ"
        `shouldBe`
        "voiceless labialized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̊ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̊ʷ"
        `shouldBe`
        "voiceless labialized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̥ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̥ʷ"
        `shouldBe`
        "voiceless labialized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̊ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̊ʷ"
        `shouldBe`
        "voiceless labialized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̥ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̥ʷ"
        `shouldBe`
        "voiceless labialized retroflex fricative pulmonic egressive consonant"
  describe "voiceless palatalized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʂʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂʲ"
        `shouldBe`
        "voiceless palatalized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̊ʲ"
        `shouldBe`
        "voiceless palatalized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̥ʲ"
        `shouldBe`
        "voiceless palatalized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̊ʲ"
        `shouldBe`
        "voiceless palatalized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̥ʲ"
        `shouldBe`
        "voiceless palatalized retroflex fricative pulmonic egressive consonant"
  describe "voiceless velarized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʂˠ] \
       \is the representation of the \
       \voiceless velarized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂˠ"
        `shouldBe`
        "voiceless velarized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̊ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̊ˠ"
        `shouldBe`
        "voiceless velarized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̥ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̥ˠ"
        `shouldBe`
        "voiceless velarized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̊ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̊ˠ"
        `shouldBe`
        "voiceless velarized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̥ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̥ˠ"
        `shouldBe`
        "voiceless velarized retroflex fricative pulmonic egressive consonant"
  describe "voiceless pharyngealized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʂˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex fricative pulmonic egressive consonant"
  describe "voiceless aspirated retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʂʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂʰ"
        `shouldBe`
        "voiceless aspirated retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̥ʰ"
        `shouldBe`
        "voiceless aspirated retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̊ʰ"
        `shouldBe`
        "voiceless aspirated retroflex fricative pulmonic egressive consonant"
  describe "voiceless aspirated labialized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʂʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatalized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʂʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex fricative pulmonic egressive consonant"
  describe "voiceless aspirated velarized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʂʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʂʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex fricative pulmonic egressive consonant"
  describe "voiced retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʐ] \
       \is the representation of the \
       \voiced retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ"
        `shouldBe`
        "voiced retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̬] \
       \is the representation of the \
       \voiced retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̬"
        `shouldBe`
        "voiced retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̬] \
       \is the representation of the \
       \voiced retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̬"
        `shouldBe`
        "voiced retroflex fricative pulmonic egressive consonant"
  describe "voiced labialized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʐʷ] \
       \is the representation of the \
       \voiced labialized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐʷ"
        `shouldBe`
        "voiced labialized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̬ʷ] \
       \is the representation of the \
       \voiced labialized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̬ʷ"
        `shouldBe`
        "voiced labialized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̬ʷ] \
       \is the representation of the \
       \voiced labialized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̬ʷ"
        `shouldBe`
        "voiced labialized retroflex fricative pulmonic egressive consonant"
  describe "voiced palatalized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʐʲ] \
       \is the representation of the \
       \voiced palatalized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐʲ"
        `shouldBe`
        "voiced palatalized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̬ʲ] \
       \is the representation of the \
       \voiced palatalized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̬ʲ"
        `shouldBe`
        "voiced palatalized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̬ʲ] \
       \is the representation of the \
       \voiced palatalized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̬ʲ"
        `shouldBe`
        "voiced palatalized retroflex fricative pulmonic egressive consonant"
  describe "voiced velarized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʐˠ] \
       \is the representation of the \
       \voiced velarized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐˠ"
        `shouldBe`
        "voiced velarized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̬ˠ] \
       \is the representation of the \
       \voiced velarized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̬ˠ"
        `shouldBe`
        "voiced velarized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̬ˠ] \
       \is the representation of the \
       \voiced velarized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̬ˠ"
        `shouldBe`
        "voiced velarized retroflex fricative pulmonic egressive consonant"
  describe "voiced pharyngealized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʐˤ] \
       \is the representation of the \
       \voiced pharyngealized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐˤ"
        `shouldBe`
        "voiced pharyngealized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̬ˤ"
        `shouldBe`
        "voiced pharyngealized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̬ˤ"
        `shouldBe`
        "voiced pharyngealized retroflex fricative pulmonic egressive consonant"
  describe "voiced aspirated retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʐʰ] \
       \is the representation of the \
       \voiced aspirated retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐʰ"
        `shouldBe`
        "voiced aspirated retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̬ʰ] \
       \is the representation of the \
       \voiced aspirated retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̬ʰ"
        `shouldBe`
        "voiced aspirated retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̬ʰ] \
       \is the representation of the \
       \voiced aspirated retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̬ʰ"
        `shouldBe`
        "voiced aspirated retroflex fricative pulmonic egressive consonant"
  describe "voiced aspirated labialized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʐʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex fricative pulmonic egressive consonant"
  describe "voiced aspirated palatalized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʐʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex fricative pulmonic egressive consonant"
  describe "voiced aspirated velarized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʐʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʐʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʐ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʐ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex fricative pulmonic egressive consonant"
    it "should be that: [ʂ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
      describeIPA "ʂ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex fricative pulmonic egressive consonant"
  describe "voiceless palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ç] \
       \is the representation of the \
       \voiceless palatal fricative pulmonic egressive consonant" $
      describeIPA "ç"
        `shouldBe`
        "voiceless palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̊] \
       \is the representation of the \
       \voiceless palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̊"
        `shouldBe`
        "voiceless palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̥] \
       \is the representation of the \
       \voiceless palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̥"
        `shouldBe`
        "voiceless palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̊] \
       \is the representation of the \
       \voiceless palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̊"
        `shouldBe`
        "voiceless palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̥] \
       \is the representation of the \
       \voiceless palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̥"
        `shouldBe`
        "voiceless palatal fricative pulmonic egressive consonant"
  describe "voiceless labialized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [çʷ] \
       \is the representation of the \
       \voiceless labialized palatal fricative pulmonic egressive consonant" $
      describeIPA "çʷ"
        `shouldBe`
        "voiceless labialized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̊ʷ] \
       \is the representation of the \
       \voiceless labialized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̊ʷ"
        `shouldBe`
        "voiceless labialized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̥ʷ] \
       \is the representation of the \
       \voiceless labialized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̥ʷ"
        `shouldBe`
        "voiceless labialized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̊ʷ] \
       \is the representation of the \
       \voiceless labialized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̊ʷ"
        `shouldBe`
        "voiceless labialized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̥ʷ] \
       \is the representation of the \
       \voiceless labialized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̥ʷ"
        `shouldBe`
        "voiceless labialized palatal fricative pulmonic egressive consonant"
  describe "voiceless palatalized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [çʲ] \
       \is the representation of the \
       \voiceless palatalized palatal fricative pulmonic egressive consonant" $
      describeIPA "çʲ"
        `shouldBe`
        "voiceless palatalized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̊ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̊ʲ"
        `shouldBe`
        "voiceless palatalized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̥ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̥ʲ"
        `shouldBe`
        "voiceless palatalized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̊ʲ"
        `shouldBe`
        "voiceless palatalized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̥ʲ"
        `shouldBe`
        "voiceless palatalized palatal fricative pulmonic egressive consonant"
  describe "voiceless velarized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [çˠ] \
       \is the representation of the \
       \voiceless velarized palatal fricative pulmonic egressive consonant" $
      describeIPA "çˠ"
        `shouldBe`
        "voiceless velarized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̊ˠ] \
       \is the representation of the \
       \voiceless velarized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̊ˠ"
        `shouldBe`
        "voiceless velarized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̥ˠ] \
       \is the representation of the \
       \voiceless velarized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̥ˠ"
        `shouldBe`
        "voiceless velarized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̊ˠ] \
       \is the representation of the \
       \voiceless velarized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̊ˠ"
        `shouldBe`
        "voiceless velarized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̥ˠ] \
       \is the representation of the \
       \voiceless velarized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̥ˠ"
        `shouldBe`
        "voiceless velarized palatal fricative pulmonic egressive consonant"
  describe "voiceless pharyngealized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [çˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal fricative pulmonic egressive consonant" $
      describeIPA "çˤ"
        `shouldBe`
        "voiceless pharyngealized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̊ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̥ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [çʰ] \
       \is the representation of the \
       \voiceless aspirated palatal fricative pulmonic egressive consonant" $
      describeIPA "çʰ"
        `shouldBe`
        "voiceless aspirated palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̥ʰ"
        `shouldBe`
        "voiceless aspirated palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̊ʰ"
        `shouldBe`
        "voiceless aspirated palatal fricative pulmonic egressive consonant"
  describe "voiceless aspirated labialized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [çʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized palatal fricative pulmonic egressive consonant" $
      describeIPA "çʰʷ"
        `shouldBe`
        "voiceless aspirated labialized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized palatal fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatalized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [çʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized palatal fricative pulmonic egressive consonant" $
      describeIPA "çʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized palatal fricative pulmonic egressive consonant"
  describe "voiceless aspirated velarized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [çʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized palatal fricative pulmonic egressive consonant" $
      describeIPA "çʰˠ"
        `shouldBe`
        "voiceless aspirated velarized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized palatal fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [çʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
      describeIPA "çʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized palatal fricative pulmonic egressive consonant"
  describe "voiced palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʝ] \
       \is the representation of the \
       \voiced palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ"
        `shouldBe`
        "voiced palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̬] \
       \is the representation of the \
       \voiced palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̬"
        `shouldBe`
        "voiced palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̬] \
       \is the representation of the \
       \voiced palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̬"
        `shouldBe`
        "voiced palatal fricative pulmonic egressive consonant"
  describe "voiced labialized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʝʷ] \
       \is the representation of the \
       \voiced labialized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝʷ"
        `shouldBe`
        "voiced labialized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̬ʷ] \
       \is the representation of the \
       \voiced labialized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̬ʷ"
        `shouldBe`
        "voiced labialized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̬ʷ] \
       \is the representation of the \
       \voiced labialized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̬ʷ"
        `shouldBe`
        "voiced labialized palatal fricative pulmonic egressive consonant"
  describe "voiced palatalized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʝʲ] \
       \is the representation of the \
       \voiced palatalized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝʲ"
        `shouldBe`
        "voiced palatalized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̬ʲ] \
       \is the representation of the \
       \voiced palatalized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̬ʲ"
        `shouldBe`
        "voiced palatalized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̬ʲ] \
       \is the representation of the \
       \voiced palatalized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̬ʲ"
        `shouldBe`
        "voiced palatalized palatal fricative pulmonic egressive consonant"
  describe "voiced velarized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʝˠ] \
       \is the representation of the \
       \voiced velarized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝˠ"
        `shouldBe`
        "voiced velarized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̬ˠ] \
       \is the representation of the \
       \voiced velarized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̬ˠ"
        `shouldBe`
        "voiced velarized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̬ˠ] \
       \is the representation of the \
       \voiced velarized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̬ˠ"
        `shouldBe`
        "voiced velarized palatal fricative pulmonic egressive consonant"
  describe "voiced pharyngealized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʝˤ] \
       \is the representation of the \
       \voiced pharyngealized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝˤ"
        `shouldBe`
        "voiced pharyngealized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̬ˤ"
        `shouldBe`
        "voiced pharyngealized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̬ˤ"
        `shouldBe`
        "voiced pharyngealized palatal fricative pulmonic egressive consonant"
  describe "voiced aspirated palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʝʰ] \
       \is the representation of the \
       \voiced aspirated palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝʰ"
        `shouldBe`
        "voiced aspirated palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̬ʰ] \
       \is the representation of the \
       \voiced aspirated palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̬ʰ"
        `shouldBe`
        "voiced aspirated palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̬ʰ] \
       \is the representation of the \
       \voiced aspirated palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̬ʰ"
        `shouldBe`
        "voiced aspirated palatal fricative pulmonic egressive consonant"
  describe "voiced aspirated labialized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʝʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝʰʷ"
        `shouldBe`
        "voiced aspirated labialized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized palatal fricative pulmonic egressive consonant"
  describe "voiced aspirated palatalized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʝʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝʰʲ"
        `shouldBe`
        "voiced aspirated palatalized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized palatal fricative pulmonic egressive consonant"
  describe "voiced aspirated velarized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʝʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝʰˠ"
        `shouldBe`
        "voiced aspirated velarized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized palatal fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʝʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized palatal fricative pulmonic egressive consonant"
    it "should be that: [ʝ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
      describeIPA "ʝ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized palatal fricative pulmonic egressive consonant"
    it "should be that: [ç̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
      describeIPA "ç̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized palatal fricative pulmonic egressive consonant"
  describe "voiceless velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [x] \
       \is the representation of the \
       \voiceless velar fricative pulmonic egressive consonant" $
      describeIPA "x"
        `shouldBe`
        "voiceless velar fricative pulmonic egressive consonant"
    it "should be that: [x̊] \
       \is the representation of the \
       \voiceless velar fricative pulmonic egressive consonant" $
      describeIPA "x̊"
        `shouldBe`
        "voiceless velar fricative pulmonic egressive consonant"
    it "should be that: [x̥] \
       \is the representation of the \
       \voiceless velar fricative pulmonic egressive consonant" $
      describeIPA "x̥"
        `shouldBe`
        "voiceless velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̊] \
       \is the representation of the \
       \voiceless velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̊"
        `shouldBe`
        "voiceless velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̥] \
       \is the representation of the \
       \voiceless velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̥"
        `shouldBe`
        "voiceless velar fricative pulmonic egressive consonant"
  describe "voiceless labialized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [xʷ] \
       \is the representation of the \
       \voiceless labialized velar fricative pulmonic egressive consonant" $
      describeIPA "xʷ"
        `shouldBe`
        "voiceless labialized velar fricative pulmonic egressive consonant"
    it "should be that: [x̊ʷ] \
       \is the representation of the \
       \voiceless labialized velar fricative pulmonic egressive consonant" $
      describeIPA "x̊ʷ"
        `shouldBe`
        "voiceless labialized velar fricative pulmonic egressive consonant"
    it "should be that: [x̥ʷ] \
       \is the representation of the \
       \voiceless labialized velar fricative pulmonic egressive consonant" $
      describeIPA "x̥ʷ"
        `shouldBe`
        "voiceless labialized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̊ʷ] \
       \is the representation of the \
       \voiceless labialized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̊ʷ"
        `shouldBe`
        "voiceless labialized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̥ʷ] \
       \is the representation of the \
       \voiceless labialized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̥ʷ"
        `shouldBe`
        "voiceless labialized velar fricative pulmonic egressive consonant"
  describe "voiceless palatalized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [xʲ] \
       \is the representation of the \
       \voiceless palatalized velar fricative pulmonic egressive consonant" $
      describeIPA "xʲ"
        `shouldBe`
        "voiceless palatalized velar fricative pulmonic egressive consonant"
    it "should be that: [x̊ʲ] \
       \is the representation of the \
       \voiceless palatalized velar fricative pulmonic egressive consonant" $
      describeIPA "x̊ʲ"
        `shouldBe`
        "voiceless palatalized velar fricative pulmonic egressive consonant"
    it "should be that: [x̥ʲ] \
       \is the representation of the \
       \voiceless palatalized velar fricative pulmonic egressive consonant" $
      describeIPA "x̥ʲ"
        `shouldBe`
        "voiceless palatalized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̊ʲ"
        `shouldBe`
        "voiceless palatalized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̥ʲ"
        `shouldBe`
        "voiceless palatalized velar fricative pulmonic egressive consonant"
  describe "voiceless velarized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [xˠ] \
       \is the representation of the \
       \voiceless velarized velar fricative pulmonic egressive consonant" $
      describeIPA "xˠ"
        `shouldBe`
        "voiceless velarized velar fricative pulmonic egressive consonant"
    it "should be that: [x̊ˠ] \
       \is the representation of the \
       \voiceless velarized velar fricative pulmonic egressive consonant" $
      describeIPA "x̊ˠ"
        `shouldBe`
        "voiceless velarized velar fricative pulmonic egressive consonant"
    it "should be that: [x̥ˠ] \
       \is the representation of the \
       \voiceless velarized velar fricative pulmonic egressive consonant" $
      describeIPA "x̥ˠ"
        `shouldBe`
        "voiceless velarized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̊ˠ] \
       \is the representation of the \
       \voiceless velarized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̊ˠ"
        `shouldBe`
        "voiceless velarized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̥ˠ] \
       \is the representation of the \
       \voiceless velarized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̥ˠ"
        `shouldBe`
        "voiceless velarized velar fricative pulmonic egressive consonant"
  describe "voiceless pharyngealized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [xˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar fricative pulmonic egressive consonant" $
      describeIPA "xˤ"
        `shouldBe`
        "voiceless pharyngealized velar fricative pulmonic egressive consonant"
    it "should be that: [x̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar fricative pulmonic egressive consonant" $
      describeIPA "x̊ˤ"
        `shouldBe`
        "voiceless pharyngealized velar fricative pulmonic egressive consonant"
    it "should be that: [x̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar fricative pulmonic egressive consonant" $
      describeIPA "x̥ˤ"
        `shouldBe`
        "voiceless pharyngealized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized velar fricative pulmonic egressive consonant"
  describe "voiceless aspirated velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [xʰ] \
       \is the representation of the \
       \voiceless aspirated velar fricative pulmonic egressive consonant" $
      describeIPA "xʰ"
        `shouldBe`
        "voiceless aspirated velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̥ʰ"
        `shouldBe`
        "voiceless aspirated velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̊ʰ"
        `shouldBe`
        "voiceless aspirated velar fricative pulmonic egressive consonant"
  describe "voiceless aspirated labialized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [xʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized velar fricative pulmonic egressive consonant" $
      describeIPA "xʰʷ"
        `shouldBe`
        "voiceless aspirated labialized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized velar fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatalized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [xʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized velar fricative pulmonic egressive consonant" $
      describeIPA "xʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized velar fricative pulmonic egressive consonant"
  describe "voiceless aspirated velarized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [xʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized velar fricative pulmonic egressive consonant" $
      describeIPA "xʰˠ"
        `shouldBe`
        "voiceless aspirated velarized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized velar fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [xʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized velar fricative pulmonic egressive consonant" $
      describeIPA "xʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized velar fricative pulmonic egressive consonant"
  describe "voiced velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɣ] \
       \is the representation of the \
       \voiced velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ"
        `shouldBe`
        "voiced velar fricative pulmonic egressive consonant"
    it "should be that: [x̬] \
       \is the representation of the \
       \voiced velar fricative pulmonic egressive consonant" $
      describeIPA "x̬"
        `shouldBe`
        "voiced velar fricative pulmonic egressive consonant"
    it "should be that: [x̬] \
       \is the representation of the \
       \voiced velar fricative pulmonic egressive consonant" $
      describeIPA "x̬"
        `shouldBe`
        "voiced velar fricative pulmonic egressive consonant"
  describe "voiced labialized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɣʷ] \
       \is the representation of the \
       \voiced labialized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣʷ"
        `shouldBe`
        "voiced labialized velar fricative pulmonic egressive consonant"
    it "should be that: [x̬ʷ] \
       \is the representation of the \
       \voiced labialized velar fricative pulmonic egressive consonant" $
      describeIPA "x̬ʷ"
        `shouldBe`
        "voiced labialized velar fricative pulmonic egressive consonant"
    it "should be that: [x̬ʷ] \
       \is the representation of the \
       \voiced labialized velar fricative pulmonic egressive consonant" $
      describeIPA "x̬ʷ"
        `shouldBe`
        "voiced labialized velar fricative pulmonic egressive consonant"
  describe "voiced palatalized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɣʲ] \
       \is the representation of the \
       \voiced palatalized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣʲ"
        `shouldBe`
        "voiced palatalized velar fricative pulmonic egressive consonant"
    it "should be that: [x̬ʲ] \
       \is the representation of the \
       \voiced palatalized velar fricative pulmonic egressive consonant" $
      describeIPA "x̬ʲ"
        `shouldBe`
        "voiced palatalized velar fricative pulmonic egressive consonant"
    it "should be that: [x̬ʲ] \
       \is the representation of the \
       \voiced palatalized velar fricative pulmonic egressive consonant" $
      describeIPA "x̬ʲ"
        `shouldBe`
        "voiced palatalized velar fricative pulmonic egressive consonant"
  describe "voiced velarized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɣˠ] \
       \is the representation of the \
       \voiced velarized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣˠ"
        `shouldBe`
        "voiced velarized velar fricative pulmonic egressive consonant"
    it "should be that: [x̬ˠ] \
       \is the representation of the \
       \voiced velarized velar fricative pulmonic egressive consonant" $
      describeIPA "x̬ˠ"
        `shouldBe`
        "voiced velarized velar fricative pulmonic egressive consonant"
    it "should be that: [x̬ˠ] \
       \is the representation of the \
       \voiced velarized velar fricative pulmonic egressive consonant" $
      describeIPA "x̬ˠ"
        `shouldBe`
        "voiced velarized velar fricative pulmonic egressive consonant"
  describe "voiced pharyngealized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɣˤ] \
       \is the representation of the \
       \voiced pharyngealized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣˤ"
        `shouldBe`
        "voiced pharyngealized velar fricative pulmonic egressive consonant"
    it "should be that: [x̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized velar fricative pulmonic egressive consonant" $
      describeIPA "x̬ˤ"
        `shouldBe`
        "voiced pharyngealized velar fricative pulmonic egressive consonant"
    it "should be that: [x̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized velar fricative pulmonic egressive consonant" $
      describeIPA "x̬ˤ"
        `shouldBe`
        "voiced pharyngealized velar fricative pulmonic egressive consonant"
  describe "voiced aspirated velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɣʰ] \
       \is the representation of the \
       \voiced aspirated velar fricative pulmonic egressive consonant" $
      describeIPA "ɣʰ"
        `shouldBe`
        "voiced aspirated velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̬ʰ] \
       \is the representation of the \
       \voiced aspirated velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̬ʰ"
        `shouldBe`
        "voiced aspirated velar fricative pulmonic egressive consonant"
    it "should be that: [x̬ʰ] \
       \is the representation of the \
       \voiced aspirated velar fricative pulmonic egressive consonant" $
      describeIPA "x̬ʰ"
        `shouldBe`
        "voiced aspirated velar fricative pulmonic egressive consonant"
  describe "voiced aspirated labialized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɣʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣʰʷ"
        `shouldBe`
        "voiced aspirated labialized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized velar fricative pulmonic egressive consonant"
    it "should be that: [x̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized velar fricative pulmonic egressive consonant" $
      describeIPA "x̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized velar fricative pulmonic egressive consonant"
  describe "voiced aspirated palatalized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɣʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣʰʲ"
        `shouldBe`
        "voiced aspirated palatalized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized velar fricative pulmonic egressive consonant"
    it "should be that: [x̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized velar fricative pulmonic egressive consonant" $
      describeIPA "x̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized velar fricative pulmonic egressive consonant"
  describe "voiced aspirated velarized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɣʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣʰˠ"
        `shouldBe`
        "voiced aspirated velarized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized velar fricative pulmonic egressive consonant"
    it "should be that: [x̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized velar fricative pulmonic egressive consonant" $
      describeIPA "x̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized velar fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɣʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized velar fricative pulmonic egressive consonant"
    it "should be that: [ɣ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized velar fricative pulmonic egressive consonant" $
      describeIPA "ɣ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized velar fricative pulmonic egressive consonant"
    it "should be that: [x̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized velar fricative pulmonic egressive consonant" $
      describeIPA "x̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized velar fricative pulmonic egressive consonant"
  describe "voiceless uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [χ] \
       \is the representation of the \
       \voiceless uvular fricative pulmonic egressive consonant" $
      describeIPA "χ"
        `shouldBe`
        "voiceless uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̊] \
       \is the representation of the \
       \voiceless uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̊"
        `shouldBe`
        "voiceless uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̥] \
       \is the representation of the \
       \voiceless uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̥"
        `shouldBe`
        "voiceless uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̊] \
       \is the representation of the \
       \voiceless uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̊"
        `shouldBe`
        "voiceless uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̥] \
       \is the representation of the \
       \voiceless uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̥"
        `shouldBe`
        "voiceless uvular fricative pulmonic egressive consonant"
  describe "voiceless labialized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [χʷ] \
       \is the representation of the \
       \voiceless labialized uvular fricative pulmonic egressive consonant" $
      describeIPA "χʷ"
        `shouldBe`
        "voiceless labialized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̊ʷ] \
       \is the representation of the \
       \voiceless labialized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̊ʷ"
        `shouldBe`
        "voiceless labialized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̥ʷ] \
       \is the representation of the \
       \voiceless labialized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̥ʷ"
        `shouldBe`
        "voiceless labialized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̊ʷ] \
       \is the representation of the \
       \voiceless labialized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̊ʷ"
        `shouldBe`
        "voiceless labialized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̥ʷ] \
       \is the representation of the \
       \voiceless labialized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̥ʷ"
        `shouldBe`
        "voiceless labialized uvular fricative pulmonic egressive consonant"
  describe "voiceless palatalized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [χʲ] \
       \is the representation of the \
       \voiceless palatalized uvular fricative pulmonic egressive consonant" $
      describeIPA "χʲ"
        `shouldBe`
        "voiceless palatalized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̊ʲ"
        `shouldBe`
        "voiceless palatalized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̥ʲ"
        `shouldBe`
        "voiceless palatalized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̊ʲ"
        `shouldBe`
        "voiceless palatalized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̥ʲ"
        `shouldBe`
        "voiceless palatalized uvular fricative pulmonic egressive consonant"
  describe "voiceless velarized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [χˠ] \
       \is the representation of the \
       \voiceless velarized uvular fricative pulmonic egressive consonant" $
      describeIPA "χˠ"
        `shouldBe`
        "voiceless velarized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̊ˠ] \
       \is the representation of the \
       \voiceless velarized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̊ˠ"
        `shouldBe`
        "voiceless velarized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̥ˠ] \
       \is the representation of the \
       \voiceless velarized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̥ˠ"
        `shouldBe`
        "voiceless velarized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̊ˠ] \
       \is the representation of the \
       \voiceless velarized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̊ˠ"
        `shouldBe`
        "voiceless velarized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̥ˠ] \
       \is the representation of the \
       \voiceless velarized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̥ˠ"
        `shouldBe`
        "voiceless velarized uvular fricative pulmonic egressive consonant"
  describe "voiceless pharyngealized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [χˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular fricative pulmonic egressive consonant" $
      describeIPA "χˤ"
        `shouldBe`
        "voiceless pharyngealized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular fricative pulmonic egressive consonant"
  describe "voiceless aspirated uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [χʰ] \
       \is the representation of the \
       \voiceless aspirated uvular fricative pulmonic egressive consonant" $
      describeIPA "χʰ"
        `shouldBe`
        "voiceless aspirated uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̥ʰ"
        `shouldBe`
        "voiceless aspirated uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̊ʰ"
        `shouldBe`
        "voiceless aspirated uvular fricative pulmonic egressive consonant"
  describe "voiceless aspirated labialized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [χʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized uvular fricative pulmonic egressive consonant" $
      describeIPA "χʰʷ"
        `shouldBe`
        "voiceless aspirated labialized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized uvular fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatalized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [χʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized uvular fricative pulmonic egressive consonant" $
      describeIPA "χʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized uvular fricative pulmonic egressive consonant"
  describe "voiceless aspirated velarized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [χʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized uvular fricative pulmonic egressive consonant" $
      describeIPA "χʰˠ"
        `shouldBe`
        "voiceless aspirated velarized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized uvular fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [χʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
      describeIPA "χʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized uvular fricative pulmonic egressive consonant"
  describe "voiced uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʁ] \
       \is the representation of the \
       \voiced uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ"
        `shouldBe`
        "voiced uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̬] \
       \is the representation of the \
       \voiced uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̬"
        `shouldBe`
        "voiced uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̬] \
       \is the representation of the \
       \voiced uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̬"
        `shouldBe`
        "voiced uvular fricative pulmonic egressive consonant"
  describe "voiced labialized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʁʷ] \
       \is the representation of the \
       \voiced labialized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁʷ"
        `shouldBe`
        "voiced labialized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̬ʷ] \
       \is the representation of the \
       \voiced labialized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̬ʷ"
        `shouldBe`
        "voiced labialized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̬ʷ] \
       \is the representation of the \
       \voiced labialized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̬ʷ"
        `shouldBe`
        "voiced labialized uvular fricative pulmonic egressive consonant"
  describe "voiced palatalized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʁʲ] \
       \is the representation of the \
       \voiced palatalized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁʲ"
        `shouldBe`
        "voiced palatalized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̬ʲ] \
       \is the representation of the \
       \voiced palatalized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̬ʲ"
        `shouldBe`
        "voiced palatalized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̬ʲ] \
       \is the representation of the \
       \voiced palatalized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̬ʲ"
        `shouldBe`
        "voiced palatalized uvular fricative pulmonic egressive consonant"
  describe "voiced velarized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʁˠ] \
       \is the representation of the \
       \voiced velarized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁˠ"
        `shouldBe`
        "voiced velarized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̬ˠ] \
       \is the representation of the \
       \voiced velarized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̬ˠ"
        `shouldBe`
        "voiced velarized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̬ˠ] \
       \is the representation of the \
       \voiced velarized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̬ˠ"
        `shouldBe`
        "voiced velarized uvular fricative pulmonic egressive consonant"
  describe "voiced pharyngealized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʁˤ] \
       \is the representation of the \
       \voiced pharyngealized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁˤ"
        `shouldBe`
        "voiced pharyngealized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̬ˤ"
        `shouldBe`
        "voiced pharyngealized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̬ˤ"
        `shouldBe`
        "voiced pharyngealized uvular fricative pulmonic egressive consonant"
  describe "voiced aspirated uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʁʰ] \
       \is the representation of the \
       \voiced aspirated uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁʰ"
        `shouldBe`
        "voiced aspirated uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̬ʰ] \
       \is the representation of the \
       \voiced aspirated uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̬ʰ"
        `shouldBe`
        "voiced aspirated uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̬ʰ] \
       \is the representation of the \
       \voiced aspirated uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̬ʰ"
        `shouldBe`
        "voiced aspirated uvular fricative pulmonic egressive consonant"
  describe "voiced aspirated labialized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʁʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁʰʷ"
        `shouldBe`
        "voiced aspirated labialized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized uvular fricative pulmonic egressive consonant"
  describe "voiced aspirated palatalized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʁʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁʰʲ"
        `shouldBe`
        "voiced aspirated palatalized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized uvular fricative pulmonic egressive consonant"
  describe "voiced aspirated velarized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʁʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁʰˠ"
        `shouldBe`
        "voiced aspirated velarized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized uvular fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʁʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized uvular fricative pulmonic egressive consonant"
    it "should be that: [ʁ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
      describeIPA "ʁ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized uvular fricative pulmonic egressive consonant"
    it "should be that: [χ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
      describeIPA "χ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized uvular fricative pulmonic egressive consonant"
  describe "voiceless pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ħ] \
       \is the representation of the \
       \voiceless pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ"
        `shouldBe`
        "voiceless pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̊] \
       \is the representation of the \
       \voiceless pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̊"
        `shouldBe`
        "voiceless pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̥] \
       \is the representation of the \
       \voiceless pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̥"
        `shouldBe`
        "voiceless pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̊] \
       \is the representation of the \
       \voiceless pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̊"
        `shouldBe`
        "voiceless pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̥] \
       \is the representation of the \
       \voiceless pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̥"
        `shouldBe`
        "voiceless pharyngeal fricative pulmonic egressive consonant"
  describe "voiceless labialized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ħʷ] \
       \is the representation of the \
       \voiceless labialized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħʷ"
        `shouldBe`
        "voiceless labialized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̊ʷ] \
       \is the representation of the \
       \voiceless labialized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̊ʷ"
        `shouldBe`
        "voiceless labialized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̥ʷ] \
       \is the representation of the \
       \voiceless labialized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̥ʷ"
        `shouldBe`
        "voiceless labialized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̊ʷ] \
       \is the representation of the \
       \voiceless labialized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̊ʷ"
        `shouldBe`
        "voiceless labialized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̥ʷ] \
       \is the representation of the \
       \voiceless labialized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̥ʷ"
        `shouldBe`
        "voiceless labialized pharyngeal fricative pulmonic egressive consonant"
  describe "voiceless palatalized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ħʲ] \
       \is the representation of the \
       \voiceless palatalized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħʲ"
        `shouldBe`
        "voiceless palatalized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̊ʲ"
        `shouldBe`
        "voiceless palatalized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̥ʲ"
        `shouldBe`
        "voiceless palatalized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̊ʲ"
        `shouldBe`
        "voiceless palatalized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̥ʲ"
        `shouldBe`
        "voiceless palatalized pharyngeal fricative pulmonic egressive consonant"
  describe "voiceless velarized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ħˠ] \
       \is the representation of the \
       \voiceless velarized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħˠ"
        `shouldBe`
        "voiceless velarized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̊ˠ] \
       \is the representation of the \
       \voiceless velarized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̊ˠ"
        `shouldBe`
        "voiceless velarized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̥ˠ] \
       \is the representation of the \
       \voiceless velarized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̥ˠ"
        `shouldBe`
        "voiceless velarized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̊ˠ] \
       \is the representation of the \
       \voiceless velarized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̊ˠ"
        `shouldBe`
        "voiceless velarized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̥ˠ] \
       \is the representation of the \
       \voiceless velarized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̥ˠ"
        `shouldBe`
        "voiceless velarized pharyngeal fricative pulmonic egressive consonant"
  describe "voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ħˤ] \
       \is the representation of the \
       \voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħˤ"
        `shouldBe`
        "voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ħʰ] \
       \is the representation of the \
       \voiceless aspirated pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħʰ"
        `shouldBe`
        "voiceless aspirated pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̥ʰ"
        `shouldBe`
        "voiceless aspirated pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̊ʰ"
        `shouldBe`
        "voiceless aspirated pharyngeal fricative pulmonic egressive consonant"
  describe "voiceless aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ħʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħʰʷ"
        `shouldBe`
        "voiceless aspirated labialized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized pharyngeal fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ħʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized pharyngeal fricative pulmonic egressive consonant"
  describe "voiceless aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ħʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħʰˠ"
        `shouldBe`
        "voiceless aspirated velarized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized pharyngeal fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ħʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant"
  describe "voiced pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʕ] \
       \is the representation of the \
       \voiced pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ"
        `shouldBe`
        "voiced pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̬] \
       \is the representation of the \
       \voiced pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̬"
        `shouldBe`
        "voiced pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̬] \
       \is the representation of the \
       \voiced pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̬"
        `shouldBe`
        "voiced pharyngeal fricative pulmonic egressive consonant"
  describe "voiced labialized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʕʷ] \
       \is the representation of the \
       \voiced labialized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕʷ"
        `shouldBe`
        "voiced labialized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̬ʷ] \
       \is the representation of the \
       \voiced labialized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̬ʷ"
        `shouldBe`
        "voiced labialized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̬ʷ] \
       \is the representation of the \
       \voiced labialized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̬ʷ"
        `shouldBe`
        "voiced labialized pharyngeal fricative pulmonic egressive consonant"
  describe "voiced palatalized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʕʲ] \
       \is the representation of the \
       \voiced palatalized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕʲ"
        `shouldBe`
        "voiced palatalized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̬ʲ] \
       \is the representation of the \
       \voiced palatalized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̬ʲ"
        `shouldBe`
        "voiced palatalized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̬ʲ] \
       \is the representation of the \
       \voiced palatalized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̬ʲ"
        `shouldBe`
        "voiced palatalized pharyngeal fricative pulmonic egressive consonant"
  describe "voiced velarized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʕˠ] \
       \is the representation of the \
       \voiced velarized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕˠ"
        `shouldBe`
        "voiced velarized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̬ˠ] \
       \is the representation of the \
       \voiced velarized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̬ˠ"
        `shouldBe`
        "voiced velarized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̬ˠ] \
       \is the representation of the \
       \voiced velarized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̬ˠ"
        `shouldBe`
        "voiced velarized pharyngeal fricative pulmonic egressive consonant"
  describe "voiced pharyngealized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʕˤ] \
       \is the representation of the \
       \voiced pharyngealized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕˤ"
        `shouldBe`
        "voiced pharyngealized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̬ˤ"
        `shouldBe`
        "voiced pharyngealized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̬ˤ"
        `shouldBe`
        "voiced pharyngealized pharyngeal fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʕʰ] \
       \is the representation of the \
       \voiced aspirated pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕʰ"
        `shouldBe`
        "voiced aspirated pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̬ʰ] \
       \is the representation of the \
       \voiced aspirated pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̬ʰ"
        `shouldBe`
        "voiced aspirated pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̬ʰ] \
       \is the representation of the \
       \voiced aspirated pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̬ʰ"
        `shouldBe`
        "voiced aspirated pharyngeal fricative pulmonic egressive consonant"
  describe "voiced aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʕʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕʰʷ"
        `shouldBe`
        "voiced aspirated labialized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized pharyngeal fricative pulmonic egressive consonant"
  describe "voiced aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʕʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕʰʲ"
        `shouldBe`
        "voiced aspirated palatalized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized pharyngeal fricative pulmonic egressive consonant"
  describe "voiced aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʕʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕʰˠ"
        `shouldBe`
        "voiced aspirated velarized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized pharyngeal fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʕʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ʕ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ʕ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant"
    it "should be that: [ħ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
      describeIPA "ħ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant"
  describe "voiceless glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [h] \
       \is the representation of the \
       \voiceless glottal fricative pulmonic egressive consonant" $
      describeIPA "h"
        `shouldBe`
        "voiceless glottal fricative pulmonic egressive consonant"
    it "should be that: [h̊] \
       \is the representation of the \
       \voiceless glottal fricative pulmonic egressive consonant" $
      describeIPA "h̊"
        `shouldBe`
        "voiceless glottal fricative pulmonic egressive consonant"
    it "should be that: [h̥] \
       \is the representation of the \
       \voiceless glottal fricative pulmonic egressive consonant" $
      describeIPA "h̥"
        `shouldBe`
        "voiceless glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̊] \
       \is the representation of the \
       \voiceless glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̊"
        `shouldBe`
        "voiceless glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̥] \
       \is the representation of the \
       \voiceless glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̥"
        `shouldBe`
        "voiceless glottal fricative pulmonic egressive consonant"
  describe "voiceless labialized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [hʷ] \
       \is the representation of the \
       \voiceless labialized glottal fricative pulmonic egressive consonant" $
      describeIPA "hʷ"
        `shouldBe`
        "voiceless labialized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̊ʷ] \
       \is the representation of the \
       \voiceless labialized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̊ʷ"
        `shouldBe`
        "voiceless labialized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̥ʷ] \
       \is the representation of the \
       \voiceless labialized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̥ʷ"
        `shouldBe`
        "voiceless labialized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̊ʷ] \
       \is the representation of the \
       \voiceless labialized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̊ʷ"
        `shouldBe`
        "voiceless labialized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̥ʷ] \
       \is the representation of the \
       \voiceless labialized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̥ʷ"
        `shouldBe`
        "voiceless labialized glottal fricative pulmonic egressive consonant"
  describe "voiceless palatalized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [hʲ] \
       \is the representation of the \
       \voiceless palatalized glottal fricative pulmonic egressive consonant" $
      describeIPA "hʲ"
        `shouldBe`
        "voiceless palatalized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̊ʲ] \
       \is the representation of the \
       \voiceless palatalized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̊ʲ"
        `shouldBe`
        "voiceless palatalized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̥ʲ] \
       \is the representation of the \
       \voiceless palatalized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̥ʲ"
        `shouldBe`
        "voiceless palatalized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̊ʲ"
        `shouldBe`
        "voiceless palatalized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̥ʲ"
        `shouldBe`
        "voiceless palatalized glottal fricative pulmonic egressive consonant"
  describe "voiceless velarized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [hˠ] \
       \is the representation of the \
       \voiceless velarized glottal fricative pulmonic egressive consonant" $
      describeIPA "hˠ"
        `shouldBe`
        "voiceless velarized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̊ˠ] \
       \is the representation of the \
       \voiceless velarized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̊ˠ"
        `shouldBe`
        "voiceless velarized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̥ˠ] \
       \is the representation of the \
       \voiceless velarized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̥ˠ"
        `shouldBe`
        "voiceless velarized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̊ˠ] \
       \is the representation of the \
       \voiceless velarized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̊ˠ"
        `shouldBe`
        "voiceless velarized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̥ˠ] \
       \is the representation of the \
       \voiceless velarized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̥ˠ"
        `shouldBe`
        "voiceless velarized glottal fricative pulmonic egressive consonant"
  describe "voiceless pharyngealized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [hˤ] \
       \is the representation of the \
       \voiceless pharyngealized glottal fricative pulmonic egressive consonant" $
      describeIPA "hˤ"
        `shouldBe`
        "voiceless pharyngealized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̊ˤ"
        `shouldBe`
        "voiceless pharyngealized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̥ˤ"
        `shouldBe`
        "voiceless pharyngealized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized glottal fricative pulmonic egressive consonant"
  describe "voiceless aspirated glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [hʰ] \
       \is the representation of the \
       \voiceless aspirated glottal fricative pulmonic egressive consonant" $
      describeIPA "hʰ"
        `shouldBe`
        "voiceless aspirated glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̥ʰ"
        `shouldBe`
        "voiceless aspirated glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̊ʰ"
        `shouldBe`
        "voiceless aspirated glottal fricative pulmonic egressive consonant"
  describe "voiceless aspirated labialized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [hʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized glottal fricative pulmonic egressive consonant" $
      describeIPA "hʰʷ"
        `shouldBe`
        "voiceless aspirated labialized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized glottal fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatalized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [hʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized glottal fricative pulmonic egressive consonant" $
      describeIPA "hʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized glottal fricative pulmonic egressive consonant"
  describe "voiceless aspirated velarized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [hʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized glottal fricative pulmonic egressive consonant" $
      describeIPA "hʰˠ"
        `shouldBe`
        "voiceless aspirated velarized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized glottal fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [hʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
      describeIPA "hʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized glottal fricative pulmonic egressive consonant"
  describe "voiced glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɦ] \
       \is the representation of the \
       \voiced glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ"
        `shouldBe`
        "voiced glottal fricative pulmonic egressive consonant"
    it "should be that: [h̬] \
       \is the representation of the \
       \voiced glottal fricative pulmonic egressive consonant" $
      describeIPA "h̬"
        `shouldBe`
        "voiced glottal fricative pulmonic egressive consonant"
    it "should be that: [h̬] \
       \is the representation of the \
       \voiced glottal fricative pulmonic egressive consonant" $
      describeIPA "h̬"
        `shouldBe`
        "voiced glottal fricative pulmonic egressive consonant"
  describe "voiced labialized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɦʷ] \
       \is the representation of the \
       \voiced labialized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦʷ"
        `shouldBe`
        "voiced labialized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̬ʷ] \
       \is the representation of the \
       \voiced labialized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̬ʷ"
        `shouldBe`
        "voiced labialized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̬ʷ] \
       \is the representation of the \
       \voiced labialized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̬ʷ"
        `shouldBe`
        "voiced labialized glottal fricative pulmonic egressive consonant"
  describe "voiced palatalized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɦʲ] \
       \is the representation of the \
       \voiced palatalized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦʲ"
        `shouldBe`
        "voiced palatalized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̬ʲ] \
       \is the representation of the \
       \voiced palatalized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̬ʲ"
        `shouldBe`
        "voiced palatalized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̬ʲ] \
       \is the representation of the \
       \voiced palatalized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̬ʲ"
        `shouldBe`
        "voiced palatalized glottal fricative pulmonic egressive consonant"
  describe "voiced velarized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɦˠ] \
       \is the representation of the \
       \voiced velarized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦˠ"
        `shouldBe`
        "voiced velarized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̬ˠ] \
       \is the representation of the \
       \voiced velarized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̬ˠ"
        `shouldBe`
        "voiced velarized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̬ˠ] \
       \is the representation of the \
       \voiced velarized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̬ˠ"
        `shouldBe`
        "voiced velarized glottal fricative pulmonic egressive consonant"
  describe "voiced pharyngealized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɦˤ] \
       \is the representation of the \
       \voiced pharyngealized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦˤ"
        `shouldBe`
        "voiced pharyngealized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̬ˤ"
        `shouldBe`
        "voiced pharyngealized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̬ˤ"
        `shouldBe`
        "voiced pharyngealized glottal fricative pulmonic egressive consonant"
  describe "voiced aspirated glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɦʰ] \
       \is the representation of the \
       \voiced aspirated glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦʰ"
        `shouldBe`
        "voiced aspirated glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̬ʰ] \
       \is the representation of the \
       \voiced aspirated glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̬ʰ"
        `shouldBe`
        "voiced aspirated glottal fricative pulmonic egressive consonant"
    it "should be that: [h̬ʰ] \
       \is the representation of the \
       \voiced aspirated glottal fricative pulmonic egressive consonant" $
      describeIPA "h̬ʰ"
        `shouldBe`
        "voiced aspirated glottal fricative pulmonic egressive consonant"
  describe "voiced aspirated labialized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɦʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦʰʷ"
        `shouldBe`
        "voiced aspirated labialized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized glottal fricative pulmonic egressive consonant"
  describe "voiced aspirated palatalized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɦʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦʰʲ"
        `shouldBe`
        "voiced aspirated palatalized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized glottal fricative pulmonic egressive consonant"
  describe "voiced aspirated velarized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɦʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦʰˠ"
        `shouldBe`
        "voiced aspirated velarized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized glottal fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɦʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized glottal fricative pulmonic egressive consonant"
    it "should be that: [ɦ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
      describeIPA "ɦ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized glottal fricative pulmonic egressive consonant"
    it "should be that: [h̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
      describeIPA "h̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized glottal fricative pulmonic egressive consonant"
  describe "voiceless alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɬ] \
       \is the representation of the \
       \voiceless alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ"
        `shouldBe`
        "voiceless alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̊] \
       \is the representation of the \
       \voiceless alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̊"
        `shouldBe`
        "voiceless alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̥] \
       \is the representation of the \
       \voiceless alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̥"
        `shouldBe`
        "voiceless alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̊] \
       \is the representation of the \
       \voiceless alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̊"
        `shouldBe`
        "voiceless alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̥] \
       \is the representation of the \
       \voiceless alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̥"
        `shouldBe`
        "voiceless alveolar lateral fricative pulmonic egressive consonant"
  describe "voiceless labialized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɬʷ] \
       \is the representation of the \
       \voiceless labialized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬʷ"
        `shouldBe`
        "voiceless labialized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̊ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̊ʷ"
        `shouldBe`
        "voiceless labialized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̥ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̥ʷ"
        `shouldBe`
        "voiceless labialized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̊ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̊ʷ"
        `shouldBe`
        "voiceless labialized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̥ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̥ʷ"
        `shouldBe`
        "voiceless labialized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiceless palatalized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɬʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬʲ"
        `shouldBe`
        "voiceless palatalized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̊ʲ"
        `shouldBe`
        "voiceless palatalized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̥ʲ"
        `shouldBe`
        "voiceless palatalized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̊ʲ"
        `shouldBe`
        "voiceless palatalized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̥ʲ"
        `shouldBe`
        "voiceless palatalized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiceless velarized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɬˠ] \
       \is the representation of the \
       \voiceless velarized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬˠ"
        `shouldBe`
        "voiceless velarized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̊ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̊ˠ"
        `shouldBe`
        "voiceless velarized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̥ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̥ˠ"
        `shouldBe`
        "voiceless velarized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̊ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̊ˠ"
        `shouldBe`
        "voiceless velarized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̥ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̥ˠ"
        `shouldBe`
        "voiceless velarized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɬˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiceless aspirated alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɬʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬʰ"
        `shouldBe`
        "voiceless aspirated alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̥ʰ"
        `shouldBe`
        "voiceless aspirated alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̊ʰ"
        `shouldBe`
        "voiceless aspirated alveolar lateral fricative pulmonic egressive consonant"
  describe "voiceless aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɬʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɬʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiceless aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɬʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɬʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiced alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɮ] \
       \is the representation of the \
       \voiced alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ"
        `shouldBe`
        "voiced alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̬] \
       \is the representation of the \
       \voiced alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̬"
        `shouldBe`
        "voiced alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̬] \
       \is the representation of the \
       \voiced alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̬"
        `shouldBe`
        "voiced alveolar lateral fricative pulmonic egressive consonant"
  describe "voiced labialized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɮʷ] \
       \is the representation of the \
       \voiced labialized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮʷ"
        `shouldBe`
        "voiced labialized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̬ʷ] \
       \is the representation of the \
       \voiced labialized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̬ʷ"
        `shouldBe`
        "voiced labialized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̬ʷ] \
       \is the representation of the \
       \voiced labialized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̬ʷ"
        `shouldBe`
        "voiced labialized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiced palatalized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɮʲ] \
       \is the representation of the \
       \voiced palatalized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮʲ"
        `shouldBe`
        "voiced palatalized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̬ʲ] \
       \is the representation of the \
       \voiced palatalized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̬ʲ"
        `shouldBe`
        "voiced palatalized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̬ʲ] \
       \is the representation of the \
       \voiced palatalized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̬ʲ"
        `shouldBe`
        "voiced palatalized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiced velarized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɮˠ] \
       \is the representation of the \
       \voiced velarized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮˠ"
        `shouldBe`
        "voiced velarized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̬ˠ] \
       \is the representation of the \
       \voiced velarized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̬ˠ"
        `shouldBe`
        "voiced velarized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̬ˠ] \
       \is the representation of the \
       \voiced velarized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̬ˠ"
        `shouldBe`
        "voiced velarized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiced pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɮˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮˤ"
        `shouldBe`
        "voiced pharyngealized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̬ˤ"
        `shouldBe`
        "voiced pharyngealized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̬ˤ"
        `shouldBe`
        "voiced pharyngealized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiced aspirated alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɮʰ] \
       \is the representation of the \
       \voiced aspirated alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮʰ"
        `shouldBe`
        "voiced aspirated alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̬ʰ] \
       \is the representation of the \
       \voiced aspirated alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̬ʰ"
        `shouldBe`
        "voiced aspirated alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̬ʰ] \
       \is the representation of the \
       \voiced aspirated alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̬ʰ"
        `shouldBe`
        "voiced aspirated alveolar lateral fricative pulmonic egressive consonant"
  describe "voiced aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɮʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiced aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɮʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiced aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɮʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɮʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɮ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɮ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant"
    it "should be that: [ɬ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
      describeIPA "ɬ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant"
  describe "voiceless bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [m̊] \
       \is the representation of the \
       \voiceless bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̊"
        `shouldBe`
        "voiceless bilabial nasal pulmonic egressive consonant"
    it "should be that: [m̥] \
       \is the representation of the \
       \voiceless bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̥"
        `shouldBe`
        "voiceless bilabial nasal pulmonic egressive consonant"
  describe "voiceless labialized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [m̊ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̊ʷ"
        `shouldBe`
        "voiceless labialized bilabial nasal pulmonic egressive consonant"
    it "should be that: [m̥ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̥ʷ"
        `shouldBe`
        "voiceless labialized bilabial nasal pulmonic egressive consonant"
  describe "voiceless palatalized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [m̊ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̊ʲ"
        `shouldBe`
        "voiceless palatalized bilabial nasal pulmonic egressive consonant"
    it "should be that: [m̥ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̥ʲ"
        `shouldBe`
        "voiceless palatalized bilabial nasal pulmonic egressive consonant"
  describe "voiceless velarized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [m̊ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̊ˠ"
        `shouldBe`
        "voiceless velarized bilabial nasal pulmonic egressive consonant"
    it "should be that: [m̥ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̥ˠ"
        `shouldBe`
        "voiceless velarized bilabial nasal pulmonic egressive consonant"
  describe "voiceless pharyngealized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [m̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̊ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial nasal pulmonic egressive consonant"
    it "should be that: [m̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̥ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial nasal pulmonic egressive consonant"
  describe "voiceless aspirated bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [m̥ʰ] \
       \is the representation of the \
       \voiceless aspirated bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̥ʰ"
        `shouldBe`
        "voiceless aspirated bilabial nasal pulmonic egressive consonant"
    it "should be that: [m̊ʰ] \
       \is the representation of the \
       \voiceless aspirated bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̊ʰ"
        `shouldBe`
        "voiceless aspirated bilabial nasal pulmonic egressive consonant"
  describe "voiceless aspirated labialized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [m̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized bilabial nasal pulmonic egressive consonant"
    it "should be that: [m̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized bilabial nasal pulmonic egressive consonant"
  describe "voiceless aspirated palatalized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [m̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized bilabial nasal pulmonic egressive consonant"
    it "should be that: [m̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized bilabial nasal pulmonic egressive consonant"
  describe "voiceless aspirated velarized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [m̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized bilabial nasal pulmonic egressive consonant"
    it "should be that: [m̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized bilabial nasal pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [m̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized bilabial nasal pulmonic egressive consonant"
    it "should be that: [m̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized bilabial nasal pulmonic egressive consonant"
  describe "voiced bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [m] \
       \is the representation of the \
       \voiced bilabial nasal pulmonic egressive consonant" $
      describeIPA "m"
        `shouldBe`
        "voiced bilabial nasal pulmonic egressive consonant"
  describe "voiced labialized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [mʷ] \
       \is the representation of the \
       \voiced labialized bilabial nasal pulmonic egressive consonant" $
      describeIPA "mʷ"
        `shouldBe`
        "voiced labialized bilabial nasal pulmonic egressive consonant"
  describe "voiced palatalized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [mʲ] \
       \is the representation of the \
       \voiced palatalized bilabial nasal pulmonic egressive consonant" $
      describeIPA "mʲ"
        `shouldBe`
        "voiced palatalized bilabial nasal pulmonic egressive consonant"
  describe "voiced velarized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [mˠ] \
       \is the representation of the \
       \voiced velarized bilabial nasal pulmonic egressive consonant" $
      describeIPA "mˠ"
        `shouldBe`
        "voiced velarized bilabial nasal pulmonic egressive consonant"
  describe "voiced pharyngealized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [mˤ] \
       \is the representation of the \
       \voiced pharyngealized bilabial nasal pulmonic egressive consonant" $
      describeIPA "mˤ"
        `shouldBe`
        "voiced pharyngealized bilabial nasal pulmonic egressive consonant"
  describe "voiced aspirated bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [mʰ] \
       \is the representation of the \
       \voiced aspirated bilabial nasal pulmonic egressive consonant" $
      describeIPA "mʰ"
        `shouldBe`
        "voiced aspirated bilabial nasal pulmonic egressive consonant"
    it "should be that: [m̬ʰ] \
       \is the representation of the \
       \voiced aspirated bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̬ʰ"
        `shouldBe`
        "voiced aspirated bilabial nasal pulmonic egressive consonant"
  describe "voiced aspirated labialized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [mʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized bilabial nasal pulmonic egressive consonant" $
      describeIPA "mʰʷ"
        `shouldBe`
        "voiced aspirated labialized bilabial nasal pulmonic egressive consonant"
    it "should be that: [m̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized bilabial nasal pulmonic egressive consonant"
  describe "voiced aspirated palatalized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [mʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized bilabial nasal pulmonic egressive consonant" $
      describeIPA "mʰʲ"
        `shouldBe`
        "voiced aspirated palatalized bilabial nasal pulmonic egressive consonant"
    it "should be that: [m̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized bilabial nasal pulmonic egressive consonant"
  describe "voiced aspirated velarized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [mʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized bilabial nasal pulmonic egressive consonant" $
      describeIPA "mʰˠ"
        `shouldBe`
        "voiced aspirated velarized bilabial nasal pulmonic egressive consonant"
    it "should be that: [m̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized bilabial nasal pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized bilabial nasal pulmonic egressive consonant" $
    do
    it "should be that: [mʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized bilabial nasal pulmonic egressive consonant" $
      describeIPA "mʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized bilabial nasal pulmonic egressive consonant"
    it "should be that: [m̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized bilabial nasal pulmonic egressive consonant" $
      describeIPA "m̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized bilabial nasal pulmonic egressive consonant"
  describe "voiceless alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [n̊] \
       \is the representation of the \
       \voiceless alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̊"
        `shouldBe`
        "voiceless alveolar nasal pulmonic egressive consonant"
    it "should be that: [n̥] \
       \is the representation of the \
       \voiceless alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̥"
        `shouldBe`
        "voiceless alveolar nasal pulmonic egressive consonant"
  describe "voiceless labialized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [n̊ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̊ʷ"
        `shouldBe`
        "voiceless labialized alveolar nasal pulmonic egressive consonant"
    it "should be that: [n̥ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̥ʷ"
        `shouldBe`
        "voiceless labialized alveolar nasal pulmonic egressive consonant"
  describe "voiceless palatalized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [n̊ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̊ʲ"
        `shouldBe`
        "voiceless palatalized alveolar nasal pulmonic egressive consonant"
    it "should be that: [n̥ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̥ʲ"
        `shouldBe`
        "voiceless palatalized alveolar nasal pulmonic egressive consonant"
  describe "voiceless velarized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [n̊ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̊ˠ"
        `shouldBe`
        "voiceless velarized alveolar nasal pulmonic egressive consonant"
    it "should be that: [n̥ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̥ˠ"
        `shouldBe`
        "voiceless velarized alveolar nasal pulmonic egressive consonant"
  describe "voiceless pharyngealized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [n̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̊ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar nasal pulmonic egressive consonant"
    it "should be that: [n̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̥ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar nasal pulmonic egressive consonant"
  describe "voiceless aspirated alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [n̥ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̥ʰ"
        `shouldBe`
        "voiceless aspirated alveolar nasal pulmonic egressive consonant"
    it "should be that: [n̊ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̊ʰ"
        `shouldBe`
        "voiceless aspirated alveolar nasal pulmonic egressive consonant"
  describe "voiceless aspirated labialized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [n̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar nasal pulmonic egressive consonant"
    it "should be that: [n̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar nasal pulmonic egressive consonant"
  describe "voiceless aspirated palatalized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [n̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar nasal pulmonic egressive consonant"
    it "should be that: [n̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar nasal pulmonic egressive consonant"
  describe "voiceless aspirated velarized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [n̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar nasal pulmonic egressive consonant"
    it "should be that: [n̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar nasal pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [n̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar nasal pulmonic egressive consonant"
    it "should be that: [n̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar nasal pulmonic egressive consonant"
  describe "voiced alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [n] \
       \is the representation of the \
       \voiced alveolar nasal pulmonic egressive consonant" $
      describeIPA "n"
        `shouldBe`
        "voiced alveolar nasal pulmonic egressive consonant"
  describe "voiced labialized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [nʷ] \
       \is the representation of the \
       \voiced labialized alveolar nasal pulmonic egressive consonant" $
      describeIPA "nʷ"
        `shouldBe`
        "voiced labialized alveolar nasal pulmonic egressive consonant"
  describe "voiced palatalized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [nʲ] \
       \is the representation of the \
       \voiced palatalized alveolar nasal pulmonic egressive consonant" $
      describeIPA "nʲ"
        `shouldBe`
        "voiced palatalized alveolar nasal pulmonic egressive consonant"
  describe "voiced velarized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [nˠ] \
       \is the representation of the \
       \voiced velarized alveolar nasal pulmonic egressive consonant" $
      describeIPA "nˠ"
        `shouldBe`
        "voiced velarized alveolar nasal pulmonic egressive consonant"
  describe "voiced pharyngealized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [nˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar nasal pulmonic egressive consonant" $
      describeIPA "nˤ"
        `shouldBe`
        "voiced pharyngealized alveolar nasal pulmonic egressive consonant"
  describe "voiced aspirated alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [nʰ] \
       \is the representation of the \
       \voiced aspirated alveolar nasal pulmonic egressive consonant" $
      describeIPA "nʰ"
        `shouldBe`
        "voiced aspirated alveolar nasal pulmonic egressive consonant"
    it "should be that: [n̬ʰ] \
       \is the representation of the \
       \voiced aspirated alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̬ʰ"
        `shouldBe`
        "voiced aspirated alveolar nasal pulmonic egressive consonant"
  describe "voiced aspirated labialized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [nʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar nasal pulmonic egressive consonant" $
      describeIPA "nʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar nasal pulmonic egressive consonant"
    it "should be that: [n̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar nasal pulmonic egressive consonant"
  describe "voiced aspirated palatalized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [nʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar nasal pulmonic egressive consonant" $
      describeIPA "nʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar nasal pulmonic egressive consonant"
    it "should be that: [n̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar nasal pulmonic egressive consonant"
  describe "voiced aspirated velarized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [nʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar nasal pulmonic egressive consonant" $
      describeIPA "nʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar nasal pulmonic egressive consonant"
    it "should be that: [n̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar nasal pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized alveolar nasal pulmonic egressive consonant" $
    do
    it "should be that: [nʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar nasal pulmonic egressive consonant" $
      describeIPA "nʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar nasal pulmonic egressive consonant"
    it "should be that: [n̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar nasal pulmonic egressive consonant" $
      describeIPA "n̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar nasal pulmonic egressive consonant"
  describe "voiceless palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲ̊] \
       \is the representation of the \
       \voiceless palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̊"
        `shouldBe`
        "voiceless palatal nasal pulmonic egressive consonant"
    it "should be that: [ɲ̥] \
       \is the representation of the \
       \voiceless palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̥"
        `shouldBe`
        "voiceless palatal nasal pulmonic egressive consonant"
  describe "voiceless labialized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲ̊ʷ] \
       \is the representation of the \
       \voiceless labialized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̊ʷ"
        `shouldBe`
        "voiceless labialized palatal nasal pulmonic egressive consonant"
    it "should be that: [ɲ̥ʷ] \
       \is the representation of the \
       \voiceless labialized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̥ʷ"
        `shouldBe`
        "voiceless labialized palatal nasal pulmonic egressive consonant"
  describe "voiceless palatalized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̊ʲ"
        `shouldBe`
        "voiceless palatalized palatal nasal pulmonic egressive consonant"
    it "should be that: [ɲ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̥ʲ"
        `shouldBe`
        "voiceless palatalized palatal nasal pulmonic egressive consonant"
  describe "voiceless velarized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲ̊ˠ] \
       \is the representation of the \
       \voiceless velarized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̊ˠ"
        `shouldBe`
        "voiceless velarized palatal nasal pulmonic egressive consonant"
    it "should be that: [ɲ̥ˠ] \
       \is the representation of the \
       \voiceless velarized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̥ˠ"
        `shouldBe`
        "voiceless velarized palatal nasal pulmonic egressive consonant"
  describe "voiceless pharyngealized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal nasal pulmonic egressive consonant"
    it "should be that: [ɲ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal nasal pulmonic egressive consonant"
  describe "voiceless aspirated palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̥ʰ"
        `shouldBe`
        "voiceless aspirated palatal nasal pulmonic egressive consonant"
    it "should be that: [ɲ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̊ʰ"
        `shouldBe`
        "voiceless aspirated palatal nasal pulmonic egressive consonant"
  describe "voiceless aspirated labialized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized palatal nasal pulmonic egressive consonant"
    it "should be that: [ɲ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized palatal nasal pulmonic egressive consonant"
  describe "voiceless aspirated palatalized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized palatal nasal pulmonic egressive consonant"
    it "should be that: [ɲ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized palatal nasal pulmonic egressive consonant"
  describe "voiceless aspirated velarized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized palatal nasal pulmonic egressive consonant"
    it "should be that: [ɲ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized palatal nasal pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized palatal nasal pulmonic egressive consonant"
    it "should be that: [ɲ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized palatal nasal pulmonic egressive consonant"
  describe "voiced palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲ] \
       \is the representation of the \
       \voiced palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ"
        `shouldBe`
        "voiced palatal nasal pulmonic egressive consonant"
  describe "voiced labialized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲʷ] \
       \is the representation of the \
       \voiced labialized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲʷ"
        `shouldBe`
        "voiced labialized palatal nasal pulmonic egressive consonant"
  describe "voiced palatalized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲʲ] \
       \is the representation of the \
       \voiced palatalized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲʲ"
        `shouldBe`
        "voiced palatalized palatal nasal pulmonic egressive consonant"
  describe "voiced velarized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲˠ] \
       \is the representation of the \
       \voiced velarized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲˠ"
        `shouldBe`
        "voiced velarized palatal nasal pulmonic egressive consonant"
  describe "voiced pharyngealized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲˤ] \
       \is the representation of the \
       \voiced pharyngealized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲˤ"
        `shouldBe`
        "voiced pharyngealized palatal nasal pulmonic egressive consonant"
  describe "voiced aspirated palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲʰ] \
       \is the representation of the \
       \voiced aspirated palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲʰ"
        `shouldBe`
        "voiced aspirated palatal nasal pulmonic egressive consonant"
    it "should be that: [ɲ̬ʰ] \
       \is the representation of the \
       \voiced aspirated palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̬ʰ"
        `shouldBe`
        "voiced aspirated palatal nasal pulmonic egressive consonant"
  describe "voiced aspirated labialized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲʰʷ"
        `shouldBe`
        "voiced aspirated labialized palatal nasal pulmonic egressive consonant"
    it "should be that: [ɲ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized palatal nasal pulmonic egressive consonant"
  describe "voiced aspirated palatalized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲʰʲ"
        `shouldBe`
        "voiced aspirated palatalized palatal nasal pulmonic egressive consonant"
    it "should be that: [ɲ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized palatal nasal pulmonic egressive consonant"
  describe "voiced aspirated velarized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲʰˠ"
        `shouldBe`
        "voiced aspirated velarized palatal nasal pulmonic egressive consonant"
    it "should be that: [ɲ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized palatal nasal pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized palatal nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɲʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized palatal nasal pulmonic egressive consonant"
    it "should be that: [ɲ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized palatal nasal pulmonic egressive consonant" $
      describeIPA "ɲ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized palatal nasal pulmonic egressive consonant"
  describe "voiceless retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳ̊] \
       \is the representation of the \
       \voiceless retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̊"
        `shouldBe`
        "voiceless retroflex nasal pulmonic egressive consonant"
    it "should be that: [ɳ̥] \
       \is the representation of the \
       \voiceless retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̥"
        `shouldBe`
        "voiceless retroflex nasal pulmonic egressive consonant"
  describe "voiceless labialized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳ̊ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̊ʷ"
        `shouldBe`
        "voiceless labialized retroflex nasal pulmonic egressive consonant"
    it "should be that: [ɳ̥ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̥ʷ"
        `shouldBe`
        "voiceless labialized retroflex nasal pulmonic egressive consonant"
  describe "voiceless palatalized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̊ʲ"
        `shouldBe`
        "voiceless palatalized retroflex nasal pulmonic egressive consonant"
    it "should be that: [ɳ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̥ʲ"
        `shouldBe`
        "voiceless palatalized retroflex nasal pulmonic egressive consonant"
  describe "voiceless velarized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳ̊ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̊ˠ"
        `shouldBe`
        "voiceless velarized retroflex nasal pulmonic egressive consonant"
    it "should be that: [ɳ̥ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̥ˠ"
        `shouldBe`
        "voiceless velarized retroflex nasal pulmonic egressive consonant"
  describe "voiceless pharyngealized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex nasal pulmonic egressive consonant"
    it "should be that: [ɳ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex nasal pulmonic egressive consonant"
  describe "voiceless aspirated retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̥ʰ"
        `shouldBe`
        "voiceless aspirated retroflex nasal pulmonic egressive consonant"
    it "should be that: [ɳ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̊ʰ"
        `shouldBe`
        "voiceless aspirated retroflex nasal pulmonic egressive consonant"
  describe "voiceless aspirated labialized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex nasal pulmonic egressive consonant"
    it "should be that: [ɳ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex nasal pulmonic egressive consonant"
  describe "voiceless aspirated palatalized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex nasal pulmonic egressive consonant"
    it "should be that: [ɳ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex nasal pulmonic egressive consonant"
  describe "voiceless aspirated velarized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex nasal pulmonic egressive consonant"
    it "should be that: [ɳ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex nasal pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex nasal pulmonic egressive consonant"
    it "should be that: [ɳ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex nasal pulmonic egressive consonant"
  describe "voiced retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳ] \
       \is the representation of the \
       \voiced retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ"
        `shouldBe`
        "voiced retroflex nasal pulmonic egressive consonant"
  describe "voiced labialized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳʷ] \
       \is the representation of the \
       \voiced labialized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳʷ"
        `shouldBe`
        "voiced labialized retroflex nasal pulmonic egressive consonant"
  describe "voiced palatalized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳʲ] \
       \is the representation of the \
       \voiced palatalized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳʲ"
        `shouldBe`
        "voiced palatalized retroflex nasal pulmonic egressive consonant"
  describe "voiced velarized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳˠ] \
       \is the representation of the \
       \voiced velarized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳˠ"
        `shouldBe`
        "voiced velarized retroflex nasal pulmonic egressive consonant"
  describe "voiced pharyngealized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳˤ] \
       \is the representation of the \
       \voiced pharyngealized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳˤ"
        `shouldBe`
        "voiced pharyngealized retroflex nasal pulmonic egressive consonant"
  describe "voiced aspirated retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳʰ] \
       \is the representation of the \
       \voiced aspirated retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳʰ"
        `shouldBe`
        "voiced aspirated retroflex nasal pulmonic egressive consonant"
    it "should be that: [ɳ̬ʰ] \
       \is the representation of the \
       \voiced aspirated retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̬ʰ"
        `shouldBe`
        "voiced aspirated retroflex nasal pulmonic egressive consonant"
  describe "voiced aspirated labialized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex nasal pulmonic egressive consonant"
    it "should be that: [ɳ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex nasal pulmonic egressive consonant"
  describe "voiced aspirated palatalized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex nasal pulmonic egressive consonant"
    it "should be that: [ɳ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex nasal pulmonic egressive consonant"
  describe "voiced aspirated velarized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex nasal pulmonic egressive consonant"
    it "should be that: [ɳ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex nasal pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized retroflex nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɳʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex nasal pulmonic egressive consonant"
    it "should be that: [ɳ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex nasal pulmonic egressive consonant" $
      describeIPA "ɳ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex nasal pulmonic egressive consonant"
  describe "voiceless velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋ̊] \
       \is the representation of the \
       \voiceless velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̊"
        `shouldBe`
        "voiceless velar nasal pulmonic egressive consonant"
    it "should be that: [ŋ̥] \
       \is the representation of the \
       \voiceless velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̥"
        `shouldBe`
        "voiceless velar nasal pulmonic egressive consonant"
  describe "voiceless labialized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋ̊ʷ] \
       \is the representation of the \
       \voiceless labialized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̊ʷ"
        `shouldBe`
        "voiceless labialized velar nasal pulmonic egressive consonant"
    it "should be that: [ŋ̥ʷ] \
       \is the representation of the \
       \voiceless labialized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̥ʷ"
        `shouldBe`
        "voiceless labialized velar nasal pulmonic egressive consonant"
  describe "voiceless palatalized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̊ʲ"
        `shouldBe`
        "voiceless palatalized velar nasal pulmonic egressive consonant"
    it "should be that: [ŋ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̥ʲ"
        `shouldBe`
        "voiceless palatalized velar nasal pulmonic egressive consonant"
  describe "voiceless velarized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋ̊ˠ] \
       \is the representation of the \
       \voiceless velarized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̊ˠ"
        `shouldBe`
        "voiceless velarized velar nasal pulmonic egressive consonant"
    it "should be that: [ŋ̥ˠ] \
       \is the representation of the \
       \voiceless velarized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̥ˠ"
        `shouldBe`
        "voiceless velarized velar nasal pulmonic egressive consonant"
  describe "voiceless pharyngealized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized velar nasal pulmonic egressive consonant"
    it "should be that: [ŋ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized velar nasal pulmonic egressive consonant"
  describe "voiceless aspirated velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̥ʰ"
        `shouldBe`
        "voiceless aspirated velar nasal pulmonic egressive consonant"
    it "should be that: [ŋ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̊ʰ"
        `shouldBe`
        "voiceless aspirated velar nasal pulmonic egressive consonant"
  describe "voiceless aspirated labialized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized velar nasal pulmonic egressive consonant"
    it "should be that: [ŋ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized velar nasal pulmonic egressive consonant"
  describe "voiceless aspirated palatalized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized velar nasal pulmonic egressive consonant"
    it "should be that: [ŋ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized velar nasal pulmonic egressive consonant"
  describe "voiceless aspirated velarized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized velar nasal pulmonic egressive consonant"
    it "should be that: [ŋ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized velar nasal pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized velar nasal pulmonic egressive consonant"
    it "should be that: [ŋ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized velar nasal pulmonic egressive consonant"
  describe "voiced velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋ] \
       \is the representation of the \
       \voiced velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ"
        `shouldBe`
        "voiced velar nasal pulmonic egressive consonant"
  describe "voiced labialized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋʷ] \
       \is the representation of the \
       \voiced labialized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋʷ"
        `shouldBe`
        "voiced labialized velar nasal pulmonic egressive consonant"
  describe "voiced palatalized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋʲ] \
       \is the representation of the \
       \voiced palatalized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋʲ"
        `shouldBe`
        "voiced palatalized velar nasal pulmonic egressive consonant"
  describe "voiced velarized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋˠ] \
       \is the representation of the \
       \voiced velarized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋˠ"
        `shouldBe`
        "voiced velarized velar nasal pulmonic egressive consonant"
  describe "voiced pharyngealized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋˤ] \
       \is the representation of the \
       \voiced pharyngealized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋˤ"
        `shouldBe`
        "voiced pharyngealized velar nasal pulmonic egressive consonant"
  describe "voiced aspirated velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋʰ] \
       \is the representation of the \
       \voiced aspirated velar nasal pulmonic egressive consonant" $
      describeIPA "ŋʰ"
        `shouldBe`
        "voiced aspirated velar nasal pulmonic egressive consonant"
    it "should be that: [ŋ̬ʰ] \
       \is the representation of the \
       \voiced aspirated velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̬ʰ"
        `shouldBe`
        "voiced aspirated velar nasal pulmonic egressive consonant"
  describe "voiced aspirated labialized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋʰʷ"
        `shouldBe`
        "voiced aspirated labialized velar nasal pulmonic egressive consonant"
    it "should be that: [ŋ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized velar nasal pulmonic egressive consonant"
  describe "voiced aspirated palatalized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋʰʲ"
        `shouldBe`
        "voiced aspirated palatalized velar nasal pulmonic egressive consonant"
    it "should be that: [ŋ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized velar nasal pulmonic egressive consonant"
  describe "voiced aspirated velarized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋʰˠ"
        `shouldBe`
        "voiced aspirated velarized velar nasal pulmonic egressive consonant"
    it "should be that: [ŋ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized velar nasal pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized velar nasal pulmonic egressive consonant" $
    do
    it "should be that: [ŋʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized velar nasal pulmonic egressive consonant"
    it "should be that: [ŋ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized velar nasal pulmonic egressive consonant" $
      describeIPA "ŋ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized velar nasal pulmonic egressive consonant"
  describe "voiceless uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴ̊] \
       \is the representation of the \
       \voiceless uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̊"
        `shouldBe`
        "voiceless uvular nasal pulmonic egressive consonant"
    it "should be that: [ɴ̥] \
       \is the representation of the \
       \voiceless uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̥"
        `shouldBe`
        "voiceless uvular nasal pulmonic egressive consonant"
  describe "voiceless labialized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴ̊ʷ] \
       \is the representation of the \
       \voiceless labialized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̊ʷ"
        `shouldBe`
        "voiceless labialized uvular nasal pulmonic egressive consonant"
    it "should be that: [ɴ̥ʷ] \
       \is the representation of the \
       \voiceless labialized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̥ʷ"
        `shouldBe`
        "voiceless labialized uvular nasal pulmonic egressive consonant"
  describe "voiceless palatalized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̊ʲ"
        `shouldBe`
        "voiceless palatalized uvular nasal pulmonic egressive consonant"
    it "should be that: [ɴ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̥ʲ"
        `shouldBe`
        "voiceless palatalized uvular nasal pulmonic egressive consonant"
  describe "voiceless velarized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴ̊ˠ] \
       \is the representation of the \
       \voiceless velarized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̊ˠ"
        `shouldBe`
        "voiceless velarized uvular nasal pulmonic egressive consonant"
    it "should be that: [ɴ̥ˠ] \
       \is the representation of the \
       \voiceless velarized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̥ˠ"
        `shouldBe`
        "voiceless velarized uvular nasal pulmonic egressive consonant"
  describe "voiceless pharyngealized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular nasal pulmonic egressive consonant"
    it "should be that: [ɴ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular nasal pulmonic egressive consonant"
  describe "voiceless aspirated uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̥ʰ"
        `shouldBe`
        "voiceless aspirated uvular nasal pulmonic egressive consonant"
    it "should be that: [ɴ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̊ʰ"
        `shouldBe`
        "voiceless aspirated uvular nasal pulmonic egressive consonant"
  describe "voiceless aspirated labialized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized uvular nasal pulmonic egressive consonant"
    it "should be that: [ɴ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized uvular nasal pulmonic egressive consonant"
  describe "voiceless aspirated palatalized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized uvular nasal pulmonic egressive consonant"
    it "should be that: [ɴ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized uvular nasal pulmonic egressive consonant"
  describe "voiceless aspirated velarized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized uvular nasal pulmonic egressive consonant"
    it "should be that: [ɴ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized uvular nasal pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized uvular nasal pulmonic egressive consonant"
    it "should be that: [ɴ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized uvular nasal pulmonic egressive consonant"
  describe "voiced uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴ] \
       \is the representation of the \
       \voiced uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ"
        `shouldBe`
        "voiced uvular nasal pulmonic egressive consonant"
  describe "voiced labialized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴʷ] \
       \is the representation of the \
       \voiced labialized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴʷ"
        `shouldBe`
        "voiced labialized uvular nasal pulmonic egressive consonant"
  describe "voiced palatalized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴʲ] \
       \is the representation of the \
       \voiced palatalized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴʲ"
        `shouldBe`
        "voiced palatalized uvular nasal pulmonic egressive consonant"
  describe "voiced velarized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴˠ] \
       \is the representation of the \
       \voiced velarized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴˠ"
        `shouldBe`
        "voiced velarized uvular nasal pulmonic egressive consonant"
  describe "voiced pharyngealized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴˤ] \
       \is the representation of the \
       \voiced pharyngealized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴˤ"
        `shouldBe`
        "voiced pharyngealized uvular nasal pulmonic egressive consonant"
  describe "voiced aspirated uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴʰ] \
       \is the representation of the \
       \voiced aspirated uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴʰ"
        `shouldBe`
        "voiced aspirated uvular nasal pulmonic egressive consonant"
    it "should be that: [ɴ̬ʰ] \
       \is the representation of the \
       \voiced aspirated uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̬ʰ"
        `shouldBe`
        "voiced aspirated uvular nasal pulmonic egressive consonant"
  describe "voiced aspirated labialized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴʰʷ"
        `shouldBe`
        "voiced aspirated labialized uvular nasal pulmonic egressive consonant"
    it "should be that: [ɴ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized uvular nasal pulmonic egressive consonant"
  describe "voiced aspirated palatalized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴʰʲ"
        `shouldBe`
        "voiced aspirated palatalized uvular nasal pulmonic egressive consonant"
    it "should be that: [ɴ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized uvular nasal pulmonic egressive consonant"
  describe "voiced aspirated velarized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴʰˠ"
        `shouldBe`
        "voiced aspirated velarized uvular nasal pulmonic egressive consonant"
    it "should be that: [ɴ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized uvular nasal pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized uvular nasal pulmonic egressive consonant" $
    do
    it "should be that: [ɴʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized uvular nasal pulmonic egressive consonant"
    it "should be that: [ɴ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized uvular nasal pulmonic egressive consonant" $
      describeIPA "ɴ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized uvular nasal pulmonic egressive consonant"
  describe "voiceless bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙ̊] \
       \is the representation of the \
       \voiceless bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̊"
        `shouldBe`
        "voiceless bilabial trill pulmonic egressive consonant"
    it "should be that: [ʙ̥] \
       \is the representation of the \
       \voiceless bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̥"
        `shouldBe`
        "voiceless bilabial trill pulmonic egressive consonant"
  describe "voiceless labialized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙ̊ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̊ʷ"
        `shouldBe`
        "voiceless labialized bilabial trill pulmonic egressive consonant"
    it "should be that: [ʙ̥ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̥ʷ"
        `shouldBe`
        "voiceless labialized bilabial trill pulmonic egressive consonant"
  describe "voiceless palatalized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̊ʲ"
        `shouldBe`
        "voiceless palatalized bilabial trill pulmonic egressive consonant"
    it "should be that: [ʙ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̥ʲ"
        `shouldBe`
        "voiceless palatalized bilabial trill pulmonic egressive consonant"
  describe "voiceless velarized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙ̊ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̊ˠ"
        `shouldBe`
        "voiceless velarized bilabial trill pulmonic egressive consonant"
    it "should be that: [ʙ̥ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̥ˠ"
        `shouldBe`
        "voiceless velarized bilabial trill pulmonic egressive consonant"
  describe "voiceless pharyngealized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial trill pulmonic egressive consonant"
    it "should be that: [ʙ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial trill pulmonic egressive consonant"
  describe "voiceless aspirated bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̥ʰ"
        `shouldBe`
        "voiceless aspirated bilabial trill pulmonic egressive consonant"
    it "should be that: [ʙ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̊ʰ"
        `shouldBe`
        "voiceless aspirated bilabial trill pulmonic egressive consonant"
  describe "voiceless aspirated labialized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized bilabial trill pulmonic egressive consonant"
    it "should be that: [ʙ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized bilabial trill pulmonic egressive consonant"
  describe "voiceless aspirated palatalized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized bilabial trill pulmonic egressive consonant"
    it "should be that: [ʙ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized bilabial trill pulmonic egressive consonant"
  describe "voiceless aspirated velarized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized bilabial trill pulmonic egressive consonant"
    it "should be that: [ʙ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized bilabial trill pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized bilabial trill pulmonic egressive consonant"
    it "should be that: [ʙ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized bilabial trill pulmonic egressive consonant"
  describe "voiced bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙ] \
       \is the representation of the \
       \voiced bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ"
        `shouldBe`
        "voiced bilabial trill pulmonic egressive consonant"
  describe "voiced labialized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙʷ] \
       \is the representation of the \
       \voiced labialized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙʷ"
        `shouldBe`
        "voiced labialized bilabial trill pulmonic egressive consonant"
  describe "voiced palatalized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙʲ] \
       \is the representation of the \
       \voiced palatalized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙʲ"
        `shouldBe`
        "voiced palatalized bilabial trill pulmonic egressive consonant"
  describe "voiced velarized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙˠ] \
       \is the representation of the \
       \voiced velarized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙˠ"
        `shouldBe`
        "voiced velarized bilabial trill pulmonic egressive consonant"
  describe "voiced pharyngealized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙˤ] \
       \is the representation of the \
       \voiced pharyngealized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙˤ"
        `shouldBe`
        "voiced pharyngealized bilabial trill pulmonic egressive consonant"
  describe "voiced aspirated bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙʰ] \
       \is the representation of the \
       \voiced aspirated bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙʰ"
        `shouldBe`
        "voiced aspirated bilabial trill pulmonic egressive consonant"
    it "should be that: [ʙ̬ʰ] \
       \is the representation of the \
       \voiced aspirated bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̬ʰ"
        `shouldBe`
        "voiced aspirated bilabial trill pulmonic egressive consonant"
  describe "voiced aspirated labialized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙʰʷ"
        `shouldBe`
        "voiced aspirated labialized bilabial trill pulmonic egressive consonant"
    it "should be that: [ʙ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized bilabial trill pulmonic egressive consonant"
  describe "voiced aspirated palatalized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙʰʲ"
        `shouldBe`
        "voiced aspirated palatalized bilabial trill pulmonic egressive consonant"
    it "should be that: [ʙ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized bilabial trill pulmonic egressive consonant"
  describe "voiced aspirated velarized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙʰˠ"
        `shouldBe`
        "voiced aspirated velarized bilabial trill pulmonic egressive consonant"
    it "should be that: [ʙ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized bilabial trill pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized bilabial trill pulmonic egressive consonant" $
    do
    it "should be that: [ʙʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized bilabial trill pulmonic egressive consonant"
    it "should be that: [ʙ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized bilabial trill pulmonic egressive consonant" $
      describeIPA "ʙ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized bilabial trill pulmonic egressive consonant"
  describe "voiceless alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [r̊] \
       \is the representation of the \
       \voiceless alveolar trill pulmonic egressive consonant" $
      describeIPA "r̊"
        `shouldBe`
        "voiceless alveolar trill pulmonic egressive consonant"
    it "should be that: [r̥] \
       \is the representation of the \
       \voiceless alveolar trill pulmonic egressive consonant" $
      describeIPA "r̥"
        `shouldBe`
        "voiceless alveolar trill pulmonic egressive consonant"
  describe "voiceless labialized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [r̊ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̊ʷ"
        `shouldBe`
        "voiceless labialized alveolar trill pulmonic egressive consonant"
    it "should be that: [r̥ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̥ʷ"
        `shouldBe`
        "voiceless labialized alveolar trill pulmonic egressive consonant"
  describe "voiceless palatalized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [r̊ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̊ʲ"
        `shouldBe`
        "voiceless palatalized alveolar trill pulmonic egressive consonant"
    it "should be that: [r̥ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̥ʲ"
        `shouldBe`
        "voiceless palatalized alveolar trill pulmonic egressive consonant"
  describe "voiceless velarized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [r̊ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̊ˠ"
        `shouldBe`
        "voiceless velarized alveolar trill pulmonic egressive consonant"
    it "should be that: [r̥ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̥ˠ"
        `shouldBe`
        "voiceless velarized alveolar trill pulmonic egressive consonant"
  describe "voiceless pharyngealized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [r̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̊ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar trill pulmonic egressive consonant"
    it "should be that: [r̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̥ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar trill pulmonic egressive consonant"
  describe "voiceless aspirated alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [r̥ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar trill pulmonic egressive consonant" $
      describeIPA "r̥ʰ"
        `shouldBe`
        "voiceless aspirated alveolar trill pulmonic egressive consonant"
    it "should be that: [r̊ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar trill pulmonic egressive consonant" $
      describeIPA "r̊ʰ"
        `shouldBe`
        "voiceless aspirated alveolar trill pulmonic egressive consonant"
  describe "voiceless aspirated labialized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [r̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar trill pulmonic egressive consonant"
    it "should be that: [r̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar trill pulmonic egressive consonant"
  describe "voiceless aspirated palatalized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [r̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar trill pulmonic egressive consonant"
    it "should be that: [r̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar trill pulmonic egressive consonant"
  describe "voiceless aspirated velarized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [r̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar trill pulmonic egressive consonant"
    it "should be that: [r̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar trill pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [r̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar trill pulmonic egressive consonant"
    it "should be that: [r̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar trill pulmonic egressive consonant"
  describe "voiced alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [r] \
       \is the representation of the \
       \voiced alveolar trill pulmonic egressive consonant" $
      describeIPA "r"
        `shouldBe`
        "voiced alveolar trill pulmonic egressive consonant"
  describe "voiced labialized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [rʷ] \
       \is the representation of the \
       \voiced labialized alveolar trill pulmonic egressive consonant" $
      describeIPA "rʷ"
        `shouldBe`
        "voiced labialized alveolar trill pulmonic egressive consonant"
  describe "voiced palatalized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [rʲ] \
       \is the representation of the \
       \voiced palatalized alveolar trill pulmonic egressive consonant" $
      describeIPA "rʲ"
        `shouldBe`
        "voiced palatalized alveolar trill pulmonic egressive consonant"
  describe "voiced velarized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [rˠ] \
       \is the representation of the \
       \voiced velarized alveolar trill pulmonic egressive consonant" $
      describeIPA "rˠ"
        `shouldBe`
        "voiced velarized alveolar trill pulmonic egressive consonant"
  describe "voiced pharyngealized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [rˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar trill pulmonic egressive consonant" $
      describeIPA "rˤ"
        `shouldBe`
        "voiced pharyngealized alveolar trill pulmonic egressive consonant"
  describe "voiced aspirated alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [rʰ] \
       \is the representation of the \
       \voiced aspirated alveolar trill pulmonic egressive consonant" $
      describeIPA "rʰ"
        `shouldBe`
        "voiced aspirated alveolar trill pulmonic egressive consonant"
    it "should be that: [r̬ʰ] \
       \is the representation of the \
       \voiced aspirated alveolar trill pulmonic egressive consonant" $
      describeIPA "r̬ʰ"
        `shouldBe`
        "voiced aspirated alveolar trill pulmonic egressive consonant"
  describe "voiced aspirated labialized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [rʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar trill pulmonic egressive consonant" $
      describeIPA "rʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar trill pulmonic egressive consonant"
    it "should be that: [r̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar trill pulmonic egressive consonant"
  describe "voiced aspirated palatalized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [rʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar trill pulmonic egressive consonant" $
      describeIPA "rʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar trill pulmonic egressive consonant"
    it "should be that: [r̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar trill pulmonic egressive consonant"
  describe "voiced aspirated velarized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [rʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar trill pulmonic egressive consonant" $
      describeIPA "rʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar trill pulmonic egressive consonant"
    it "should be that: [r̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar trill pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized alveolar trill pulmonic egressive consonant" $
    do
    it "should be that: [rʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar trill pulmonic egressive consonant" $
      describeIPA "rʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar trill pulmonic egressive consonant"
    it "should be that: [r̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar trill pulmonic egressive consonant" $
      describeIPA "r̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar trill pulmonic egressive consonant"
  describe "voiceless uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀ̊] \
       \is the representation of the \
       \voiceless uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̊"
        `shouldBe`
        "voiceless uvular trill pulmonic egressive consonant"
    it "should be that: [ʀ̥] \
       \is the representation of the \
       \voiceless uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̥"
        `shouldBe`
        "voiceless uvular trill pulmonic egressive consonant"
  describe "voiceless labialized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀ̊ʷ] \
       \is the representation of the \
       \voiceless labialized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̊ʷ"
        `shouldBe`
        "voiceless labialized uvular trill pulmonic egressive consonant"
    it "should be that: [ʀ̥ʷ] \
       \is the representation of the \
       \voiceless labialized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̥ʷ"
        `shouldBe`
        "voiceless labialized uvular trill pulmonic egressive consonant"
  describe "voiceless palatalized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̊ʲ"
        `shouldBe`
        "voiceless palatalized uvular trill pulmonic egressive consonant"
    it "should be that: [ʀ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̥ʲ"
        `shouldBe`
        "voiceless palatalized uvular trill pulmonic egressive consonant"
  describe "voiceless velarized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀ̊ˠ] \
       \is the representation of the \
       \voiceless velarized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̊ˠ"
        `shouldBe`
        "voiceless velarized uvular trill pulmonic egressive consonant"
    it "should be that: [ʀ̥ˠ] \
       \is the representation of the \
       \voiceless velarized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̥ˠ"
        `shouldBe`
        "voiceless velarized uvular trill pulmonic egressive consonant"
  describe "voiceless pharyngealized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular trill pulmonic egressive consonant"
    it "should be that: [ʀ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular trill pulmonic egressive consonant"
  describe "voiceless aspirated uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̥ʰ"
        `shouldBe`
        "voiceless aspirated uvular trill pulmonic egressive consonant"
    it "should be that: [ʀ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̊ʰ"
        `shouldBe`
        "voiceless aspirated uvular trill pulmonic egressive consonant"
  describe "voiceless aspirated labialized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized uvular trill pulmonic egressive consonant"
    it "should be that: [ʀ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized uvular trill pulmonic egressive consonant"
  describe "voiceless aspirated palatalized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized uvular trill pulmonic egressive consonant"
    it "should be that: [ʀ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized uvular trill pulmonic egressive consonant"
  describe "voiceless aspirated velarized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized uvular trill pulmonic egressive consonant"
    it "should be that: [ʀ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized uvular trill pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized uvular trill pulmonic egressive consonant"
    it "should be that: [ʀ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized uvular trill pulmonic egressive consonant"
  describe "voiced uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀ] \
       \is the representation of the \
       \voiced uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ"
        `shouldBe`
        "voiced uvular trill pulmonic egressive consonant"
  describe "voiced labialized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀʷ] \
       \is the representation of the \
       \voiced labialized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀʷ"
        `shouldBe`
        "voiced labialized uvular trill pulmonic egressive consonant"
  describe "voiced palatalized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀʲ] \
       \is the representation of the \
       \voiced palatalized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀʲ"
        `shouldBe`
        "voiced palatalized uvular trill pulmonic egressive consonant"
  describe "voiced velarized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀˠ] \
       \is the representation of the \
       \voiced velarized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀˠ"
        `shouldBe`
        "voiced velarized uvular trill pulmonic egressive consonant"
  describe "voiced pharyngealized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀˤ] \
       \is the representation of the \
       \voiced pharyngealized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀˤ"
        `shouldBe`
        "voiced pharyngealized uvular trill pulmonic egressive consonant"
  describe "voiced aspirated uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀʰ] \
       \is the representation of the \
       \voiced aspirated uvular trill pulmonic egressive consonant" $
      describeIPA "ʀʰ"
        `shouldBe`
        "voiced aspirated uvular trill pulmonic egressive consonant"
    it "should be that: [ʀ̬ʰ] \
       \is the representation of the \
       \voiced aspirated uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̬ʰ"
        `shouldBe`
        "voiced aspirated uvular trill pulmonic egressive consonant"
  describe "voiced aspirated labialized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀʰʷ"
        `shouldBe`
        "voiced aspirated labialized uvular trill pulmonic egressive consonant"
    it "should be that: [ʀ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized uvular trill pulmonic egressive consonant"
  describe "voiced aspirated palatalized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀʰʲ"
        `shouldBe`
        "voiced aspirated palatalized uvular trill pulmonic egressive consonant"
    it "should be that: [ʀ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized uvular trill pulmonic egressive consonant"
  describe "voiced aspirated velarized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀʰˠ"
        `shouldBe`
        "voiced aspirated velarized uvular trill pulmonic egressive consonant"
    it "should be that: [ʀ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized uvular trill pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized uvular trill pulmonic egressive consonant" $
    do
    it "should be that: [ʀʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized uvular trill pulmonic egressive consonant"
    it "should be that: [ʀ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized uvular trill pulmonic egressive consonant" $
      describeIPA "ʀ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized uvular trill pulmonic egressive consonant"
  describe "voiceless labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱ̊] \
       \is the representation of the \
       \voiceless labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̊"
        `shouldBe`
        "voiceless labio-dental tap or flap pulmonic egressive consonant"
    it "should be that: [ⱱ̥] \
       \is the representation of the \
       \voiceless labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̥"
        `shouldBe`
        "voiceless labio-dental tap or flap pulmonic egressive consonant"
  describe "voiceless labialized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱ̊ʷ] \
       \is the representation of the \
       \voiceless labialized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̊ʷ"
        `shouldBe`
        "voiceless labialized labio-dental tap or flap pulmonic egressive consonant"
    it "should be that: [ⱱ̥ʷ] \
       \is the representation of the \
       \voiceless labialized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̥ʷ"
        `shouldBe`
        "voiceless labialized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiceless palatalized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̊ʲ"
        `shouldBe`
        "voiceless palatalized labio-dental tap or flap pulmonic egressive consonant"
    it "should be that: [ⱱ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̥ʲ"
        `shouldBe`
        "voiceless palatalized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiceless velarized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱ̊ˠ] \
       \is the representation of the \
       \voiceless velarized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̊ˠ"
        `shouldBe`
        "voiceless velarized labio-dental tap or flap pulmonic egressive consonant"
    it "should be that: [ⱱ̥ˠ] \
       \is the representation of the \
       \voiceless velarized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̥ˠ"
        `shouldBe`
        "voiceless velarized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiceless pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized labio-dental tap or flap pulmonic egressive consonant"
    it "should be that: [ⱱ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiceless aspirated labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̥ʰ"
        `shouldBe`
        "voiceless aspirated labio-dental tap or flap pulmonic egressive consonant"
    it "should be that: [ⱱ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̊ʰ"
        `shouldBe`
        "voiceless aspirated labio-dental tap or flap pulmonic egressive consonant"
  describe "voiceless aspirated labialized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized labio-dental tap or flap pulmonic egressive consonant"
    it "should be that: [ⱱ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiceless aspirated palatalized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized labio-dental tap or flap pulmonic egressive consonant"
    it "should be that: [ⱱ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiceless aspirated velarized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized labio-dental tap or flap pulmonic egressive consonant"
    it "should be that: [ⱱ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant"
    it "should be that: [ⱱ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiced labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱ] \
       \is the representation of the \
       \voiced labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ"
        `shouldBe`
        "voiced labio-dental tap or flap pulmonic egressive consonant"
  describe "voiced labialized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱʷ] \
       \is the representation of the \
       \voiced labialized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱʷ"
        `shouldBe`
        "voiced labialized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiced palatalized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱʲ] \
       \is the representation of the \
       \voiced palatalized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱʲ"
        `shouldBe`
        "voiced palatalized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiced velarized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱˠ] \
       \is the representation of the \
       \voiced velarized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱˠ"
        `shouldBe`
        "voiced velarized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiced pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱˤ] \
       \is the representation of the \
       \voiced pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱˤ"
        `shouldBe`
        "voiced pharyngealized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiced aspirated labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱʰ] \
       \is the representation of the \
       \voiced aspirated labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱʰ"
        `shouldBe`
        "voiced aspirated labio-dental tap or flap pulmonic egressive consonant"
    it "should be that: [ⱱ̬ʰ] \
       \is the representation of the \
       \voiced aspirated labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̬ʰ"
        `shouldBe`
        "voiced aspirated labio-dental tap or flap pulmonic egressive consonant"
  describe "voiced aspirated labialized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱʰʷ"
        `shouldBe`
        "voiced aspirated labialized labio-dental tap or flap pulmonic egressive consonant"
    it "should be that: [ⱱ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiced aspirated palatalized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱʰʲ"
        `shouldBe`
        "voiced aspirated palatalized labio-dental tap or flap pulmonic egressive consonant"
    it "should be that: [ⱱ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiced aspirated velarized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱʰˠ"
        `shouldBe`
        "voiced aspirated velarized labio-dental tap or flap pulmonic egressive consonant"
    it "should be that: [ⱱ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ⱱʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant"
    it "should be that: [ⱱ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
      describeIPA "ⱱ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant"
  describe "voiceless alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾ̊] \
       \is the representation of the \
       \voiceless alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̊"
        `shouldBe`
        "voiceless alveolar tap or flap pulmonic egressive consonant"
    it "should be that: [ɾ̥] \
       \is the representation of the \
       \voiceless alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̥"
        `shouldBe`
        "voiceless alveolar tap or flap pulmonic egressive consonant"
  describe "voiceless labialized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾ̊ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̊ʷ"
        `shouldBe`
        "voiceless labialized alveolar tap or flap pulmonic egressive consonant"
    it "should be that: [ɾ̥ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̥ʷ"
        `shouldBe`
        "voiceless labialized alveolar tap or flap pulmonic egressive consonant"
  describe "voiceless palatalized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̊ʲ"
        `shouldBe`
        "voiceless palatalized alveolar tap or flap pulmonic egressive consonant"
    it "should be that: [ɾ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̥ʲ"
        `shouldBe`
        "voiceless palatalized alveolar tap or flap pulmonic egressive consonant"
  describe "voiceless velarized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾ̊ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̊ˠ"
        `shouldBe`
        "voiceless velarized alveolar tap or flap pulmonic egressive consonant"
    it "should be that: [ɾ̥ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̥ˠ"
        `shouldBe`
        "voiceless velarized alveolar tap or flap pulmonic egressive consonant"
  describe "voiceless pharyngealized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar tap or flap pulmonic egressive consonant"
    it "should be that: [ɾ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar tap or flap pulmonic egressive consonant"
  describe "voiceless aspirated alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̥ʰ"
        `shouldBe`
        "voiceless aspirated alveolar tap or flap pulmonic egressive consonant"
    it "should be that: [ɾ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̊ʰ"
        `shouldBe`
        "voiceless aspirated alveolar tap or flap pulmonic egressive consonant"
  describe "voiceless aspirated labialized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar tap or flap pulmonic egressive consonant"
    it "should be that: [ɾ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar tap or flap pulmonic egressive consonant"
  describe "voiceless aspirated palatalized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar tap or flap pulmonic egressive consonant"
    it "should be that: [ɾ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar tap or flap pulmonic egressive consonant"
  describe "voiceless aspirated velarized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar tap or flap pulmonic egressive consonant"
    it "should be that: [ɾ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar tap or flap pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant"
    it "should be that: [ɾ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant"
  describe "voiced alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾ] \
       \is the representation of the \
       \voiced alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ"
        `shouldBe`
        "voiced alveolar tap or flap pulmonic egressive consonant"
  describe "voiced labialized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾʷ] \
       \is the representation of the \
       \voiced labialized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾʷ"
        `shouldBe`
        "voiced labialized alveolar tap or flap pulmonic egressive consonant"
  describe "voiced palatalized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾʲ] \
       \is the representation of the \
       \voiced palatalized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾʲ"
        `shouldBe`
        "voiced palatalized alveolar tap or flap pulmonic egressive consonant"
  describe "voiced velarized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾˠ] \
       \is the representation of the \
       \voiced velarized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾˠ"
        `shouldBe`
        "voiced velarized alveolar tap or flap pulmonic egressive consonant"
  describe "voiced pharyngealized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾˤ"
        `shouldBe`
        "voiced pharyngealized alveolar tap or flap pulmonic egressive consonant"
  describe "voiced aspirated alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾʰ] \
       \is the representation of the \
       \voiced aspirated alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾʰ"
        `shouldBe`
        "voiced aspirated alveolar tap or flap pulmonic egressive consonant"
    it "should be that: [ɾ̬ʰ] \
       \is the representation of the \
       \voiced aspirated alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̬ʰ"
        `shouldBe`
        "voiced aspirated alveolar tap or flap pulmonic egressive consonant"
  describe "voiced aspirated labialized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar tap or flap pulmonic egressive consonant"
    it "should be that: [ɾ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar tap or flap pulmonic egressive consonant"
  describe "voiced aspirated palatalized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar tap or flap pulmonic egressive consonant"
    it "should be that: [ɾ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar tap or flap pulmonic egressive consonant"
  describe "voiced aspirated velarized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar tap or flap pulmonic egressive consonant"
    it "should be that: [ɾ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar tap or flap pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɾʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant"
    it "should be that: [ɾ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant" $
      describeIPA "ɾ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant"
  describe "voiceless retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽ̊] \
       \is the representation of the \
       \voiceless retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̊"
        `shouldBe`
        "voiceless retroflex tap or flap pulmonic egressive consonant"
    it "should be that: [ɽ̥] \
       \is the representation of the \
       \voiceless retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̥"
        `shouldBe`
        "voiceless retroflex tap or flap pulmonic egressive consonant"
  describe "voiceless labialized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽ̊ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̊ʷ"
        `shouldBe`
        "voiceless labialized retroflex tap or flap pulmonic egressive consonant"
    it "should be that: [ɽ̥ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̥ʷ"
        `shouldBe`
        "voiceless labialized retroflex tap or flap pulmonic egressive consonant"
  describe "voiceless palatalized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̊ʲ"
        `shouldBe`
        "voiceless palatalized retroflex tap or flap pulmonic egressive consonant"
    it "should be that: [ɽ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̥ʲ"
        `shouldBe`
        "voiceless palatalized retroflex tap or flap pulmonic egressive consonant"
  describe "voiceless velarized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽ̊ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̊ˠ"
        `shouldBe`
        "voiceless velarized retroflex tap or flap pulmonic egressive consonant"
    it "should be that: [ɽ̥ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̥ˠ"
        `shouldBe`
        "voiceless velarized retroflex tap or flap pulmonic egressive consonant"
  describe "voiceless pharyngealized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex tap or flap pulmonic egressive consonant"
    it "should be that: [ɽ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex tap or flap pulmonic egressive consonant"
  describe "voiceless aspirated retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̥ʰ"
        `shouldBe`
        "voiceless aspirated retroflex tap or flap pulmonic egressive consonant"
    it "should be that: [ɽ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̊ʰ"
        `shouldBe`
        "voiceless aspirated retroflex tap or flap pulmonic egressive consonant"
  describe "voiceless aspirated labialized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex tap or flap pulmonic egressive consonant"
    it "should be that: [ɽ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex tap or flap pulmonic egressive consonant"
  describe "voiceless aspirated palatalized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex tap or flap pulmonic egressive consonant"
    it "should be that: [ɽ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex tap or flap pulmonic egressive consonant"
  describe "voiceless aspirated velarized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex tap or flap pulmonic egressive consonant"
    it "should be that: [ɽ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex tap or flap pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant"
    it "should be that: [ɽ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant"
  describe "voiced retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽ] \
       \is the representation of the \
       \voiced retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ"
        `shouldBe`
        "voiced retroflex tap or flap pulmonic egressive consonant"
  describe "voiced labialized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽʷ] \
       \is the representation of the \
       \voiced labialized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽʷ"
        `shouldBe`
        "voiced labialized retroflex tap or flap pulmonic egressive consonant"
  describe "voiced palatalized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽʲ] \
       \is the representation of the \
       \voiced palatalized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽʲ"
        `shouldBe`
        "voiced palatalized retroflex tap or flap pulmonic egressive consonant"
  describe "voiced velarized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽˠ] \
       \is the representation of the \
       \voiced velarized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽˠ"
        `shouldBe`
        "voiced velarized retroflex tap or flap pulmonic egressive consonant"
  describe "voiced pharyngealized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽˤ] \
       \is the representation of the \
       \voiced pharyngealized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽˤ"
        `shouldBe`
        "voiced pharyngealized retroflex tap or flap pulmonic egressive consonant"
  describe "voiced aspirated retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽʰ] \
       \is the representation of the \
       \voiced aspirated retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽʰ"
        `shouldBe`
        "voiced aspirated retroflex tap or flap pulmonic egressive consonant"
    it "should be that: [ɽ̬ʰ] \
       \is the representation of the \
       \voiced aspirated retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̬ʰ"
        `shouldBe`
        "voiced aspirated retroflex tap or flap pulmonic egressive consonant"
  describe "voiced aspirated labialized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex tap or flap pulmonic egressive consonant"
    it "should be that: [ɽ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex tap or flap pulmonic egressive consonant"
  describe "voiced aspirated palatalized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex tap or flap pulmonic egressive consonant"
    it "should be that: [ɽ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex tap or flap pulmonic egressive consonant"
  describe "voiced aspirated velarized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex tap or flap pulmonic egressive consonant"
    it "should be that: [ɽ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex tap or flap pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant" $
    do
    it "should be that: [ɽʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant"
    it "should be that: [ɽ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant" $
      describeIPA "ɽ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant"
  describe "voiceless labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋ̊] \
       \is the representation of the \
       \voiceless labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̊"
        `shouldBe`
        "voiceless labio-dental approximant pulmonic egressive consonant"
    it "should be that: [ʋ̥] \
       \is the representation of the \
       \voiceless labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̥"
        `shouldBe`
        "voiceless labio-dental approximant pulmonic egressive consonant"
  describe "voiceless labialized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋ̊ʷ] \
       \is the representation of the \
       \voiceless labialized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̊ʷ"
        `shouldBe`
        "voiceless labialized labio-dental approximant pulmonic egressive consonant"
    it "should be that: [ʋ̥ʷ] \
       \is the representation of the \
       \voiceless labialized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̥ʷ"
        `shouldBe`
        "voiceless labialized labio-dental approximant pulmonic egressive consonant"
  describe "voiceless palatalized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̊ʲ"
        `shouldBe`
        "voiceless palatalized labio-dental approximant pulmonic egressive consonant"
    it "should be that: [ʋ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̥ʲ"
        `shouldBe`
        "voiceless palatalized labio-dental approximant pulmonic egressive consonant"
  describe "voiceless velarized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋ̊ˠ] \
       \is the representation of the \
       \voiceless velarized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̊ˠ"
        `shouldBe`
        "voiceless velarized labio-dental approximant pulmonic egressive consonant"
    it "should be that: [ʋ̥ˠ] \
       \is the representation of the \
       \voiceless velarized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̥ˠ"
        `shouldBe`
        "voiceless velarized labio-dental approximant pulmonic egressive consonant"
  describe "voiceless pharyngealized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized labio-dental approximant pulmonic egressive consonant"
    it "should be that: [ʋ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized labio-dental approximant pulmonic egressive consonant"
  describe "voiceless aspirated labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̥ʰ"
        `shouldBe`
        "voiceless aspirated labio-dental approximant pulmonic egressive consonant"
    it "should be that: [ʋ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̊ʰ"
        `shouldBe`
        "voiceless aspirated labio-dental approximant pulmonic egressive consonant"
  describe "voiceless aspirated labialized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized labio-dental approximant pulmonic egressive consonant"
    it "should be that: [ʋ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized labio-dental approximant pulmonic egressive consonant"
  describe "voiceless aspirated palatalized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized labio-dental approximant pulmonic egressive consonant"
    it "should be that: [ʋ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized labio-dental approximant pulmonic egressive consonant"
  describe "voiceless aspirated velarized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized labio-dental approximant pulmonic egressive consonant"
    it "should be that: [ʋ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized labio-dental approximant pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized labio-dental approximant pulmonic egressive consonant"
    it "should be that: [ʋ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized labio-dental approximant pulmonic egressive consonant"
  describe "voiced labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋ] \
       \is the representation of the \
       \voiced labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ"
        `shouldBe`
        "voiced labio-dental approximant pulmonic egressive consonant"
  describe "voiced labialized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋʷ] \
       \is the representation of the \
       \voiced labialized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋʷ"
        `shouldBe`
        "voiced labialized labio-dental approximant pulmonic egressive consonant"
  describe "voiced palatalized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋʲ] \
       \is the representation of the \
       \voiced palatalized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋʲ"
        `shouldBe`
        "voiced palatalized labio-dental approximant pulmonic egressive consonant"
  describe "voiced velarized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋˠ] \
       \is the representation of the \
       \voiced velarized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋˠ"
        `shouldBe`
        "voiced velarized labio-dental approximant pulmonic egressive consonant"
  describe "voiced pharyngealized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋˤ] \
       \is the representation of the \
       \voiced pharyngealized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋˤ"
        `shouldBe`
        "voiced pharyngealized labio-dental approximant pulmonic egressive consonant"
  describe "voiced aspirated labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋʰ] \
       \is the representation of the \
       \voiced aspirated labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋʰ"
        `shouldBe`
        "voiced aspirated labio-dental approximant pulmonic egressive consonant"
    it "should be that: [ʋ̬ʰ] \
       \is the representation of the \
       \voiced aspirated labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̬ʰ"
        `shouldBe`
        "voiced aspirated labio-dental approximant pulmonic egressive consonant"
  describe "voiced aspirated labialized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋʰʷ"
        `shouldBe`
        "voiced aspirated labialized labio-dental approximant pulmonic egressive consonant"
    it "should be that: [ʋ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized labio-dental approximant pulmonic egressive consonant"
  describe "voiced aspirated palatalized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋʰʲ"
        `shouldBe`
        "voiced aspirated palatalized labio-dental approximant pulmonic egressive consonant"
    it "should be that: [ʋ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized labio-dental approximant pulmonic egressive consonant"
  describe "voiced aspirated velarized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋʰˠ"
        `shouldBe`
        "voiced aspirated velarized labio-dental approximant pulmonic egressive consonant"
    it "should be that: [ʋ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized labio-dental approximant pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized labio-dental approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʋʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized labio-dental approximant pulmonic egressive consonant"
    it "should be that: [ʋ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized labio-dental approximant pulmonic egressive consonant" $
      describeIPA "ʋ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized labio-dental approximant pulmonic egressive consonant"
  describe "voiceless alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹ̊] \
       \is the representation of the \
       \voiceless alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̊"
        `shouldBe`
        "voiceless alveolar approximant pulmonic egressive consonant"
    it "should be that: [ɹ̥] \
       \is the representation of the \
       \voiceless alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̥"
        `shouldBe`
        "voiceless alveolar approximant pulmonic egressive consonant"
  describe "voiceless labialized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹ̊ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̊ʷ"
        `shouldBe`
        "voiceless labialized alveolar approximant pulmonic egressive consonant"
    it "should be that: [ɹ̥ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̥ʷ"
        `shouldBe`
        "voiceless labialized alveolar approximant pulmonic egressive consonant"
  describe "voiceless palatalized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̊ʲ"
        `shouldBe`
        "voiceless palatalized alveolar approximant pulmonic egressive consonant"
    it "should be that: [ɹ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̥ʲ"
        `shouldBe`
        "voiceless palatalized alveolar approximant pulmonic egressive consonant"
  describe "voiceless velarized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹ̊ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̊ˠ"
        `shouldBe`
        "voiceless velarized alveolar approximant pulmonic egressive consonant"
    it "should be that: [ɹ̥ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̥ˠ"
        `shouldBe`
        "voiceless velarized alveolar approximant pulmonic egressive consonant"
  describe "voiceless pharyngealized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar approximant pulmonic egressive consonant"
    it "should be that: [ɹ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar approximant pulmonic egressive consonant"
  describe "voiceless aspirated alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̥ʰ"
        `shouldBe`
        "voiceless aspirated alveolar approximant pulmonic egressive consonant"
    it "should be that: [ɹ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̊ʰ"
        `shouldBe`
        "voiceless aspirated alveolar approximant pulmonic egressive consonant"
  describe "voiceless aspirated labialized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar approximant pulmonic egressive consonant"
    it "should be that: [ɹ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar approximant pulmonic egressive consonant"
  describe "voiceless aspirated palatalized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar approximant pulmonic egressive consonant"
    it "should be that: [ɹ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar approximant pulmonic egressive consonant"
  describe "voiceless aspirated velarized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar approximant pulmonic egressive consonant"
    it "should be that: [ɹ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar approximant pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar approximant pulmonic egressive consonant"
    it "should be that: [ɹ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar approximant pulmonic egressive consonant"
  describe "voiced alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹ] \
       \is the representation of the \
       \voiced alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ"
        `shouldBe`
        "voiced alveolar approximant pulmonic egressive consonant"
  describe "voiced labialized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹʷ] \
       \is the representation of the \
       \voiced labialized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹʷ"
        `shouldBe`
        "voiced labialized alveolar approximant pulmonic egressive consonant"
  describe "voiced palatalized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹʲ] \
       \is the representation of the \
       \voiced palatalized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹʲ"
        `shouldBe`
        "voiced palatalized alveolar approximant pulmonic egressive consonant"
  describe "voiced velarized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹˠ] \
       \is the representation of the \
       \voiced velarized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹˠ"
        `shouldBe`
        "voiced velarized alveolar approximant pulmonic egressive consonant"
  describe "voiced pharyngealized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹˤ"
        `shouldBe`
        "voiced pharyngealized alveolar approximant pulmonic egressive consonant"
  describe "voiced aspirated alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹʰ] \
       \is the representation of the \
       \voiced aspirated alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹʰ"
        `shouldBe`
        "voiced aspirated alveolar approximant pulmonic egressive consonant"
    it "should be that: [ɹ̬ʰ] \
       \is the representation of the \
       \voiced aspirated alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̬ʰ"
        `shouldBe`
        "voiced aspirated alveolar approximant pulmonic egressive consonant"
  describe "voiced aspirated labialized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar approximant pulmonic egressive consonant"
    it "should be that: [ɹ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar approximant pulmonic egressive consonant"
  describe "voiced aspirated palatalized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar approximant pulmonic egressive consonant"
    it "should be that: [ɹ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar approximant pulmonic egressive consonant"
  describe "voiced aspirated velarized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar approximant pulmonic egressive consonant"
    it "should be that: [ɹ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar approximant pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized alveolar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɹʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar approximant pulmonic egressive consonant"
    it "should be that: [ɹ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar approximant pulmonic egressive consonant" $
      describeIPA "ɹ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar approximant pulmonic egressive consonant"
  describe "voiceless retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̊] \
       \is the representation of the \
       \voiceless retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊"
        `shouldBe`
        "voiceless retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̥] \
       \is the representation of the \
       \voiceless retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥"
        `shouldBe`
        "voiceless retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless labialized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̊ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ʷ"
        `shouldBe`
        "voiceless labialized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̥ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ʷ"
        `shouldBe`
        "voiceless labialized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless palatalized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ʲ"
        `shouldBe`
        "voiceless palatalized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ʲ"
        `shouldBe`
        "voiceless palatalized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless velarized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̊ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ˠ"
        `shouldBe`
        "voiceless velarized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̥ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ˠ"
        `shouldBe`
        "voiceless velarized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ʰ"
        `shouldBe`
        "voiceless aspirated retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ʰ"
        `shouldBe`
        "voiceless aspirated retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated labialized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated palatalized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated velarized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ] \
       \is the representation of the \
       \voiced retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ"
        `shouldBe`
        "voiced retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced labialized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭʷ] \
       \is the representation of the \
       \voiced labialized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭʷ"
        `shouldBe`
        "voiced labialized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced palatalized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭʲ] \
       \is the representation of the \
       \voiced palatalized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭʲ"
        `shouldBe`
        "voiced palatalized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced velarized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭˠ] \
       \is the representation of the \
       \voiced velarized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭˠ"
        `shouldBe`
        "voiced velarized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭˤ] \
       \is the representation of the \
       \voiced pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭˤ"
        `shouldBe`
        "voiced pharyngealized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭʰ] \
       \is the representation of the \
       \voiced aspirated retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭʰ"
        `shouldBe`
        "voiced aspirated retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̬ʰ] \
       \is the representation of the \
       \voiced aspirated retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̬ʰ"
        `shouldBe`
        "voiced aspirated retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated labialized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated palatalized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated velarized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̊] \
       \is the representation of the \
       \voiceless palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊"
        `shouldBe`
        "voiceless palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̥] \
       \is the representation of the \
       \voiceless palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥"
        `shouldBe`
        "voiceless palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless labialized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̊ʷ] \
       \is the representation of the \
       \voiceless labialized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ʷ"
        `shouldBe`
        "voiceless labialized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̥ʷ] \
       \is the representation of the \
       \voiceless labialized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ʷ"
        `shouldBe`
        "voiceless labialized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless palatalized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ʲ"
        `shouldBe`
        "voiceless palatalized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ʲ"
        `shouldBe`
        "voiceless palatalized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless velarized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̊ˠ] \
       \is the representation of the \
       \voiceless velarized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ˠ"
        `shouldBe`
        "voiceless velarized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̥ˠ] \
       \is the representation of the \
       \voiceless velarized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ˠ"
        `shouldBe`
        "voiceless velarized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless pharyngealized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ʰ"
        `shouldBe`
        "voiceless aspirated palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ʰ"
        `shouldBe`
        "voiceless aspirated palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated labialized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated palatalized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated velarized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ] \
       \is the representation of the \
       \voiced palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ"
        `shouldBe`
        "voiced palatal lateral approximant pulmonic egressive consonant"
  describe "voiced labialized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎʷ] \
       \is the representation of the \
       \voiced labialized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎʷ"
        `shouldBe`
        "voiced labialized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced palatalized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎʲ] \
       \is the representation of the \
       \voiced palatalized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎʲ"
        `shouldBe`
        "voiced palatalized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced velarized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎˠ] \
       \is the representation of the \
       \voiced velarized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎˠ"
        `shouldBe`
        "voiced velarized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced pharyngealized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎˤ] \
       \is the representation of the \
       \voiced pharyngealized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎˤ"
        `shouldBe`
        "voiced pharyngealized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎʰ] \
       \is the representation of the \
       \voiced aspirated palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎʰ"
        `shouldBe`
        "voiced aspirated palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̬ʰ] \
       \is the representation of the \
       \voiced aspirated palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̬ʰ"
        `shouldBe`
        "voiced aspirated palatal lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated labialized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎʰʷ"
        `shouldBe`
        "voiced aspirated labialized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated palatalized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎʰʲ"
        `shouldBe`
        "voiced aspirated palatalized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated velarized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎʰˠ"
        `shouldBe`
        "voiced aspirated velarized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̊] \
       \is the representation of the \
       \voiceless velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊"
        `shouldBe`
        "voiceless velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̥] \
       \is the representation of the \
       \voiceless velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥"
        `shouldBe`
        "voiceless velar lateral approximant pulmonic egressive consonant"
  describe "voiceless labialized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̊ʷ] \
       \is the representation of the \
       \voiceless labialized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ʷ"
        `shouldBe`
        "voiceless labialized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̥ʷ] \
       \is the representation of the \
       \voiceless labialized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ʷ"
        `shouldBe`
        "voiceless labialized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless palatalized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ʲ"
        `shouldBe`
        "voiceless palatalized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ʲ"
        `shouldBe`
        "voiceless palatalized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless velarized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̊ˠ] \
       \is the representation of the \
       \voiceless velarized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ˠ"
        `shouldBe`
        "voiceless velarized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̥ˠ] \
       \is the representation of the \
       \voiceless velarized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ˠ"
        `shouldBe`
        "voiceless velarized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless pharyngealized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ʰ"
        `shouldBe`
        "voiceless aspirated velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ʰ"
        `shouldBe`
        "voiceless aspirated velar lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated labialized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated palatalized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated velarized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized velar lateral approximant pulmonic egressive consonant"
  describe "voiced velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ] \
       \is the representation of the \
       \voiced velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ"
        `shouldBe`
        "voiced velar lateral approximant pulmonic egressive consonant"
  describe "voiced labialized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟʷ] \
       \is the representation of the \
       \voiced labialized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟʷ"
        `shouldBe`
        "voiced labialized velar lateral approximant pulmonic egressive consonant"
  describe "voiced palatalized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟʲ] \
       \is the representation of the \
       \voiced palatalized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟʲ"
        `shouldBe`
        "voiced palatalized velar lateral approximant pulmonic egressive consonant"
  describe "voiced velarized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟˠ] \
       \is the representation of the \
       \voiced velarized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟˠ"
        `shouldBe`
        "voiced velarized velar lateral approximant pulmonic egressive consonant"
  describe "voiced pharyngealized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟˤ] \
       \is the representation of the \
       \voiced pharyngealized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟˤ"
        `shouldBe`
        "voiced pharyngealized velar lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟʰ] \
       \is the representation of the \
       \voiced aspirated velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟʰ"
        `shouldBe`
        "voiced aspirated velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̬ʰ] \
       \is the representation of the \
       \voiced aspirated velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̬ʰ"
        `shouldBe`
        "voiced aspirated velar lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated labialized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟʰʷ"
        `shouldBe`
        "voiced aspirated labialized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized velar lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated palatalized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟʰʲ"
        `shouldBe`
        "voiced aspirated palatalized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized velar lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated velarized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟʰˠ"
        `shouldBe`
        "voiced aspirated velarized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized velar lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ẘ] \
       \is the representation of the \
       \voiceless labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘ"
        `shouldBe`
        "voiceless labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̥] \
       \is the representation of the \
       \voiceless labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥"
        `shouldBe`
        "voiceless labial-velar approximant pulmonic egressive consonant"
  describe "voiceless labialized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ẘʷ] \
       \is the representation of the \
       \voiceless labialized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘʷ"
        `shouldBe`
        "voiceless labialized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̥ʷ] \
       \is the representation of the \
       \voiceless labialized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ʷ"
        `shouldBe`
        "voiceless labialized labial-velar approximant pulmonic egressive consonant"
  describe "voiceless palatalized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ẘʲ] \
       \is the representation of the \
       \voiceless palatalized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘʲ"
        `shouldBe`
        "voiceless palatalized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̥ʲ] \
       \is the representation of the \
       \voiceless palatalized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ʲ"
        `shouldBe`
        "voiceless palatalized labial-velar approximant pulmonic egressive consonant"
  describe "voiceless velarized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ẘˠ] \
       \is the representation of the \
       \voiceless velarized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘˠ"
        `shouldBe`
        "voiceless velarized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̥ˠ] \
       \is the representation of the \
       \voiceless velarized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ˠ"
        `shouldBe`
        "voiceless velarized labial-velar approximant pulmonic egressive consonant"
  describe "voiceless pharyngealized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ẘˤ] \
       \is the representation of the \
       \voiceless pharyngealized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘˤ"
        `shouldBe`
        "voiceless pharyngealized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ˤ"
        `shouldBe`
        "voiceless pharyngealized labial-velar approximant pulmonic egressive consonant"
  describe "voiceless aspirated labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [w̥ʰ] \
       \is the representation of the \
       \voiceless aspirated labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ʰ"
        `shouldBe`
        "voiceless aspirated labial-velar approximant pulmonic egressive consonant"
    it "should be that: [ẘʰ] \
       \is the representation of the \
       \voiceless aspirated labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘʰ"
        `shouldBe`
        "voiceless aspirated labial-velar approximant pulmonic egressive consonant"
  describe "voiceless aspirated labialized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [w̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [ẘʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘʰʷ"
        `shouldBe`
        "voiceless aspirated labialized labial-velar approximant pulmonic egressive consonant"
  describe "voiceless aspirated palatalized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [w̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [ẘʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized labial-velar approximant pulmonic egressive consonant"
  describe "voiceless aspirated velarized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [w̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [ẘʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘʰˠ"
        `shouldBe`
        "voiceless aspirated velarized labial-velar approximant pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [w̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [ẘʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized labial-velar approximant pulmonic egressive consonant"
  describe "voiced labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [w] \
       \is the representation of the \
       \voiced labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w"
        `shouldBe`
        "voiced labial-velar approximant pulmonic egressive consonant"
  describe "voiced labialized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wʷ] \
       \is the representation of the \
       \voiced labialized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wʷ"
        `shouldBe`
        "voiced labialized labial-velar approximant pulmonic egressive consonant"
  describe "voiced palatalized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wʲ] \
       \is the representation of the \
       \voiced palatalized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wʲ"
        `shouldBe`
        "voiced palatalized labial-velar approximant pulmonic egressive consonant"
  describe "voiced velarized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wˠ] \
       \is the representation of the \
       \voiced velarized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wˠ"
        `shouldBe`
        "voiced velarized labial-velar approximant pulmonic egressive consonant"
  describe "voiced pharyngealized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wˤ] \
       \is the representation of the \
       \voiced pharyngealized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wˤ"
        `shouldBe`
        "voiced pharyngealized labial-velar approximant pulmonic egressive consonant"
  describe "voiced aspirated labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wʰ] \
       \is the representation of the \
       \voiced aspirated labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wʰ"
        `shouldBe`
        "voiced aspirated labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̬ʰ] \
       \is the representation of the \
       \voiced aspirated labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̬ʰ"
        `shouldBe`
        "voiced aspirated labial-velar approximant pulmonic egressive consonant"
  describe "voiced aspirated labialized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wʰʷ"
        `shouldBe`
        "voiced aspirated labialized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized labial-velar approximant pulmonic egressive consonant"
  describe "voiced aspirated palatalized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wʰʲ"
        `shouldBe`
        "voiced aspirated palatalized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized labial-velar approximant pulmonic egressive consonant"
  describe "voiced aspirated velarized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wʰˠ"
        `shouldBe`
        "voiced aspirated velarized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized labial-velar approximant pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized labial-velar approximant pulmonic egressive consonant"
  describe "voiceless labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍ] \
       \is the representation of the \
       \voiceless labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ"
        `shouldBe`
        "voiceless labial-velar fricative pulmonic egressive consonant"
    it "should be that: [ʍ̊] \
       \is the representation of the \
       \voiceless labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̊"
        `shouldBe`
        "voiceless labial-velar fricative pulmonic egressive consonant"
    it "should be that: [ʍ̥] \
       \is the representation of the \
       \voiceless labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̥"
        `shouldBe`
        "voiceless labial-velar fricative pulmonic egressive consonant"
  describe "voiceless labialized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍʷ] \
       \is the representation of the \
       \voiceless labialized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍʷ"
        `shouldBe`
        "voiceless labialized labial-velar fricative pulmonic egressive consonant"
    it "should be that: [ʍ̊ʷ] \
       \is the representation of the \
       \voiceless labialized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̊ʷ"
        `shouldBe`
        "voiceless labialized labial-velar fricative pulmonic egressive consonant"
    it "should be that: [ʍ̥ʷ] \
       \is the representation of the \
       \voiceless labialized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̥ʷ"
        `shouldBe`
        "voiceless labialized labial-velar fricative pulmonic egressive consonant"
  describe "voiceless palatalized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍʲ] \
       \is the representation of the \
       \voiceless palatalized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍʲ"
        `shouldBe`
        "voiceless palatalized labial-velar fricative pulmonic egressive consonant"
    it "should be that: [ʍ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̊ʲ"
        `shouldBe`
        "voiceless palatalized labial-velar fricative pulmonic egressive consonant"
    it "should be that: [ʍ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̥ʲ"
        `shouldBe`
        "voiceless palatalized labial-velar fricative pulmonic egressive consonant"
  describe "voiceless velarized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍˠ] \
       \is the representation of the \
       \voiceless velarized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍˠ"
        `shouldBe`
        "voiceless velarized labial-velar fricative pulmonic egressive consonant"
    it "should be that: [ʍ̊ˠ] \
       \is the representation of the \
       \voiceless velarized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̊ˠ"
        `shouldBe`
        "voiceless velarized labial-velar fricative pulmonic egressive consonant"
    it "should be that: [ʍ̥ˠ] \
       \is the representation of the \
       \voiceless velarized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̥ˠ"
        `shouldBe`
        "voiceless velarized labial-velar fricative pulmonic egressive consonant"
  describe "voiceless pharyngealized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍˤ] \
       \is the representation of the \
       \voiceless pharyngealized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍˤ"
        `shouldBe`
        "voiceless pharyngealized labial-velar fricative pulmonic egressive consonant"
    it "should be that: [ʍ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized labial-velar fricative pulmonic egressive consonant"
    it "should be that: [ʍ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized labial-velar fricative pulmonic egressive consonant"
  describe "voiceless aspirated labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍʰ] \
       \is the representation of the \
       \voiceless aspirated labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍʰ"
        `shouldBe`
        "voiceless aspirated labial-velar fricative pulmonic egressive consonant"
  describe "voiceless aspirated labialized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍʰʷ"
        `shouldBe`
        "voiceless aspirated labialized labial-velar fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatalized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized labial-velar fricative pulmonic egressive consonant"
  describe "voiceless aspirated velarized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍʰˠ"
        `shouldBe`
        "voiceless aspirated velarized labial-velar fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized labial-velar fricative pulmonic egressive consonant"
  describe "voiced labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍ̬] \
       \is the representation of the \
       \voiced labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̬"
        `shouldBe`
        "voiced labial-velar fricative pulmonic egressive consonant"
    it "should be that: [ʍ̬] \
       \is the representation of the \
       \voiced labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̬"
        `shouldBe`
        "voiced labial-velar fricative pulmonic egressive consonant"
  describe "voiced labialized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍ̬ʷ] \
       \is the representation of the \
       \voiced labialized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̬ʷ"
        `shouldBe`
        "voiced labialized labial-velar fricative pulmonic egressive consonant"
    it "should be that: [ʍ̬ʷ] \
       \is the representation of the \
       \voiced labialized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̬ʷ"
        `shouldBe`
        "voiced labialized labial-velar fricative pulmonic egressive consonant"
  describe "voiced palatalized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍ̬ʲ] \
       \is the representation of the \
       \voiced palatalized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̬ʲ"
        `shouldBe`
        "voiced palatalized labial-velar fricative pulmonic egressive consonant"
    it "should be that: [ʍ̬ʲ] \
       \is the representation of the \
       \voiced palatalized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̬ʲ"
        `shouldBe`
        "voiced palatalized labial-velar fricative pulmonic egressive consonant"
  describe "voiced velarized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍ̬ˠ] \
       \is the representation of the \
       \voiced velarized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̬ˠ"
        `shouldBe`
        "voiced velarized labial-velar fricative pulmonic egressive consonant"
    it "should be that: [ʍ̬ˠ] \
       \is the representation of the \
       \voiced velarized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̬ˠ"
        `shouldBe`
        "voiced velarized labial-velar fricative pulmonic egressive consonant"
  describe "voiced pharyngealized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̬ˤ"
        `shouldBe`
        "voiced pharyngealized labial-velar fricative pulmonic egressive consonant"
    it "should be that: [ʍ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̬ˤ"
        `shouldBe`
        "voiced pharyngealized labial-velar fricative pulmonic egressive consonant"
  describe "voiced aspirated labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍ̬ʰ] \
       \is the representation of the \
       \voiced aspirated labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̬ʰ"
        `shouldBe`
        "voiced aspirated labial-velar fricative pulmonic egressive consonant"
  describe "voiced aspirated labialized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized labial-velar fricative pulmonic egressive consonant"
  describe "voiced aspirated palatalized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized labial-velar fricative pulmonic egressive consonant"
  describe "voiced aspirated velarized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized labial-velar fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized labial-velar fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʍ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized labial-velar fricative pulmonic egressive consonant" $
      describeIPA "ʍ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized labial-velar fricative pulmonic egressive consonant"
  describe "voiceless epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡ] \
       \is the representation of the \
       \voiceless epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ"
        `shouldBe`
        "voiceless epiglottal plosive pulmonic egressive consonant"
    it "should be that: [ʡ̊] \
       \is the representation of the \
       \voiceless epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̊"
        `shouldBe`
        "voiceless epiglottal plosive pulmonic egressive consonant"
    it "should be that: [ʡ̥] \
       \is the representation of the \
       \voiceless epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̥"
        `shouldBe`
        "voiceless epiglottal plosive pulmonic egressive consonant"
  describe "voiceless labialized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡʷ] \
       \is the representation of the \
       \voiceless labialized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡʷ"
        `shouldBe`
        "voiceless labialized epiglottal plosive pulmonic egressive consonant"
    it "should be that: [ʡ̊ʷ] \
       \is the representation of the \
       \voiceless labialized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̊ʷ"
        `shouldBe`
        "voiceless labialized epiglottal plosive pulmonic egressive consonant"
    it "should be that: [ʡ̥ʷ] \
       \is the representation of the \
       \voiceless labialized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̥ʷ"
        `shouldBe`
        "voiceless labialized epiglottal plosive pulmonic egressive consonant"
  describe "voiceless palatalized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡʲ] \
       \is the representation of the \
       \voiceless palatalized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡʲ"
        `shouldBe`
        "voiceless palatalized epiglottal plosive pulmonic egressive consonant"
    it "should be that: [ʡ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̊ʲ"
        `shouldBe`
        "voiceless palatalized epiglottal plosive pulmonic egressive consonant"
    it "should be that: [ʡ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̥ʲ"
        `shouldBe`
        "voiceless palatalized epiglottal plosive pulmonic egressive consonant"
  describe "voiceless velarized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡˠ] \
       \is the representation of the \
       \voiceless velarized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡˠ"
        `shouldBe`
        "voiceless velarized epiglottal plosive pulmonic egressive consonant"
    it "should be that: [ʡ̊ˠ] \
       \is the representation of the \
       \voiceless velarized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̊ˠ"
        `shouldBe`
        "voiceless velarized epiglottal plosive pulmonic egressive consonant"
    it "should be that: [ʡ̥ˠ] \
       \is the representation of the \
       \voiceless velarized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̥ˠ"
        `shouldBe`
        "voiceless velarized epiglottal plosive pulmonic egressive consonant"
  describe "voiceless pharyngealized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡˤ] \
       \is the representation of the \
       \voiceless pharyngealized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡˤ"
        `shouldBe`
        "voiceless pharyngealized epiglottal plosive pulmonic egressive consonant"
    it "should be that: [ʡ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized epiglottal plosive pulmonic egressive consonant"
    it "should be that: [ʡ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized epiglottal plosive pulmonic egressive consonant"
  describe "voiceless aspirated epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡʰ] \
       \is the representation of the \
       \voiceless aspirated epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡʰ"
        `shouldBe`
        "voiceless aspirated epiglottal plosive pulmonic egressive consonant"
  describe "voiceless aspirated labialized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡʰʷ"
        `shouldBe`
        "voiceless aspirated labialized epiglottal plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatalized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized epiglottal plosive pulmonic egressive consonant"
  describe "voiceless aspirated velarized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡʰˠ"
        `shouldBe`
        "voiceless aspirated velarized epiglottal plosive pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized epiglottal plosive pulmonic egressive consonant"
  describe "voiced epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡ̬] \
       \is the representation of the \
       \voiced epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̬"
        `shouldBe`
        "voiced epiglottal plosive pulmonic egressive consonant"
    it "should be that: [ʡ̬] \
       \is the representation of the \
       \voiced epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̬"
        `shouldBe`
        "voiced epiglottal plosive pulmonic egressive consonant"
  describe "voiced labialized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡ̬ʷ] \
       \is the representation of the \
       \voiced labialized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̬ʷ"
        `shouldBe`
        "voiced labialized epiglottal plosive pulmonic egressive consonant"
    it "should be that: [ʡ̬ʷ] \
       \is the representation of the \
       \voiced labialized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̬ʷ"
        `shouldBe`
        "voiced labialized epiglottal plosive pulmonic egressive consonant"
  describe "voiced palatalized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡ̬ʲ] \
       \is the representation of the \
       \voiced palatalized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̬ʲ"
        `shouldBe`
        "voiced palatalized epiglottal plosive pulmonic egressive consonant"
    it "should be that: [ʡ̬ʲ] \
       \is the representation of the \
       \voiced palatalized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̬ʲ"
        `shouldBe`
        "voiced palatalized epiglottal plosive pulmonic egressive consonant"
  describe "voiced velarized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡ̬ˠ] \
       \is the representation of the \
       \voiced velarized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̬ˠ"
        `shouldBe`
        "voiced velarized epiglottal plosive pulmonic egressive consonant"
    it "should be that: [ʡ̬ˠ] \
       \is the representation of the \
       \voiced velarized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̬ˠ"
        `shouldBe`
        "voiced velarized epiglottal plosive pulmonic egressive consonant"
  describe "voiced pharyngealized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̬ˤ"
        `shouldBe`
        "voiced pharyngealized epiglottal plosive pulmonic egressive consonant"
    it "should be that: [ʡ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̬ˤ"
        `shouldBe`
        "voiced pharyngealized epiglottal plosive pulmonic egressive consonant"
  describe "voiced aspirated epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡ̬ʰ] \
       \is the representation of the \
       \voiced aspirated epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̬ʰ"
        `shouldBe`
        "voiced aspirated epiglottal plosive pulmonic egressive consonant"
  describe "voiced aspirated labialized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized epiglottal plosive pulmonic egressive consonant"
  describe "voiced aspirated palatalized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized epiglottal plosive pulmonic egressive consonant"
  describe "voiced aspirated velarized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized epiglottal plosive pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized epiglottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʡ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized epiglottal plosive pulmonic egressive consonant" $
      describeIPA "ʡ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized epiglottal plosive pulmonic egressive consonant"
  describe "voiceless epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʜ] \
       \is the representation of the \
       \voiceless epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ"
        `shouldBe`
        "voiceless epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̊] \
       \is the representation of the \
       \voiceless epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̊"
        `shouldBe`
        "voiceless epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̥] \
       \is the representation of the \
       \voiceless epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̥"
        `shouldBe`
        "voiceless epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̊] \
       \is the representation of the \
       \voiceless epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̊"
        `shouldBe`
        "voiceless epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̥] \
       \is the representation of the \
       \voiceless epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̥"
        `shouldBe`
        "voiceless epiglottal fricative pulmonic egressive consonant"
  describe "voiceless labialized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʜʷ] \
       \is the representation of the \
       \voiceless labialized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜʷ"
        `shouldBe`
        "voiceless labialized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̊ʷ] \
       \is the representation of the \
       \voiceless labialized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̊ʷ"
        `shouldBe`
        "voiceless labialized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̥ʷ] \
       \is the representation of the \
       \voiceless labialized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̥ʷ"
        `shouldBe`
        "voiceless labialized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̊ʷ] \
       \is the representation of the \
       \voiceless labialized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̊ʷ"
        `shouldBe`
        "voiceless labialized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̥ʷ] \
       \is the representation of the \
       \voiceless labialized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̥ʷ"
        `shouldBe`
        "voiceless labialized epiglottal fricative pulmonic egressive consonant"
  describe "voiceless palatalized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʜʲ] \
       \is the representation of the \
       \voiceless palatalized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜʲ"
        `shouldBe`
        "voiceless palatalized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̊ʲ"
        `shouldBe`
        "voiceless palatalized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̥ʲ"
        `shouldBe`
        "voiceless palatalized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̊ʲ"
        `shouldBe`
        "voiceless palatalized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̥ʲ"
        `shouldBe`
        "voiceless palatalized epiglottal fricative pulmonic egressive consonant"
  describe "voiceless velarized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʜˠ] \
       \is the representation of the \
       \voiceless velarized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜˠ"
        `shouldBe`
        "voiceless velarized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̊ˠ] \
       \is the representation of the \
       \voiceless velarized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̊ˠ"
        `shouldBe`
        "voiceless velarized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̥ˠ] \
       \is the representation of the \
       \voiceless velarized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̥ˠ"
        `shouldBe`
        "voiceless velarized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̊ˠ] \
       \is the representation of the \
       \voiceless velarized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̊ˠ"
        `shouldBe`
        "voiceless velarized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̥ˠ] \
       \is the representation of the \
       \voiceless velarized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̥ˠ"
        `shouldBe`
        "voiceless velarized epiglottal fricative pulmonic egressive consonant"
  describe "voiceless pharyngealized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʜˤ] \
       \is the representation of the \
       \voiceless pharyngealized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜˤ"
        `shouldBe`
        "voiceless pharyngealized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized epiglottal fricative pulmonic egressive consonant"
  describe "voiceless aspirated epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʜʰ] \
       \is the representation of the \
       \voiceless aspirated epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜʰ"
        `shouldBe`
        "voiceless aspirated epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̥ʰ"
        `shouldBe`
        "voiceless aspirated epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̊ʰ"
        `shouldBe`
        "voiceless aspirated epiglottal fricative pulmonic egressive consonant"
  describe "voiceless aspirated labialized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʜʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜʰʷ"
        `shouldBe`
        "voiceless aspirated labialized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized epiglottal fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʜʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized epiglottal fricative pulmonic egressive consonant"
  describe "voiceless aspirated velarized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʜʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜʰˠ"
        `shouldBe`
        "voiceless aspirated velarized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized epiglottal fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʜʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized epiglottal fricative pulmonic egressive consonant"
  describe "voiced epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʢ] \
       \is the representation of the \
       \voiced epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ"
        `shouldBe`
        "voiced epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̬] \
       \is the representation of the \
       \voiced epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̬"
        `shouldBe`
        "voiced epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̬] \
       \is the representation of the \
       \voiced epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̬"
        `shouldBe`
        "voiced epiglottal fricative pulmonic egressive consonant"
  describe "voiced labialized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʢʷ] \
       \is the representation of the \
       \voiced labialized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢʷ"
        `shouldBe`
        "voiced labialized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̬ʷ] \
       \is the representation of the \
       \voiced labialized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̬ʷ"
        `shouldBe`
        "voiced labialized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̬ʷ] \
       \is the representation of the \
       \voiced labialized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̬ʷ"
        `shouldBe`
        "voiced labialized epiglottal fricative pulmonic egressive consonant"
  describe "voiced palatalized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʢʲ] \
       \is the representation of the \
       \voiced palatalized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢʲ"
        `shouldBe`
        "voiced palatalized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̬ʲ] \
       \is the representation of the \
       \voiced palatalized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̬ʲ"
        `shouldBe`
        "voiced palatalized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̬ʲ] \
       \is the representation of the \
       \voiced palatalized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̬ʲ"
        `shouldBe`
        "voiced palatalized epiglottal fricative pulmonic egressive consonant"
  describe "voiced velarized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʢˠ] \
       \is the representation of the \
       \voiced velarized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢˠ"
        `shouldBe`
        "voiced velarized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̬ˠ] \
       \is the representation of the \
       \voiced velarized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̬ˠ"
        `shouldBe`
        "voiced velarized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̬ˠ] \
       \is the representation of the \
       \voiced velarized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̬ˠ"
        `shouldBe`
        "voiced velarized epiglottal fricative pulmonic egressive consonant"
  describe "voiced pharyngealized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʢˤ] \
       \is the representation of the \
       \voiced pharyngealized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢˤ"
        `shouldBe`
        "voiced pharyngealized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̬ˤ"
        `shouldBe`
        "voiced pharyngealized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̬ˤ"
        `shouldBe`
        "voiced pharyngealized epiglottal fricative pulmonic egressive consonant"
  describe "voiced aspirated epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʢʰ] \
       \is the representation of the \
       \voiced aspirated epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢʰ"
        `shouldBe`
        "voiced aspirated epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̬ʰ] \
       \is the representation of the \
       \voiced aspirated epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̬ʰ"
        `shouldBe`
        "voiced aspirated epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̬ʰ] \
       \is the representation of the \
       \voiced aspirated epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̬ʰ"
        `shouldBe`
        "voiced aspirated epiglottal fricative pulmonic egressive consonant"
  describe "voiced aspirated labialized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʢʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢʰʷ"
        `shouldBe`
        "voiced aspirated labialized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized epiglottal fricative pulmonic egressive consonant"
  describe "voiced aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʢʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢʰʲ"
        `shouldBe`
        "voiced aspirated palatalized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized epiglottal fricative pulmonic egressive consonant"
  describe "voiced aspirated velarized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʢʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢʰˠ"
        `shouldBe`
        "voiced aspirated velarized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized epiglottal fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʢʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʢ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʢ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized epiglottal fricative pulmonic egressive consonant"
    it "should be that: [ʜ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
      describeIPA "ʜ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized epiglottal fricative pulmonic egressive consonant"
  describe "voiceless alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɕ] \
       \is the representation of the \
       \voiceless alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ"
        `shouldBe`
        "voiceless alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̊] \
       \is the representation of the \
       \voiceless alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̊"
        `shouldBe`
        "voiceless alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̥] \
       \is the representation of the \
       \voiceless alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̥"
        `shouldBe`
        "voiceless alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̊] \
       \is the representation of the \
       \voiceless alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̊"
        `shouldBe`
        "voiceless alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̥] \
       \is the representation of the \
       \voiceless alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̥"
        `shouldBe`
        "voiceless alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiceless labialized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɕʷ] \
       \is the representation of the \
       \voiceless labialized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕʷ"
        `shouldBe`
        "voiceless labialized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̊ʷ] \
       \is the representation of the \
       \voiceless labialized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̊ʷ"
        `shouldBe`
        "voiceless labialized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̥ʷ] \
       \is the representation of the \
       \voiceless labialized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̥ʷ"
        `shouldBe`
        "voiceless labialized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̊ʷ] \
       \is the representation of the \
       \voiceless labialized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̊ʷ"
        `shouldBe`
        "voiceless labialized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̥ʷ] \
       \is the representation of the \
       \voiceless labialized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̥ʷ"
        `shouldBe`
        "voiceless labialized alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɕʲ] \
       \is the representation of the \
       \voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕʲ"
        `shouldBe`
        "voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̊ʲ"
        `shouldBe`
        "voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̥ʲ"
        `shouldBe`
        "voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̊ʲ"
        `shouldBe`
        "voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̥ʲ"
        `shouldBe`
        "voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiceless velarized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɕˠ] \
       \is the representation of the \
       \voiceless velarized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕˠ"
        `shouldBe`
        "voiceless velarized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̊ˠ] \
       \is the representation of the \
       \voiceless velarized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̊ˠ"
        `shouldBe`
        "voiceless velarized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̥ˠ] \
       \is the representation of the \
       \voiceless velarized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̥ˠ"
        `shouldBe`
        "voiceless velarized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̊ˠ] \
       \is the representation of the \
       \voiceless velarized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̊ˠ"
        `shouldBe`
        "voiceless velarized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̥ˠ] \
       \is the representation of the \
       \voiceless velarized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̥ˠ"
        `shouldBe`
        "voiceless velarized alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɕˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕˤ"
        `shouldBe`
        "voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiceless aspirated alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɕʰ] \
       \is the representation of the \
       \voiceless aspirated alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕʰ"
        `shouldBe`
        "voiceless aspirated alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̥ʰ"
        `shouldBe`
        "voiceless aspirated alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̊ʰ"
        `shouldBe`
        "voiceless aspirated alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiceless aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɕʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiceless aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɕʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiceless aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɕʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ɕʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiced alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʑ] \
       \is the representation of the \
       \voiced alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ"
        `shouldBe`
        "voiced alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̬] \
       \is the representation of the \
       \voiced alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̬"
        `shouldBe`
        "voiced alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̬] \
       \is the representation of the \
       \voiced alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̬"
        `shouldBe`
        "voiced alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiced labialized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʑʷ] \
       \is the representation of the \
       \voiced labialized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑʷ"
        `shouldBe`
        "voiced labialized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̬ʷ] \
       \is the representation of the \
       \voiced labialized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̬ʷ"
        `shouldBe`
        "voiced labialized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̬ʷ] \
       \is the representation of the \
       \voiced labialized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̬ʷ"
        `shouldBe`
        "voiced labialized alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiced palatalized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʑʲ] \
       \is the representation of the \
       \voiced palatalized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑʲ"
        `shouldBe`
        "voiced palatalized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̬ʲ] \
       \is the representation of the \
       \voiced palatalized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̬ʲ"
        `shouldBe`
        "voiced palatalized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̬ʲ] \
       \is the representation of the \
       \voiced palatalized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̬ʲ"
        `shouldBe`
        "voiced palatalized alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiced velarized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʑˠ] \
       \is the representation of the \
       \voiced velarized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑˠ"
        `shouldBe`
        "voiced velarized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̬ˠ] \
       \is the representation of the \
       \voiced velarized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̬ˠ"
        `shouldBe`
        "voiced velarized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̬ˠ] \
       \is the representation of the \
       \voiced velarized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̬ˠ"
        `shouldBe`
        "voiced velarized alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiced pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʑˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑˤ"
        `shouldBe`
        "voiced pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̬ˤ"
        `shouldBe`
        "voiced pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̬ˤ"
        `shouldBe`
        "voiced pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiced aspirated alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʑʰ] \
       \is the representation of the \
       \voiced aspirated alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑʰ"
        `shouldBe`
        "voiced aspirated alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̬ʰ] \
       \is the representation of the \
       \voiced aspirated alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̬ʰ"
        `shouldBe`
        "voiced aspirated alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̬ʰ] \
       \is the representation of the \
       \voiced aspirated alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̬ʰ"
        `shouldBe`
        "voiced aspirated alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiced aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʑʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiced aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʑʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiced aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʑʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolo-palatal fricative pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
    do
    it "should be that: [ʑʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ʑ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ʑ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
    it "should be that: [ɕ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
      describeIPA "ɕ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
