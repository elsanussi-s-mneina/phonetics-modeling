{-# LANGUAGE OverloadedStrings #-}
module Spec(main) where

import           Data.Maybe    (fromJust)
import           Test.Hspec    (Spec, describe, hspec, it, shouldBe)
import           IPA (analyzeIPA, ipaTextToPhonetListReport, voicedIPA, devoicedIPA, analyzeIPAToSPE, describeIPA)
import           PhoneticFeatures (isGlide)
import           Relude

main = do
  hspec glideSpec
  hspec ipaTextToPhonetListReportSpec
  hspec voicingSpec
  hspec analyzeIPAToSPESpec
  hspec secondaryArticulationSpec
  hspec vowelLengthSpec

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
      describeIPA "tʷ" `shouldBe` "voiceless alveolar plosive pulmonic egressive labialized consonant"
    it "case: r labialized" $
      describeIPA "rʷ" `shouldBe` "voiced alveolar trill pulmonic egressive labialized consonant"
  describe "palatalization" $ do
    it "case: t palatalized" $
      describeIPA "tʲ" `shouldBe` "voiceless alveolar plosive pulmonic egressive palatalized consonant"
    it "case: r palatalized" $
      describeIPA "rʲ" `shouldBe` "voiced alveolar trill pulmonic egressive palatalized consonant"
  describe "velarization" $ do
    it "case: t velarized" $
      describeIPA "tˠ" `shouldBe` "voiceless alveolar plosive pulmonic egressive velarized consonant"
    it "case: r velarized" $
      describeIPA "rˠ" `shouldBe` "voiced alveolar trill pulmonic egressive velarized consonant"
  describe "palatalization" $ do
    it "case: t pharyngealized" $
      describeIPA "tˤ" `shouldBe` "voiceless alveolar plosive pulmonic egressive pharyngealized consonant"
    it "case: r pharyngealized" $
      describeIPA "rˤ" `shouldBe` "voiced alveolar trill pulmonic egressive pharyngealized consonant"

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
