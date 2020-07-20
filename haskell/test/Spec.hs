{-# LANGUAGE OverloadedStrings #-}
module Spec(main) where

import           Data.Maybe    (fromJust)
import           Test.Hspec    (Spec, describe, hspec, it, shouldBe)
import           Lib_Functions (analyzeIPA, isGlide, ipaTextToPhonetListReport, voicedIPA, devoicedIPA, analyzeIPAToSPE)
import           Relude

main = do
  hspec glideSpec
  hspec ipaTextToPhonetListReportSpec
  hspec voicingSpec
  hspec analyzeIPAToSPESpec

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

isVoicelessCounterpartOf :: Text -> Text -> Spec
isVoicelessCounterpartOf unvoicedPhoneme voicedPhoneme =
  describe "voicing and devoicing a phoneme" $ do
    it ("should be that: [" ++ toString unvoicedPhoneme ++ "] voiced is [" ++ toString voicedPhoneme ++ "]") $
      voicedIPA unvoicedPhoneme `shouldBe` voicedPhoneme
    it ("should be that: [" ++ toString voicedPhoneme ++ "] devoiced is [" ++ toString unvoicedPhoneme ++ "]") $
      devoicedIPA voicedPhoneme `shouldBe` unvoicedPhoneme



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
