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

isVoicelessCounterpartOf :: Text -> Text -> Spec
isVoicelessCounterpartOf unvoicedPhoneme voicedPhoneme =
  describe "voicing and devoicing a phoneme" $ do
    it ("should be that: [" ++ (toString unvoicedPhoneme) ++ "] voiced is [" ++ (toString voicedPhoneme) ++ "]") $
      voicedIPA unvoicedPhoneme `shouldBe` voicedPhoneme
    it ("should be that: [" ++ (toString voicedPhoneme) ++ "] devoiced is [" ++ (toString unvoicedPhoneme) ++ "]") $
      devoicedIPA voicedPhoneme `shouldBe` unvoicedPhoneme



voicingSpec :: Spec
voicingSpec = do
  describe "voicing and devoicing a phoneme (usual cases)" $ do
    it "should be that: [t] voiced is [d]" $
      voicedIPA "t" `shouldBe` "d"
    it "should be that: [d] devoiced is [t]" $
      devoicedIPA "d" `shouldBe` "t"
    it "should be that: [s] voiced is [z]" $
      voicedIPA "s" `shouldBe` "z"
    it "should be that: [z] devoiced is [s]" $
      devoicedIPA "z" `shouldBe` "s"
    it "should be that: [p] voiced is [b]" $
      voicedIPA "p" `shouldBe` "b"
    it "should be that: [b] devoiced is [p]" $
      devoicedIPA "b" `shouldBe` "p"
    it "should be that: [ʈ] voiced is [ɖ]" $
      voicedIPA "ʈ" `shouldBe` "ɖ"
    it "should be that: [ɖ] devoiced is [ʈ]" $
      devoicedIPA "ɖ" `shouldBe` "ʈ"
    it "should be that: [c] voiced is [ɟ]" $
      voicedIPA "c" `shouldBe` "ɟ"
    it "should be that: [ɟ] devoiced is [c]" $
      devoicedIPA "ɟ" `shouldBe` "c"
    it "should be that: [k] voiced is [g]" $
      voicedIPA "k" `shouldBe` "g"
    it "should be that: [g] devoiced is [k]" $
      devoicedIPA "g" `shouldBe` "k"
    it "should be that: [q] voiced is [ɢ]" $
      voicedIPA "q" `shouldBe` "ɢ"
    it "should be that: [ɢ] devoiced is [q]" $
      devoicedIPA "ɢ" `shouldBe` "q"
    it "should be that: [ɸ] voiced is [β]" $
      voicedIPA "ɸ" `shouldBe` "β"
    it "should be that: [β] devoiced is [ɸ]" $
      devoicedIPA "β" `shouldBe` "ɸ"
    it "should be that: [f] voiced is [v]" $
      voicedIPA "f" `shouldBe` "v"
    it "should be that: [v] devoiced is [f]" $
      devoicedIPA "v" `shouldBe` "f"
    it "should be that: [θ] voiced is [ð]" $
      voicedIPA "θ" `shouldBe` "ð"
    it "should be that: [ð] devoiced is [θ]" $
      devoicedIPA "ð" `shouldBe` "θ"
    it "should be that: [s] voiced is [z]" $
      voicedIPA "s" `shouldBe` "z"
    it "should be that: [z] devoiced is [s]" $
      devoicedIPA "z" `shouldBe` "s"
    it "should be that: [ʃ] voiced is [ʒ]" $
      voicedIPA "ʃ" `shouldBe` "ʒ"
    it "should be that: [ʒ] devoiced is [ʃ]" $
      devoicedIPA "ʒ" `shouldBe` "ʃ"
    it "should be that: [ʂ] voiced is [ʐ]" $
      voicedIPA "ʂ" `shouldBe` "ʐ"
    it "should be that: [ʐ] devoiced is [ʂ]" $
      devoicedIPA "ʐ" `shouldBe` "ʂ"
    it "should be that: [ç] voiced is [ʝ]" $
      voicedIPA "ç" `shouldBe` "ʝ"
    it "should be that: [ʝ] devoiced is [ç]" $
      devoicedIPA "ʝ" `shouldBe` "ç"
    it "should be that: [ɕ] voiced is [ʑ]" $
      voicedIPA "ɕ" `shouldBe` "ʑ"
    it "should be that: [ʑ] devoiced is [ɕ]" $
      devoicedIPA "ʑ" `shouldBe` "ɕ"
    it "should be that: [x] voiced is [ɣ]" $
      voicedIPA "x" `shouldBe` "ɣ"
    it "should be that: [ɣ] devoiced is [x]" $
      devoicedIPA "ɣ" `shouldBe` "x"
    "χ" `isVoicelessCounterpartOf` "ʁ"
    "ħ" `isVoicelessCounterpartOf` "ʕ"
    "h" `isVoicelessCounterpartOf` "ɦ"
    "ɬ" `isVoicelessCounterpartOf` "ɮ"

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
