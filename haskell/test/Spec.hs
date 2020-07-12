{-# LANGUAGE OverloadedStrings #-}
module Spec(main) where

import           Data.Maybe    (fromJust)
import           Test.Hspec    (Spec, describe, hspec, it, shouldBe)

import           Lib_Functions (analyzeIPA, isGlide, ipaTextToPhonetListReport, voicedIPA, devoicedIPA)


main = do
  hspec glideSpec
  hspec ipaTextToPhonetListReportSpec
  hspec voicingSpec

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
        ipaTextToPhonetListReport "kc" `shouldBe` "/k/ voiceless velar plosive pulmonic egressive consonant\n/c/ voiceless palatal plosive pulmonic egressive consonant\n"


voicingSpec :: Spec
voicingSpec =
  describe "voicing and devoicing a phoneme" $ do
    it "should be that: [t] voiced is [d]" $
      voicedIPA "t" `shouldBe` "d"
    it "should be that: [d] devoiced is [t]" $
      devoicedIPA "d" `shouldBe` "t"
    it "should be that: [s] voiced is [z]" $
      voicedIPA "s" `shouldBe` "z"
    it "should be that: [z] devoiced is [s]" $
      devoicedIPA "z" `shouldBe` "s"

