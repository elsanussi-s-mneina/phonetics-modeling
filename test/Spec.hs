module Spec (main) where

import Prelude (($), (++), IO, length, Maybe(Just), Bool(True, False))
import Test.Hspec (describe, hspec, it, shouldBe, Spec)
    
import Lib

main :: IO ()
main = do
  hspec glideSpec
  hspec analyzeIPASpec



glideSpec :: Spec
glideSpec =
  describe "isGlide" $ do
    it "should return true when given [j] the voiced palatal approximant" $
          isGlide (analyzeIPA "j") `shouldBe` True
    it "should return false when given [ʝ] the voiced palatal fricative" $
          isGlide (analyzeIPA "ʝ") `shouldBe` False
    it "should return true when given [w]" $
          isGlide (analyzeIPA "w") `shouldBe` True
    it "should return false when given [c]" $
          isGlide (analyzeIPA "c") `shouldBe` False
    it "should return true when given [ɥ]" $
          isGlide (analyzeIPA "ɥ") `shouldBe` True


analyzeIPASpec :: Spec
analyzeIPASpec = 
    describe "analyzeIPA" $ do
      it ("should return that [p] is a voiceless bilabial plosive consonant." ++ 
          "with pulmonic egressive airstream mechanism.") $
            analyzeIPA "p" `shouldBe` Consonant Voiceless Bilabial Plosive PulmonicEgressive
      it ("should return that [s] is a voiceless alveolar fricative consonant." ++
          "with pulmonic egressive airstream mechanism.") $
            analyzeIPA "s" `shouldBe` Consonant Voiceless Alveolar Fricative PulmonicEgressive
      it ("should return that [b] is voiced.") $
            vocalFolds (analyzeIPA "b") `shouldBe` Voiced
      it ("should return that [b] is bilabial.") $
            place (analyzeIPA "b") `shouldBe` Bilabial
      it ("should return that [b] is a plosive.") $
            manner (analyzeIPA "b") `shouldBe` Plosive
      it ("should return that [b] is pulmonic egressive.") $
            airstream (analyzeIPA "b") `shouldBe` PulmonicEgressive