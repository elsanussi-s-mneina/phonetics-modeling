module Spec (main) where

import Prelude (($), (++), IO, length, Maybe(Just), Bool(True, False))
import Test.Hspec (describe, hspec, it, shouldBe, Spec)
    
import Lib

main :: IO ()
main = do
  hspec glideSpec
  hspec analyzeIPASpec
  hspec spirantizedIPASpec
  hspec devoicedIPASpec


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
      it ("should return that [t] is voiceless.") $
            vocalFolds (analyzeIPA "t") `shouldBe` Voiceless
      it ("should return that [t] is alveolar.") $
            place (analyzeIPA "t") `shouldBe` Alveolar
      it ("should return that [t] is a plosive.") $
            manner (analyzeIPA "t") `shouldBe` Plosive
      it ("should return that [t] is plumonic egressive.") $
            airstream (analyzeIPA "t") `shouldBe` PulmonicEgressive
      it ("should return that [d] is voiced.") $
            vocalFolds (analyzeIPA "d") `shouldBe` Voiced
      it ("should return that [d] is alveolar") $
            place (analyzeIPA "d") `shouldBe` Alveolar
      it ("should return that [d] is plosive") $
            manner (analyzeIPA "d") `shouldBe` Plosive
      it ("should return that [d] is pulmonic egressive") $
            airstream (analyzeIPA "d") `shouldBe` PulmonicEgressive
      it ("should return that [ʈ] is voiceless") $
            vocalFolds (analyzeIPA "ʈ") `shouldBe` Voiceless
      it ("should return that [ʈ] is retroflex") $
            place (analyzeIPA "ʈ") `shouldBe` Retroflex
      it ("should return that [ʈ] is a plosive") $
            manner (analyzeIPA "ʈ") `shouldBe` Plosive
      it ("should return that [ʈ] is pulmonic egressive") $
            airstream (analyzeIPA "ʈ") `shouldBe` PulmonicEgressive
      it ("should return that [ɖ] is voiced") $
            vocalFolds (analyzeIPA "ɖ") `shouldBe` Voiced
      it ("should return that [ɖ] is retroflex") $
            place (analyzeIPA "ɖ") `shouldBe` Retroflex
      it ("should return that [ɖ] is a plosive") $
            manner (analyzeIPA "ɖ") `shouldBe` Plosive
      it ("should return that [ɖ] is pulmonic egressive") $
            airstream (analyzeIPA "ɖ") `shouldBe` PulmonicEgressive


spirantizedIPASpec :: Spec
spirantizedIPASpec = 
    describe "spirantizedIPA" $ do
      it ("should return that [β] is spirantized [b].") $
            spirantizedIPA "b" `shouldBe` "β"
      it ("should return that [ɸ] is spirantized [p].") $
            spirantizedIPA "p" `shouldBe` "ɸ"
      it ("should return that [x] is spirantized [k].") $
            spirantizedIPA "k" `shouldBe` "x"

devoicedIPASpec :: Spec
devoicedIPASpec =
    describe "devoicedIPA" $ do
      it ("should return that [p] is devoiced [b].") $
            devoicedIPA "b" `shouldBe` "p"
      it ("should return that [n̥] is devoiced [n].") $
            devoicedIPA "n" `shouldBe` "n̥"