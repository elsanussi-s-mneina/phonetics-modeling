module Spec (main) where

import Prelude (($), (++), IO, length, Maybe(Just), Bool(True, False))
import Test.Hspec (describe, hspec, it, shouldBe, Spec)
import PhonemeFeature
import Lib

main :: IO ()
main = do
  hspec glideSpec
  hspec analyzeIPASpec
  hspec spirantizedIPASpec
  hspec devoicedIPASpec


glideSpec :: Spec
glideSpec =
  describe "recognizing a glide" $ do
    it "should be that: [j] the voiced palatal approximant is a glide." $
          isGlide (analyzeIPA "j") `shouldBe` True
    it "should be that: [ʝ] the voiced palatal fricative is not a glide." $
          isGlide (analyzeIPA "ʝ") `shouldBe` False
    it "should be that: [w] is a glide." $
          isGlide (analyzeIPA "w") `shouldBe` True
    it "should be that: [c] is not a glide." $
          isGlide (analyzeIPA "c") `shouldBe` False
    it "should be that: [ɥ] is a glide." $
          isGlide (analyzeIPA "ɥ") `shouldBe` True


analyzeIPASpec :: Spec
analyzeIPASpec = 
    describe "analyzing place, voicing, manner, and airstream mechanism of sound represented by IPA symbols" $ do
      it ("should be that: [p] is a voiceless bilabial plosive consonant" ++ 
          "with pulmonic egressive airstream mechanism.") $
            analyzeIPA "p" `shouldBe` Consonant Voiceless Bilabial Plosive PulmonicEgressive
      it ("should be that: [s] is a voiceless alveolar fricative consonant" ++
          "with pulmonic egressive airstream mechanism.") $
            analyzeIPA "s" `shouldBe` Consonant Voiceless Alveolar Fricative PulmonicEgressive
      it ("should be that: [b] is voiced.") $
            vocalFolds (analyzeIPA "b") `shouldBe` Voiced
      it ("should be that: [b] is bilabial.") $
            place (analyzeIPA "b") `shouldBe` Bilabial
      it ("should be that: [b] is a plosive.") $
            manner (analyzeIPA "b") `shouldBe` Plosive
      it ("should be that: [b] is pulmonic egressive.") $
            airstream (analyzeIPA "b") `shouldBe` PulmonicEgressive
      it ("should be that: [t] is voiceless.") $
            vocalFolds (analyzeIPA "t") `shouldBe` Voiceless
      it ("should be that: [t] is alveolar.") $
            place (analyzeIPA "t") `shouldBe` Alveolar
      it ("should be that: [t] is a plosive.") $
            manner (analyzeIPA "t") `shouldBe` Plosive
      it ("should be that: [t] is plumonic egressive.") $
            airstream (analyzeIPA "t") `shouldBe` PulmonicEgressive
      it ("should be that: [d] is voiced.") $
            vocalFolds (analyzeIPA "d") `shouldBe` Voiced
      it ("should be that: [d] is alveolar.") $
            place (analyzeIPA "d") `shouldBe` Alveolar
      it ("should be that: [d] is plosive.") $
            manner (analyzeIPA "d") `shouldBe` Plosive
      it ("should be that: [d] is pulmonic egressive.") $
            airstream (analyzeIPA "d") `shouldBe` PulmonicEgressive
      it ("should be that: [ʈ] is voiceless.") $
            vocalFolds (analyzeIPA "ʈ") `shouldBe` Voiceless
      it ("should be that: [ʈ] is retroflex.") $
            place (analyzeIPA "ʈ") `shouldBe` Retroflex
      it ("should be that: [ʈ] is a plosive.") $
            manner (analyzeIPA "ʈ") `shouldBe` Plosive
      it ("should be that: [ʈ] is pulmonic egressive.") $
            airstream (analyzeIPA "ʈ") `shouldBe` PulmonicEgressive
      it ("should be that: [ɖ] is voiced.") $
            vocalFolds (analyzeIPA "ɖ") `shouldBe` Voiced
      it ("should be that: [ɖ] is retroflex.") $
            place (analyzeIPA "ɖ") `shouldBe` Retroflex
      it ("should be that: [ɖ] is a plosive.") $
            manner (analyzeIPA "ɖ") `shouldBe` Plosive
      it ("should be that: [ɖ] is pulmonic egressive.") $
            airstream (analyzeIPA "ɖ") `shouldBe` PulmonicEgressive


spirantizedIPASpec :: Spec
spirantizedIPASpec = 
    describe "spirantizing a sound (represented in IPA)" $ do
      it ("should be that: [β] is spirantized [b].") $
            spirantizedIPA "b" `shouldBe` "β"
      it ("should be that: [ɸ] is spirantized [p].") $
            spirantizedIPA "p" `shouldBe` "ɸ"
      it ("should be that: [x] is spirantized [k].") $
            spirantizedIPA "k" `shouldBe` "x"

devoicedIPASpec :: Spec
devoicedIPASpec =
    describe "devoicing a sound (represented in IPA)" $ do
      it ("should be that: [p] is devoiced [b].") $
            devoicedIPA "b" `shouldBe` "p"
      it ("should be that: [n̥] is devoiced [n].") $
            devoicedIPA "n" `shouldBe` "n̥"