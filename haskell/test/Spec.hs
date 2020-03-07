module Spec (main) where

import Prelude (($), (++), IO, length, Maybe(Just), Bool(True, False))
import Test.Hspec (describe, hspec, it, shouldBe, Spec)
import PhonemeFeature (isGlide)
import Lib
import InternationalPhoneticAlphabet (analyzeIPA, constructIPA, spirantizedIPA, devoicedIPA)


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
          "with pulmonic egressive airstream mechanism.") $ do
            analyzeIPA "p" `shouldBe` Consonant Voiceless Bilabial Plosive PulmonicEgressive
            constructIPA             (Consonant Voiceless Bilabial Plosive PulmonicEgressive) `shouldBe` "p"
      it ("should be that: [s] is a voiceless alveolar fricative consonant" ++
          "with pulmonic egressive airstream mechanism.") $ do
            analyzeIPA "s" `shouldBe` Consonant Voiceless Alveolar Fricative PulmonicEgressive
            constructIPA             (Consonant Voiceless Alveolar Fricative PulmonicEgressive) `shouldBe` "s"
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
      it ("should be that: [b] is a voiceless bilabial plosive consonant with " ++
          "pulmonic egressive airstream mechanism") $ do
            analyzeIPA "b" `shouldBe` Consonant  Voiced    Bilabial  Plosive PulmonicEgressive
            constructIPA             (Consonant  Voiced    Bilabial  Plosive PulmonicEgressive) `shouldBe` "b"
      it ("should be that: [t] is a voiceless alveloar plosive consonant with " ++
          "pulmonic egressive airstream mechanism") $ do
            analyzeIPA "t" `shouldBe` Consonant  Voiceless Alveolar  Plosive PulmonicEgressive
            constructIPA             (Consonant  Voiceless Alveolar  Plosive PulmonicEgressive) `shouldBe` "t"
-- Plosives:
      it ("should be that: [p] is a voiceless bilabial plosive consonant with " ++ 
          "pulmonic egressive airstream mechanism") $ 
           do
           analyzeIPA "p"  `shouldBe` Consonant  Voiceless Bilabial  Plosive PulmonicEgressive
           constructIPA (Consonant Voiceless Bilabial Plosive PulmonicEgressive) `shouldBe` "p"
      it ("should be that: [t] is a voiceless alveolar plosive with " ++ 
          "pulmonic egressive airstream mechanism") $ do 
          analyzeIPA "t"  `shouldBe` Consonant  Voiceless Alveolar  Plosive PulmonicEgressive
          constructIPA              (Consonant  Voiceless Alveolar  Plosive PulmonicEgressive) `shouldBe` "t"
      it ("should be that: [d] is a voiced alveolar plosive with " ++ 
          "pulmonic egressive airstream mechanism") $ do
          analyzeIPA "d"  `shouldBe` Consonant  Voiced    Alveolar  Plosive PulmonicEgressive
          constructIPA              (Consonant  Voiced    Alveolar  Plosive PulmonicEgressive) `shouldBe` "d"
      it ("should be that: [ʈ] is a voiceless retroflex plosive with " ++ 
          "pulmonic egressive airstream mechanism") $ do
          analyzeIPA "ʈ"  `shouldBe` Consonant  Voiceless Retroflex Plosive PulmonicEgressive
          constructIPA              (Consonant  Voiceless Retroflex Plosive PulmonicEgressive) `shouldBe` "ʈ"
      it ("should be that: [ɖ] is a voiced retroflex plosive with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɖ"  `shouldBe` Consonant  Voiced    Retroflex Plosive PulmonicEgressive
      it ("should be that: [c] is a voiceless palatal plosive with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "c"  `shouldBe` Consonant  Voiceless Palatal   Plosive PulmonicEgressive
      it ("should be that: [ɟ] is a voiced palatal plosive with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɟ"  `shouldBe` Consonant  Voiced    Palatal   Plosive PulmonicEgressive
      it ("should be that: [k] is a voiceless velar plosive with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "k"  `shouldBe` Consonant  Voiceless Velar     Plosive PulmonicEgressive
      it ("should be that: [g] is a voiced velar plosive with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "g"  `shouldBe` Consonant  Voiced    Velar     Plosive PulmonicEgressive
      it ("should be that: [q] is a voiceless uvular plosive with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "q"  `shouldBe` Consonant  Voiceless Uvular    Plosive PulmonicEgressive
      it ("should be that: [ɢ] is a voiced uvular plosive with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɢ"  `shouldBe` Consonant  Voiced    Uvular    Plosive PulmonicEgressive
      it ("should be that: [ʔ] is a voiceless glottal plosive with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ʔ"  `shouldBe` Consonant  Voiceless Glottal   Plosive PulmonicEgressive
     -- Nasals:
      it ("should be that: [m] is a voiced bilabial nasal with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "m"  `shouldBe` Consonant  Voiced Bilabial    Nasal PulmonicEgressive
      it ("should be that: [ɱ] is a voiced labio-dental nasal with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɱ"  `shouldBe` Consonant  Voiced LabioDental Nasal PulmonicEgressive
      it ("should be that: [n] is a voiced alveolar nasal with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "n"  `shouldBe` Consonant  Voiced Alveolar    Nasal PulmonicEgressive
      it ("should be that: [ɳ] is a voiced retroflex nasal with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɳ"  `shouldBe` Consonant  Voiced Retroflex   Nasal PulmonicEgressive
      it ("should be that: [ɲ] is a voiced palatal nasal with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɲ"  `shouldBe` Consonant  Voiced Palatal     Nasal PulmonicEgressive
      it ("should be that: [ŋ] is a voiced velar nasal with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ŋ"  `shouldBe` Consonant  Voiced Velar       Nasal PulmonicEgressive
      it ("should be that: [ɴ] is a voiced uvular nasal with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɴ"  `shouldBe` Consonant  Voiced Uvular      Nasal PulmonicEgressive
     -- Trills:
      it ("should be that: [ʙ] is a voiced bilabial trill with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ʙ"  `shouldBe` Consonant  Voiced Bilabial Trill PulmonicEgressive
      it ("should be that: [r] is a voiced alveolar trill with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "r"  `shouldBe` Consonant  Voiced Alveolar Trill PulmonicEgressive
      it ("should be that: [ʀ] is a voiced uvular trill with " ++ 
          "pulmonic egressive airstream mechanism") $ do
          analyzeIPA "ʀ"  `shouldBe` Consonant  Voiced Uvular   Trill PulmonicEgressive
          constructIPA              (Consonant  Voiced Uvular   Trill PulmonicEgressive) `shouldBe` "ʀ"
     -- Taps or flaps:
      it ("should be that: [ⱱ] is a voiced labio-dental tap or flap with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ⱱ"  `shouldBe` Consonant  Voiced LabioDental TapOrFlap PulmonicEgressive
      it ("should be that: [ɾ] is a voiced alveolar tap or flap with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɾ"  `shouldBe` Consonant  Voiced Alveolar    TapOrFlap PulmonicEgressive
      it ("should be that: [ɽ] is a voiced retroflex tap or flap with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɽ"  `shouldBe` Consonant  Voiced Retroflex   TapOrFlap PulmonicEgressive
      -- Fricatives:
      it ("should be that: [ɸ] is a voiceless bilabial fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɸ"  `shouldBe` Consonant  Voiceless Bilabial     Fricative PulmonicEgressive
      it ("should be that: [β] is a voiced bilabial fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "β"  `shouldBe` Consonant  Voiced    Bilabial     Fricative PulmonicEgressive
      it ("should be that: [f] is a voiceless labio-dental fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "f"  `shouldBe` Consonant  Voiceless LabioDental  Fricative PulmonicEgressive
      it ("should be that: [v] is a voiced labio-dental fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "v"  `shouldBe` Consonant  Voiced    LabioDental  Fricative PulmonicEgressive
      it ("should be that: [θ] is a voiceless dental fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "θ"  `shouldBe` Consonant  Voiceless Dental       Fricative PulmonicEgressive
      it ("should be that: [ð] is a voiced dental fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ð"  `shouldBe` Consonant  Voiced    Dental       Fricative PulmonicEgressive
      it ("should be that: [s] is a voiceless alveolar fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "s"  `shouldBe` Consonant  Voiceless Alveolar     Fricative PulmonicEgressive
      it ("should be that: [z] is a voiced alveolar fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "z"  `shouldBe` Consonant  Voiced    Alveolar     Fricative PulmonicEgressive
      it ("should be that: [ʃ] is a voiceless post-alveolar fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ʃ"  `shouldBe` Consonant  Voiceless PostAlveolar Fricative PulmonicEgressive
      it ("should be that: [ʒ] is a voiced post-alveolar fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ʒ"  `shouldBe` Consonant  Voiced    PostAlveolar Fricative PulmonicEgressive
      it ("should be that: [ʂ] is a voiceless retroflex fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ʂ"  `shouldBe` Consonant  Voiceless Retroflex    Fricative PulmonicEgressive
      it ("should be that: [ʐ] is a voiced retroflex fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ʐ"  `shouldBe` Consonant  Voiced    Retroflex    Fricative PulmonicEgressive
      it ("should be that: [ç] is a voiceless palatal fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ç"  `shouldBe` Consonant  Voiceless Palatal      Fricative PulmonicEgressive
      it ("should be that: [ʝ] is a voiced palatal fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ʝ"  `shouldBe` Consonant  Voiced    Palatal      Fricative PulmonicEgressive
      it ("should be that: [x] is a voiceless velar fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "x"  `shouldBe` Consonant  Voiceless Velar        Fricative PulmonicEgressive
      it ("should be that: [ɣ] is a voiced velar fricative with " ++ 
          "pulmonic egressive airstream mechanism") $ 
          analyzeIPA "ɣ"  `shouldBe` Consonant  Voiced    Velar        Fricative PulmonicEgressive
      it ("should be that: [χ] is a voiceless uvular fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "χ"  `shouldBe` Consonant  Voiceless Uvular       Fricative PulmonicEgressive
      it ("should be that: [ʁ] is a voiced uvular fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ʁ"  `shouldBe` Consonant  Voiced    Uvular       Fricative PulmonicEgressive
      it ("should be that: [ħ] is a voiceless pharyngeal fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ħ"  `shouldBe` Consonant  Voiceless Pharyngeal   Fricative PulmonicEgressive
      it ("should be that: [ʕ] is a voiced pharyngeal fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ʕ"  `shouldBe` Consonant  Voiced    Pharyngeal   Fricative PulmonicEgressive
      it ("should be that: [h] is a voiceless glottal fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "h"  `shouldBe` Consonant  Voiceless Glottal      Fricative PulmonicEgressive
      it ("should be that: [ɦ] is a voiced glottal fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɦ"  `shouldBe` Consonant  Voiced    Glottal      Fricative PulmonicEgressive
      -- Lateral Fricatives:
      it ("should be that: [ɬ] is a voiceless alveolar lateral fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɬ" `shouldBe` Consonant  Voiceless Alveolar LateralFricative PulmonicEgressive
      it ("should be that: [ɮ] is a voiced alveolar lateral fricative with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɮ" `shouldBe` Consonant  Voiced    Alveolar LateralFricative PulmonicEgressive
      -- Approximants:
      it ("should be that: [ʋ] is a voiced labio-dental approximant with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ʋ"  `shouldBe` Consonant  Voiced LabioDental  Approximant PulmonicEgressive
      it ("should be that: [ɹ] is a voiced alveolar approximant with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɹ"  `shouldBe` Consonant  Voiced Alveolar Approximant PulmonicEgressive
      it ("should be that: [ɻ] is a voiced retroflex approximant with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɻ"  `shouldBe` Consonant  Voiced Retroflex    Approximant PulmonicEgressive
      it ("should be that: [j] is a voiced palatal approximant with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "j"  `shouldBe` Consonant  Voiced Palatal      Approximant PulmonicEgressive
      it ("should be that: [ɰ] is a voiced velar approximant with " ++
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɰ"  `shouldBe` Consonant  Voiced Velar        Approximant PulmonicEgressive
      -- Lateral Approximants:
      it ("should be that: [l] is a voiced alveolar lateral approximant with " ++ 
          "pulmonic egressive airstream mechanism") $ 
          analyzeIPA "l"  `shouldBe` Consonant  Voiced Alveolar  LateralApproximant PulmonicEgressive
      it ("should be that: [ɭ] is a voiced retroflex lateral approximant with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ɭ"  `shouldBe` Consonant  Voiced Retroflex LateralApproximant PulmonicEgressive
      it ("should be that: [ʎ] is a voiced palatal lateral approximant with " ++ 
          "pulmonic egressive airstream mechanism") $
          analyzeIPA "ʎ"  `shouldBe` Consonant  Voiced Palatal   LateralApproximant PulmonicEgressive
      it ("should be that: [ʟ] is a voiced velar lateral approximant with " ++ 
          "pulmonic egressive airstream mechanism") $
           analyzeIPA "ʟ"  `shouldBe` Consonant  Voiced Velar     LateralApproximant PulmonicEgressive




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
