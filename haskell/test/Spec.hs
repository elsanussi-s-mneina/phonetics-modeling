module Spec (main) where

import Prelude ((++), IO, length, Bool(True, False), putStrLn, putStr, (==), (&&), String, Maybe(Just, Nothing))
import PhonemeFeature (isGlide, toTextLowFeature, toTextAnteriorFeature,
                       toTextAnteriorFeature, toTextDistributedFeature,
                       toTextStridentFeature, toTextHighFeature, 
                       toTextLowFeature, toTextNasalFeature, 
                       toTextLabialFeature, toTextCoronalFeature, 
                       toTextDorsalFeature, 
                       toTextPharyngealFeature, toTextLaryngealFeature,
                       toTextRoundFeature, toTextATRFeature, toTextBackFeature,
                       PhonemeFeature(NasalFeature, LateralFeature, 
                       DelayedReleaseFeature,
                       SpreadGlottisFeature, ConstrictedGlottisFeature,
                       LabialFeature, CoronalFeature, DorsalFeature,
                       PharyngealFeature, LaryngealFeature),
                       isUnary)
import Lib
import InternationalPhoneticAlphabet (analyzeIPA, constructIPA, showIPA, spirantizedIPA, devoicedIPA)
import Tester (printLegend, runTest)
import English (englishPhonetInventory)
sectionDivider = "\n\n"


main :: IO ()
main = do
  printLegend
  glideSpec
  analyzeIPASpec
  devoicedIPASpec
  spirantizedIPASpec
  showEnglishPhonemeInventorySpec
  speFeaturesSpec
  putStrLn "\n\nProgram (Test suite) terminated normally."
  -- We need at least one PutStrLn so that all output makes it to the console before the program terminates.


glideSpec :: IO ()
glideSpec = do
  putStrLn sectionDivider
  putStrLn "recognizing a glide"
  runTest "[j] the voiced palatal approximant is a glide." (isGlide (analyzeIPA "j") == True ) 
  runTest "[ʝ] the voiced palatal fricative is not a glide." (isGlide (analyzeIPA "ʝ") == False) 
  runTest "[w] is a glide." (isGlide (analyzeIPA "w") == True) 
  runTest "[c] is not a glide." (isGlide (analyzeIPA "c") == False) 

analyzeIPASpec :: IO ()
analyzeIPASpec = do
      putStrLn sectionDivider

      putStrLn "analyzing place, voicing, manner, and airstream mechanism of sound represented by IPA symbols"
      runTest ("[p] is a voiceless bilabial plosive consonant" ++ 
                " with pulmonic egressive airstream mechanism.")
                (analyzeIPA "p" == Consonant Voiceless Bilabial Plosive PulmonicEgressive &&
                constructIPA     (Consonant Voiceless Bilabial Plosive PulmonicEgressive) == "p") 
 
      runTest ("[pʰ] is a voiceless aspirated bilabial plosive consonant" ++ 
                " with pulmonic egressive airstream mechanism.")
                (analyzeIPA "pʰ" == Consonant VoicelessAspirated Bilabial Plosive PulmonicEgressive &&
                constructIPA     (Consonant VoicelessAspirated Bilabial Plosive PulmonicEgressive) == "pʰ") 
                
      runTest ("should be that: [s] is a voiceless alveolar fricative consonant" ++
                "with pulmonic egressive airstream mechanism.") 
                (analyzeIPA "s" == Consonant Voiceless Alveolar Fricative PulmonicEgressive &&
                    constructIPA             (Consonant Voiceless Alveolar Fricative PulmonicEgressive) == "s")
      runTest ("should be that: [b] is voiced.") 
            (vocalFolds (analyzeIPA "b") == Voiced)
      runTest ("should be that: [b] is bilabial.") 
            (place (analyzeIPA "b") == Bilabial)
      runTest ("should be that: [b] is a plosive.")
            (manner (analyzeIPA "b") == Plosive)
      runTest ("should be that: [b] is pulmonic egressive.")
            (airstream (analyzeIPA "b") == PulmonicEgressive)
      runTest ("should be that: [t] is voiceless.")
            (vocalFolds (analyzeIPA "t") == Voiceless)
      runTest ("should be that: [t] is alveolar.")
            (place (analyzeIPA "t") == Alveolar)
      runTest ("should be that: [t] is a plosive.")
            (manner (analyzeIPA "t") == Plosive)
      runTest ("should be that: [t] is plumonic egressive.")
            (airstream (analyzeIPA "t") == PulmonicEgressive)
      runTest ("should be that: [d] is voiced.")
            (vocalFolds (analyzeIPA "d") == Voiced)
      runTest ("should be that: [d] is alveolar.")
            (place (analyzeIPA "d") == Alveolar)
      runTest ("should be that: [d] is plosive.")
            (manner (analyzeIPA "d") == Plosive)
      runTest ("should be that: [d] is pulmonic egressive.")
            (airstream (analyzeIPA "d") == PulmonicEgressive)
      runTest ("should be that: [ʈ] is voiceless.")
            (vocalFolds (analyzeIPA "ʈ") == Voiceless)
      runTest ("should be that: [ʈ] is retroflex.")
            (place (analyzeIPA "ʈ") == Retroflex)
      runTest ("should be that: [ʈ] is a plosive.")
            (manner (analyzeIPA "ʈ") == Plosive)
      runTest ("should be that: [ʈ] is pulmonic egressive.")
            (airstream (analyzeIPA "ʈ") == PulmonicEgressive)
      runTest ("should be that: [ɖ] is voiced.")
            (vocalFolds (analyzeIPA "ɖ") == Voiced)
      runTest ("should be that: [ɖ] is retroflex.")
            (place (analyzeIPA "ɖ") == Retroflex)
      runTest ("should be that: [ɖ] is a plosive.")
            (manner (analyzeIPA "ɖ") == Plosive)
      runTest ("should be that: [ɖ] is pulmonic egressive.")
            (airstream (analyzeIPA "ɖ") == PulmonicEgressive)
      runTest ("should be that: [b] is a voiceless bilabial plosive consonant with " ++
          "pulmonic egressive airstream mechanism") 
            (analyzeIPA "b" == Consonant  Voiced    Bilabial  Plosive PulmonicEgressive &&
            constructIPA             (Consonant  Voiced    Bilabial  Plosive PulmonicEgressive) == "b")
      runTest ("should be that: [t] is a voiceless alveloar plosive consonant with " ++
          "pulmonic egressive airstream mechanism") 
            (analyzeIPA "t" == Consonant  Voiceless Alveolar  Plosive PulmonicEgressive && 
            constructIPA             (Consonant  Voiceless Alveolar  Plosive PulmonicEgressive) == "t")
-- Plosives:
      runTest ("should be that: [p] is a voiceless bilabial plosive consonant with " ++ 
          "pulmonic egressive airstream mechanism") 
           (analyzeIPA "p"  == Consonant  Voiceless Bilabial  Plosive PulmonicEgressive &&
           constructIPA (Consonant Voiceless Bilabial Plosive PulmonicEgressive) == "p")
      runTest ("should be that: [t] is a voiceless alveolar plosive with " ++ 
          "pulmonic egressive airstream mechanism")  
          (analyzeIPA "t"  == Consonant  Voiceless Alveolar  Plosive PulmonicEgressive &&
          constructIPA              (Consonant  Voiceless Alveolar  Plosive PulmonicEgressive) == "t")
      runTest ("should be that: [d] is a voiced alveolar plosive with " ++ 
          "pulmonic egressive airstream mechanism") 
          (analyzeIPA "d"  == Consonant  Voiced    Alveolar  Plosive PulmonicEgressive &&
          constructIPA              (Consonant  Voiced    Alveolar  Plosive PulmonicEgressive) == "d")
      runTest ("should be that: [ʈ] is a voiceless retroflex plosive with " ++ 
          "pulmonic egressive airstream mechanism") 
          (analyzeIPA "ʈ"  == Consonant  Voiceless Retroflex Plosive PulmonicEgressive &&
          constructIPA              (Consonant  Voiceless Retroflex Plosive PulmonicEgressive) == "ʈ")
      runTest ("should be that: [ɖ] is a voiced retroflex plosive with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɖ"  == Consonant  Voiced    Retroflex Plosive PulmonicEgressive)
      runTest ("should be that: [c] is a voiceless palatal plosive with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "c"  == Consonant  Voiceless Palatal   Plosive PulmonicEgressive)
      runTest ("should be that: [ɟ] is a voiced palatal plosive with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɟ"  == Consonant  Voiced    Palatal   Plosive PulmonicEgressive)
      runTest ("should be that: [k] is a voiceless velar plosive with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "k"  == Consonant  Voiceless Velar     Plosive PulmonicEgressive)
      runTest ("should be that: [g] is a voiced velar plosive with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "g"  == Consonant  Voiced    Velar     Plosive PulmonicEgressive)
      runTest ("should be that: [q] is a voiceless uvular plosive with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "q"  == Consonant  Voiceless Uvular    Plosive PulmonicEgressive)
      runTest ("should be that: [ɢ] is a voiced uvular plosive with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɢ"  == Consonant  Voiced    Uvular    Plosive PulmonicEgressive)
      runTest ("should be that: [ʔ] is a voiceless glottal plosive with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ʔ"  == Consonant  Voiceless Glottal   Plosive PulmonicEgressive)
     -- Nasals:
      runTest ("should be that: [m] is a voiced bilabial nasal with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "m"  == Consonant  Voiced Bilabial    Nasal PulmonicEgressive)
      runTest ("should be that: [ɱ] is a voiced labio-dental nasal with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɱ"  == Consonant  Voiced LabioDental Nasal PulmonicEgressive)
      runTest ("should be that: [n] is a voiced alveolar nasal with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "n"  == Consonant  Voiced Alveolar    Nasal PulmonicEgressive)
      runTest ("should be that: [ɳ] is a voiced retroflex nasal with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɳ"  == Consonant  Voiced Retroflex   Nasal PulmonicEgressive)
      runTest ("should be that: [ɲ] is a voiced palatal nasal with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɲ"  == Consonant  Voiced Palatal     Nasal PulmonicEgressive)
      runTest ("should be that: [ŋ] is a voiced velar nasal with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ŋ"  == Consonant  Voiced Velar       Nasal PulmonicEgressive)
      runTest ("should be that: [ɴ] is a voiced uvular nasal with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɴ"  == Consonant  Voiced Uvular      Nasal PulmonicEgressive)
     -- Trills:
      runTest ("should be that: [ʙ] is a voiced bilabial trill with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ʙ"  == Consonant  Voiced Bilabial Trill PulmonicEgressive)
      runTest ("should be that: [r] is a voiced alveolar trill with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "r"  == Consonant  Voiced Alveolar Trill PulmonicEgressive)
      runTest ("should be that: [ʀ] is a voiced uvular trill with " ++ 
          "pulmonic egressive airstream mechanism") 
          (analyzeIPA "ʀ"  == Consonant  Voiced Uvular   Trill PulmonicEgressive &&
          constructIPA              (Consonant  Voiced Uvular   Trill PulmonicEgressive) == "ʀ")
     -- Taps or flaps:
      runTest ("should be that: [ⱱ] is a voiced labio-dental tap or flap with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ⱱ"  == Consonant  Voiced LabioDental TapOrFlap PulmonicEgressive)
      runTest ("should be that: [ɾ] is a voiced alveolar tap or flap with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɾ"  == Consonant  Voiced Alveolar    TapOrFlap PulmonicEgressive)
      runTest ("should be that: [ɽ] is a voiced retroflex tap or flap with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɽ"  == Consonant  Voiced Retroflex   TapOrFlap PulmonicEgressive)
      -- Fricatives:
      runTest ("should be that: [ɸ] is a voiceless bilabial fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɸ"  == Consonant  Voiceless Bilabial     Fricative PulmonicEgressive)
      runTest ("should be that: [β] is a voiced bilabial fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "β"  == Consonant  Voiced    Bilabial     Fricative PulmonicEgressive)
      runTest ("should be that: [f] is a voiceless labio-dental fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "f"  == Consonant  Voiceless LabioDental  Fricative PulmonicEgressive)
      runTest ("should be that: [v] is a voiced labio-dental fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "v"  == Consonant  Voiced    LabioDental  Fricative PulmonicEgressive)
      runTest ("should be that: [θ] is a voiceless dental fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "θ"  == Consonant  Voiceless Dental       Fricative PulmonicEgressive)
      runTest ("should be that: [ð] is a voiced dental fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ð"  == Consonant  Voiced    Dental       Fricative PulmonicEgressive)
      runTest ("should be that: [s] is a voiceless alveolar fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "s"  == Consonant  Voiceless Alveolar     Fricative PulmonicEgressive)
      runTest ("should be that: [z] is a voiced alveolar fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "z"  == Consonant  Voiced    Alveolar     Fricative PulmonicEgressive)
      runTest ("should be that: [ʃ] is a voiceless post-alveolar fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ʃ"  == Consonant  Voiceless PostAlveolar Fricative PulmonicEgressive)
      runTest ("should be that: [ʒ] is a voiced post-alveolar fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ʒ"  == Consonant  Voiced    PostAlveolar Fricative PulmonicEgressive)
      runTest ("should be that: [ʂ] is a voiceless retroflex fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ʂ"  == Consonant  Voiceless Retroflex    Fricative PulmonicEgressive)
      runTest ("should be that: [ʐ] is a voiced retroflex fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ʐ"  == Consonant  Voiced    Retroflex    Fricative PulmonicEgressive)
      runTest ("should be that: [ç] is a voiceless palatal fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ç"  == Consonant  Voiceless Palatal      Fricative PulmonicEgressive)
      runTest ("should be that: [ʝ] is a voiced palatal fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ʝ"  == Consonant  Voiced    Palatal      Fricative PulmonicEgressive)
      runTest ("should be that: [x] is a voiceless velar fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "x"  == Consonant  Voiceless Velar        Fricative PulmonicEgressive)
      runTest ("should be that: [ɣ] is a voiced velar fricative with " ++ 
          "pulmonic egressive airstream mechanism") 
          (analyzeIPA "ɣ"  == Consonant  Voiced    Velar        Fricative PulmonicEgressive)
      runTest ("should be that: [χ] is a voiceless uvular fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "χ"  == Consonant  Voiceless Uvular       Fricative PulmonicEgressive)
      runTest ("should be that: [ʁ] is a voiced uvular fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ʁ"  == Consonant  Voiced    Uvular       Fricative PulmonicEgressive)
      runTest ("should be that: [ħ] is a voiceless pharyngeal fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ħ"  == Consonant  Voiceless Pharyngeal   Fricative PulmonicEgressive)
      runTest ("should be that: [ʕ] is a voiced pharyngeal fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ʕ"  == Consonant  Voiced    Pharyngeal   Fricative PulmonicEgressive)
      runTest ("should be that: [h] is a voiceless glottal fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "h"  == Consonant  Voiceless Glottal      Fricative PulmonicEgressive)
      runTest ("should be that: [ɦ] is a voiced glottal fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɦ"  == Consonant  Voiced    Glottal      Fricative PulmonicEgressive)
      -- Lateral Fricatives:
      runTest ("should be that: [ɬ] is a voiceless alveolar lateral fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɬ" == Consonant  Voiceless Alveolar LateralFricative PulmonicEgressive)
      runTest ("should be that: [ɮ] is a voiced alveolar lateral fricative with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɮ" == Consonant  Voiced    Alveolar LateralFricative PulmonicEgressive)
      -- Approximants:
      runTest ("should be that: [ʋ] is a voiced labio-dental approximant with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ʋ"  == Consonant  Voiced LabioDental  Approximant PulmonicEgressive)
      runTest ("should be that: [ɹ] is a voiced alveolar approximant with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɹ"  == Consonant  Voiced Alveolar Approximant PulmonicEgressive)
      runTest ("should be that: [ɻ] is a voiced retroflex approximant with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɻ"  == Consonant  Voiced Retroflex    Approximant PulmonicEgressive)
      runTest ("should be that: [j] is a voiced palatal approximant with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "j"  == Consonant  Voiced Palatal      Approximant PulmonicEgressive)
      runTest ("should be that: [ɰ] is a voiced velar approximant with " ++
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɰ"  == Consonant  Voiced Velar        Approximant PulmonicEgressive)
      -- Lateral Approximants:
      runTest ("should be that: [l] is a voiced alveolar lateral approximant with " ++ 
          "pulmonic egressive airstream mechanism") 
          (analyzeIPA "l"  == Consonant  Voiced Alveolar  LateralApproximant PulmonicEgressive)
      runTest ("should be that: [ɭ] is a voiced retroflex lateral approximant with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ɭ"  == Consonant  Voiced Retroflex LateralApproximant PulmonicEgressive)
      runTest ("should be that: [ʎ] is a voiced palatal lateral approximant with " ++ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPA "ʎ"  == Consonant  Voiced Palatal   LateralApproximant PulmonicEgressive)
      runTest ("should be that: [ʟ] is a voiced velar lateral approximant with " ++ 
          "pulmonic egressive airstream mechanism")
           (analyzeIPA "ʟ"  == Consonant  Voiced Velar     LateralApproximant PulmonicEgressive)




spirantizedIPASpec :: IO()
spirantizedIPASpec = do
      putStrLn sectionDivider
      putStrLn "spirantizing a sound (represented in IPA)" 
      runTest ("should be that: [β] is spirantized [b].")
            (spirantizedIPA "b" == "β")
      runTest ("should be that: [ɸ] is spirantized [p].")
            (spirantizedIPA "p" == "ɸ")
      runTest ("should be that: [x] is spirantized [k].")
            (spirantizedIPA "k" == "x")

devoicedIPASpec :: IO()
devoicedIPASpec = do
      putStrLn sectionDivider
      putStrLn "devoicing a sound (represented in IPA)" 
      runTest ("should be that: [p] is devoiced [b].")
            (devoicedIPA "b" == "p")
      runTest ("should be that: [n̥] is devoiced [n].")
            (devoicedIPA "n" == "n̥")

showEnglishPhonemeInventorySpec :: IO ()
showEnglishPhonemeInventorySpec = do
  putStrLn sectionDivider
  putStrLn "Showing english phoneme inventory"
  runTest "Should show all English phonemes."
    (showIPA englishPhonetInventory == 
     "bpdtgkʔvfðθzsʒʃhd͡ʒt͡ʃmnŋɹ̠jwiuɪʊeoəɛɜʌɔæɐɑɒ")

speFeaturesSpec :: IO ()
speFeaturesSpec = do
  putStrLn sectionDivider
  putStrLn "SPE Features"
  -- Go to page 267 of the textbook.
  runTest "The low feature value is nothing for [β]." 
    (toTextLowFeature (Consonant Voiced Bilabial Fricative PulmonicEgressive) == Nothing)
  runTest "The low feature value for [χ] is [+ low]."
    (toTextLowFeature (Consonant Voiceless Uvular Fricative PulmonicEgressive) == Just "+ low")
  runTest "The low feature value for [ɣ] is [+ low]."
    (toTextLowFeature (Consonant Voiced Uvular Fricative PulmonicEgressive) == Just "+ low")
  runTest "The low feature value for [ħ] is [+ low]."
    (toTextLowFeature (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) == Just "+ low")
  runTest "The low feature value for [ʕ] is [+ low]."
    (toTextLowFeature (Consonant Voiced Pharyngeal Fricative PulmonicEgressive) == Just "+ low")
  runTest "The low feature value for [ʔ] is [+ low]."
    (toTextLowFeature (Consonant Voiceless Glottal Fricative PulmonicEgressive) == Just "+ low")
  runTest "The low feature value is nothing for [k]."
    (toTextLowFeature (Consonant Voiceless Velar Plosive PulmonicEgressive) == Nothing)

  runTest "The anterior feature is nothing for [b]."
     (toTextAnteriorFeature (Consonant Voiceless Bilabial Plosive PulmonicEgressive) == Nothing)
  runTest "The anterior feature is nothing for [f]."
     (toTextAnteriorFeature (Consonant Voiceless LabioDental Fricative PulmonicEgressive) == Nothing)
  runTest "The anterior feature is + for [θ]."
     (toTextAnteriorFeature (Consonant Voiceless Dental Fricative PulmonicEgressive) == Just "+ anterior")
  runTest "The anterior feature is + for [s]."
     (toTextAnteriorFeature (Consonant Voiceless Alveolar Fricative PulmonicEgressive) == Just "+ anterior")
  runTest "The anterior feature is - for [ʃ]."
     (toTextAnteriorFeature (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) == Just "- anterior")
  runTest "The anterior feature is - for [ʂ]."
     (toTextAnteriorFeature (Consonant Voiceless Retroflex Fricative PulmonicEgressive) == Just "- anterior")
  runTest "The anterior feature is - for [ɕ]" 
     (toTextAnteriorFeature (analyzeIPA "ɕ") == Just "- anterior")
  runTest "The anterior feature is - for [ç]" 
     (toTextAnteriorFeature (analyzeIPA "ç") == Just "- anterior") -- TODO: Check this, is it true .ccording to the textbook?
  runTest "The anterior feature is nothing for [x]"
     (toTextAnteriorFeature (Consonant Voiceless Velar Fricative PulmonicEgressive) == Nothing)
  runTest "The anterior feature is nothing for [χ]"
     (toTextAnteriorFeature (Consonant Voiceless Uvular Fricative PulmonicEgressive) == Nothing)
  runTest "The anterior feature is nothing for [ħ]"
     (toTextAnteriorFeature (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) == Nothing)
  runTest "The anterior feature is nothing for [h]"
     (toTextAnteriorFeature (Consonant Voiceless Glottal Fricative PulmonicEgressive) == Nothing)
    

  runTest "The distributed feature is nothing for [b]."
     (toTextDistributedFeature (Consonant Voiceless Bilabial Plosive PulmonicEgressive) == Nothing)
  runTest "The distributed feature is nothing for [f]."
     (toTextDistributedFeature (Consonant Voiceless LabioDental Fricative PulmonicEgressive) == Nothing)
  runTest "The distributed feature is + for [θ]."
     (toTextDistributedFeature (Consonant Voiceless Dental Fricative PulmonicEgressive) == Just "+ distributed")
  runTest "The distributed feature is - for [s]."
     (toTextDistributedFeature (Consonant Voiceless Alveolar Fricative PulmonicEgressive) == Just "- distributed")
  runTest "The distributed feature is + for [ʃ]."
     (toTextDistributedFeature (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) == Just "+ distributed")
  runTest "The distributed feature is - for [ʂ]."
     (toTextDistributedFeature (Consonant Voiceless Retroflex Fricative PulmonicEgressive) == Just "- distributed")
  runTest "The distributed feature is + for [ɕ]" 
     (toTextDistributedFeature (analyzeIPA "ɕ") == Just "+ distributed")
  runTest "The distributed feature is + for [ç]" 
     (toTextDistributedFeature (analyzeIPA "ç") == Just "+ distributed") -- TODO: Check this, is it true according to the textbook?
  runTest "The distributed feature is nothing for [x]"
     (toTextDistributedFeature (Consonant Voiceless Velar Fricative PulmonicEgressive) == Nothing)
  runTest "The distributed feature is nothing for [χ]"
     (toTextDistributedFeature (Consonant Voiceless Uvular Fricative PulmonicEgressive) == Nothing)
  runTest "The distributed feature is nothing for [ħ]"
     (toTextDistributedFeature (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) == Nothing)
  runTest "The distributed feature is nothing for [h]"
     (toTextDistributedFeature (Consonant Voiceless Glottal Fricative PulmonicEgressive) == Nothing)
    

  runTest "The strident feature is - for [ɸ]."
     (toTextStridentFeature (Consonant Voiceless Bilabial Fricative PulmonicEgressive) == Just "- strident")
  runTest "The strident feature is + for [f]."
     (toTextStridentFeature (Consonant Voiceless LabioDental Fricative PulmonicEgressive) == Just "+ strident")
  runTest "The strident feature is - for [θ]."
     (toTextStridentFeature (Consonant Voiceless Dental Fricative PulmonicEgressive) == Just "- strident")
  runTest "The strident feature is + for [s]."
     (toTextStridentFeature (Consonant Voiceless Alveolar Fricative PulmonicEgressive) == Just "+ strident")
  runTest "The strident feature is + for [ʃ]."
     (toTextStridentFeature (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) == Just "+ strident")
  runTest "The strident feature is - for [ʂ]."
     (toTextStridentFeature (Consonant Voiceless Retroflex Fricative PulmonicEgressive) == Just "- strident")
  runTest "The strident feature is - for [ɕ]" 
     (toTextStridentFeature (analyzeIPA "ɕ") == Just "- strident")
  runTest "The strident feature is - for [ç]" 
     (toTextStridentFeature (analyzeIPA "ç") == Just "- strident") -- TODO: Check this, is it true according to the textbook?
  runTest "The strident feature is - for [x]"
     (toTextStridentFeature (Consonant Voiceless Velar Fricative PulmonicEgressive) == Just "- strident")
  runTest "The strident feature is + for [χ]"
     (toTextStridentFeature (Consonant Voiceless Uvular Fricative PulmonicEgressive) == Just "+ strident")
  runTest "The strident feature is - for [ħ]"
     (toTextStridentFeature (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) == Just "- strident")
  runTest "The strident fetaure is - for [h]"
     (toTextStridentFeature (Consonant Voiceless Glottal Fricative PulmonicEgressive) == Just "- strident")
    
  runTest "The high feature is nothing for [ɸ]."
     (toTextHighFeature (Consonant Voiceless Bilabial Fricative PulmonicEgressive) == Nothing)
  runTest "The high feature is nothing for [f]."
     (toTextHighFeature (Consonant Voiceless LabioDental Fricative PulmonicEgressive) == Nothing)
  runTest "The high feature is nothing for [θ]."
     (toTextHighFeature (Consonant Voiceless Dental Fricative PulmonicEgressive) == Nothing)
  runTest "The high feature is nothing for [s]."
     (toTextHighFeature (Consonant Voiceless Alveolar Fricative PulmonicEgressive) == Nothing)
  runTest "The high feature is nothing for [ʃ]."
     (toTextHighFeature (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) == Nothing)
  runTest "The high feature is nothing for [ʂ]."
     (toTextHighFeature (Consonant Voiceless Retroflex Fricative PulmonicEgressive) == Nothing)
  runTest "The high feature is + for [ɕ]" 
     (toTextHighFeature (analyzeIPA "ɕ") == Just "+ high")
  runTest "The high feature is + for [ç]" 
     (toTextHighFeature (analyzeIPA "ç") == Just "+ high") -- TODO: Check this, is it true according to the textbook?
  runTest "The high feature is + for [x]"
     (toTextHighFeature (Consonant Voiceless Velar Fricative PulmonicEgressive) == Just "+ high")
  runTest "The high feature is - for [χ]"
     (toTextHighFeature (Consonant Voiceless Uvular Fricative PulmonicEgressive) == Just "- high")
  runTest "The high feature is nothing for [ħ]"
     (toTextHighFeature (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) == Nothing)
  runTest "The high feature is nothing for [h]"
     (toTextHighFeature (Consonant Voiceless Glottal Fricative PulmonicEgressive) == Nothing)
    
  runTest "The nasal feature is nothing for [ɸ]."
     (toTextNasalFeature (Consonant Voiceless Bilabial Fricative PulmonicEgressive) == Nothing)
  runTest "The nasal feature is nothing for [f]."
     (toTextNasalFeature (Consonant Voiceless LabioDental Fricative PulmonicEgressive) == Nothing)
  runTest "The nasal feature is nothing for [θ]."
     (toTextNasalFeature (Consonant Voiceless Dental Fricative PulmonicEgressive) == Nothing)
  runTest "The nasal feature is nothing for [s]."
     (toTextNasalFeature (Consonant Voiceless Alveolar Fricative PulmonicEgressive) == Nothing)
  runTest "The nasal feature is nothing for [ʃ]."
     (toTextNasalFeature (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) == Nothing)
  runTest "The nasal feature is nothing for [ʂ]."
     (toTextNasalFeature (Consonant Voiceless Retroflex Fricative PulmonicEgressive) == Nothing)
  runTest "The nasal feature is nothing for [ɕ]" 
     (toTextNasalFeature (analyzeIPA "ɕ") == Nothing)
  runTest "The nasal feature is nothing for [ç]" 
     (toTextNasalFeature (analyzeIPA "ç") == Nothing)
  runTest "The nasal feature is nothing for [x]"
     (toTextNasalFeature (Consonant Voiceless Velar Fricative PulmonicEgressive) == Nothing)
  runTest "The nasal feature is nothing for [χ]"
     (toTextNasalFeature (Consonant Voiceless Uvular Fricative PulmonicEgressive) == Nothing)
  runTest "The nasal feature is nothing for [ħ]"
     (toTextNasalFeature (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) == Nothing)
  runTest "The nasal feature is nothing for [h]"
     (toTextNasalFeature (Consonant Voiceless Glottal Fricative PulmonicEgressive) == Nothing)
  runTest "The nasal feature is present for [m]"
    (toTextNasalFeature (Consonant Voiced Bilabial Nasal PulmonicEgressive) == Just "nasal")
  runTest "The nasal feature is present for [ɱ]"
    (toTextNasalFeature (Consonant Voiced LabioDental Nasal PulmonicEgressive) == Just "nasal")
  runTest "The nasal feature is present for [n]"
    (toTextNasalFeature (Consonant Voiced Alveolar Nasal PulmonicEgressive) == Just "nasal")
  runTest "The nasal feature is present for [ɳ]"
    (toTextNasalFeature (Consonant Voiced Retroflex Nasal PulmonicEgressive) == Just "nasal")
  runTest "The nasal feature is present for [ɲ]"
    (toTextNasalFeature (Consonant Voiced Palatal Nasal PulmonicEgressive) == Just "nasal")
  runTest "The nasal feature is present for [ŋ]"
    (toTextNasalFeature (Consonant Voiced Velar Nasal PulmonicEgressive) == Just "nasal")
  runTest "The nasal feature is present for [ɴ]"
    (toTextNasalFeature (Consonant Voiced Uvular Nasal PulmonicEgressive) == Just "nasal")

  runTest "The labial feature is present for [ɸ]."
     (toTextLabialFeature (Consonant Voiceless Bilabial Fricative PulmonicEgressive) == Just "labial")
  runTest "The labial feature is present for [f]."
     (toTextLabialFeature (Consonant Voiceless LabioDental Fricative PulmonicEgressive) == Just "labial")
  runTest "The labial feature is nothing for [θ]."
     (toTextLabialFeature (Consonant Voiceless Dental Fricative PulmonicEgressive) == Nothing)
  runTest "The labial feature is nothing for [s]."
     (toTextLabialFeature (Consonant Voiceless Alveolar Fricative PulmonicEgressive) == Nothing)
  runTest "The labial feature is nothing for [ʃ]."
     (toTextLabialFeature (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) == Nothing)
  runTest "The labial feature is nothing for [ʂ]."
     (toTextLabialFeature (Consonant Voiceless Retroflex Fricative PulmonicEgressive) == Nothing)
  runTest "The labial feature is nothing for [ɕ]" 
     (toTextLabialFeature (analyzeIPA "ɕ") == Nothing)
  runTest "The labial feature is nothing for [ç]" 
     (toTextLabialFeature (analyzeIPA "ç") == Nothing)
  runTest "The labial feature is nothing for [x]"
     (toTextLabialFeature (Consonant Voiceless Velar Fricative PulmonicEgressive) == Nothing)
  runTest "The labial feature is nothing for [χ]"
     (toTextLabialFeature (Consonant Voiceless Uvular Fricative PulmonicEgressive) == Nothing)
  runTest "The labial feature is nothing for [ħ]"
     (toTextLabialFeature (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) == Nothing)
  runTest "The labial feature is nothing for [h]"
     (toTextLabialFeature (Consonant Voiceless Glottal Fricative PulmonicEgressive) == Nothing)
  runTest "The labial feature is present for [m]"
    (toTextLabialFeature (Consonant Voiced Bilabial Nasal PulmonicEgressive) == Just "labial")
  runTest "The labial feature is present for [ɱ]"
    (toTextLabialFeature (Consonant Voiced LabioDental Nasal PulmonicEgressive) == Just "labial")
  runTest "The labial feature is nothing for [n]"
    (toTextLabialFeature (Consonant Voiced Alveolar Nasal PulmonicEgressive) == Nothing)
  runTest "The labial feature is nothing for [ɳ]"
    (toTextLabialFeature (Consonant Voiced Retroflex Nasal PulmonicEgressive) == Nothing)
  runTest "The labial feature is nothing for [ɲ]"
    (toTextLabialFeature (Consonant Voiced Palatal Nasal PulmonicEgressive) == Nothing)
  runTest "The labial feature is nothing for [ŋ]"
    (toTextLabialFeature (Consonant Voiced Velar Nasal PulmonicEgressive) == Nothing)
  runTest "The labial feature is nothing for [ɴ]"
    (toTextLabialFeature (Consonant Voiced Uvular Nasal PulmonicEgressive) == Nothing)


  runTest "The coronal feature is nothing for [ɸ]."
     (toTextCoronalFeature (Consonant Voiceless Bilabial Fricative PulmonicEgressive) == Nothing)
  runTest "The coronal feature is nothing for [f]."
     (toTextCoronalFeature (Consonant Voiceless LabioDental Fricative PulmonicEgressive) == Nothing)
  runTest "The coronal feature is present for [θ]."
     (toTextCoronalFeature (Consonant Voiceless Dental Fricative PulmonicEgressive) == Just "coronal")
  runTest "The coronal feature is present for [s]."
     (toTextCoronalFeature (Consonant Voiceless Alveolar Fricative PulmonicEgressive) == Just "coronal")
  runTest "The coronal feature is present for [ʃ]."
     (toTextCoronalFeature (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) == Just "coronal")
  runTest "The coronal feature is present for [ʂ]."
     (toTextCoronalFeature (Consonant Voiceless Retroflex Fricative PulmonicEgressive) == Just "coronal")
  runTest "The coronal feature is present for [ɕ]" 
     (toTextCoronalFeature (analyzeIPA "ɕ") == Just "coronal")
  runTest "The coronal feature is present for [ç]" 
     (toTextCoronalFeature (analyzeIPA "ç") == Just "coronal")
  runTest "The coronal feature is nothing for [x]"
     (toTextCoronalFeature (Consonant Voiceless Velar Fricative PulmonicEgressive) == Nothing)
  runTest "The coronal feature is nothing for [χ]"
     (toTextCoronalFeature (Consonant Voiceless Uvular Fricative PulmonicEgressive) == Nothing)
  runTest "The coronal feature is nothing for [ħ]"
     (toTextCoronalFeature (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) == Nothing)
  runTest "The coronal feature is nothing for [h]"
     (toTextCoronalFeature (Consonant Voiceless Glottal Fricative PulmonicEgressive) == Nothing)
  runTest "The coronal feature is nothing for [m]"
    (toTextCoronalFeature (Consonant Voiced Bilabial Nasal PulmonicEgressive) == Nothing)
  runTest "The coronal feature is nothing for [ɱ]"
    (toTextCoronalFeature (Consonant Voiced LabioDental Nasal PulmonicEgressive) == Nothing)
  runTest "The coronal feature is present for [n]"
    (toTextCoronalFeature (Consonant Voiced Alveolar Nasal PulmonicEgressive) == Just "coronal")
  runTest "The coronal feature is present for [ɳ]"
    (toTextCoronalFeature (Consonant Voiced Retroflex Nasal PulmonicEgressive) == Just "coronal")
  runTest "The coronal feature is present for [ɲ]"
    (toTextCoronalFeature (Consonant Voiced Palatal Nasal PulmonicEgressive) == Just "coronal")
  runTest "The coronal feature is nothing for [ŋ]"
    (toTextCoronalFeature (Consonant Voiced Velar Nasal PulmonicEgressive) == Nothing)
  runTest "The coronal feature is nothing for [ɴ]"
    (toTextCoronalFeature (Consonant Voiced Uvular Nasal PulmonicEgressive) == Nothing)

  runTest "The dorsal feature is nothing for [ɸ]."
     (toTextDorsalFeature (Consonant Voiceless Bilabial Fricative PulmonicEgressive) == Nothing)
  runTest "The dorsal feature is nothing for [f]."
     (toTextDorsalFeature (Consonant Voiceless LabioDental Fricative PulmonicEgressive) == Nothing)
  runTest "The dorsal feature is nothing for [θ]."
     (toTextDorsalFeature (Consonant Voiceless Dental Fricative PulmonicEgressive) == Nothing)
  runTest "The dorsal feature is nothing for [s]."
     (toTextDorsalFeature (Consonant Voiceless Alveolar Fricative PulmonicEgressive) == Nothing)
  runTest "The dorsal feature is nothing for [ʃ]."
     (toTextDorsalFeature (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) == Nothing)
  runTest "The dorsal feature is nothing for [ʂ]."
     (toTextDorsalFeature (Consonant Voiceless Retroflex Fricative PulmonicEgressive) == Nothing)
  runTest "The dorsal feature is present for [ɕ]"  -- It is actually in parentheses on page 267.
     (toTextDorsalFeature (analyzeIPA "ɕ") == Just "dorsal")
  runTest "The dorsal feature is present for [ç]" 
     (toTextDorsalFeature (analyzeIPA "ç") == Just "dorsal")
  runTest "The dorsal feature is present for [x]"
     (toTextDorsalFeature (Consonant Voiceless Velar Fricative PulmonicEgressive) == Just "dorsal")
  runTest "The dorsal feature is present for [χ]"
     (toTextDorsalFeature (Consonant Voiceless Uvular Fricative PulmonicEgressive) == Just "dorsal")
  runTest "The dorsal feature is nothing for [ħ]"
     (toTextDorsalFeature (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) == Nothing)
  runTest "The dorsal feature is nothing for [h]"
     (toTextDorsalFeature (Consonant Voiceless Glottal Fricative PulmonicEgressive) == Nothing)
  runTest "The dorsal feature is nothing for [m]"
    (toTextDorsalFeature (Consonant Voiced Bilabial Nasal PulmonicEgressive) == Nothing)
  runTest "The dorsal feature is nothing for [ɱ]"
    (toTextDorsalFeature (Consonant Voiced LabioDental Nasal PulmonicEgressive) == Nothing)
  runTest "The dorsal feature is nothing for [n]"
    (toTextDorsalFeature (Consonant Voiced Alveolar Nasal PulmonicEgressive) == Nothing)
  runTest "The dorsal feature is nothing for [ɳ]"
    (toTextDorsalFeature (Consonant Voiced Retroflex Nasal PulmonicEgressive) == Nothing)
  runTest "The dorsal feature is present for [ɲ]"
    (toTextDorsalFeature (Consonant Voiced Palatal Nasal PulmonicEgressive) == Just "dorsal")
  runTest "The dorsal feature is present for [ŋ]"
    (toTextDorsalFeature (Consonant Voiced Velar Nasal PulmonicEgressive) == Just "dorsal")
  runTest "The dorsal feature is present for [ɴ]"
    (toTextDorsalFeature (Consonant Voiced Uvular Nasal PulmonicEgressive) == Just "dorsal")




  runTest "The pharyngeal feature is nothing for [ɸ]."
     (toTextPharyngealFeature (Consonant Voiceless Bilabial Fricative PulmonicEgressive) == Nothing)
  runTest "The pharyngeal feature is nothing for [f]."
     (toTextPharyngealFeature (Consonant Voiceless LabioDental Fricative PulmonicEgressive) == Nothing)
  runTest "The pharyngeal feature is nothing for [θ]."
     (toTextPharyngealFeature (Consonant Voiceless Dental Fricative PulmonicEgressive) == Nothing)
  runTest "The pharyngeal feature is nothing for [s]."
     (toTextPharyngealFeature (Consonant Voiceless Alveolar Fricative PulmonicEgressive) == Nothing)
  runTest "The pharyngeal feature is nothing for [ʃ]."
     (toTextPharyngealFeature (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) == Nothing)
  runTest "The pharyngeal feature is nothing for [ʂ]."
     (toTextPharyngealFeature (Consonant Voiceless Retroflex Fricative PulmonicEgressive) == Nothing)
  runTest "The pharyngeal feature is nothing for [ɕ]"
     (toTextPharyngealFeature (analyzeIPA "ɕ") == Nothing)
  runTest "The pharyngeal feature is nothing for [ç]" 
     (toTextPharyngealFeature (analyzeIPA "ç") == Nothing)
  runTest "The pharyngeal feature is nothing for [x]"
     (toTextPharyngealFeature (Consonant Voiceless Velar Fricative PulmonicEgressive) == Nothing)
  runTest "The pharyngeal feature is nothing for [χ]"
     (toTextPharyngealFeature (Consonant Voiceless Uvular Fricative PulmonicEgressive) == Nothing)
  runTest "The pharyngeal feature is present for [ħ]"
     (toTextPharyngealFeature (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) == Just "pharyngeal")
  runTest "The pharyngeal feature is nothing for [h]"
     (toTextPharyngealFeature (Consonant Voiceless Glottal Fricative PulmonicEgressive) == Nothing)
  runTest "The pharyngeal feature is nothing for [m]"
    (toTextPharyngealFeature (Consonant Voiced Bilabial Nasal PulmonicEgressive) == Nothing)
  runTest "The pharyngeal feature is nothing for [ɱ]"
    (toTextPharyngealFeature (Consonant Voiced LabioDental Nasal PulmonicEgressive) == Nothing)
  runTest "The pharyngeal feature is nothing for [n]"
    (toTextPharyngealFeature (Consonant Voiced Alveolar Nasal PulmonicEgressive) == Nothing)
  runTest "The pharyngeal feature is nothing for [ɳ]"
    (toTextPharyngealFeature (Consonant Voiced Retroflex Nasal PulmonicEgressive) == Nothing)
  runTest "The pharyngeal feature is nothing for [ɲ]"
    (toTextPharyngealFeature (Consonant Voiced Palatal Nasal PulmonicEgressive) == Nothing)
  runTest "The pharyngeal feature is nothing for [ŋ]"
    (toTextPharyngealFeature (Consonant Voiced Velar Nasal PulmonicEgressive) == Nothing)
  runTest "The pharyngeal feature is nothing for [ɴ]"
    (toTextPharyngealFeature (Consonant Voiced Uvular Nasal PulmonicEgressive) == Nothing)



  runTest "The laryngeal feature is nothing for [ɸ]."
     (toTextLaryngealFeature (Consonant Voiceless Bilabial Fricative PulmonicEgressive) == Nothing)
  runTest "The laryngeal feature is nothing for [f]."
     (toTextLaryngealFeature (Consonant Voiceless LabioDental Fricative PulmonicEgressive) == Nothing)
  runTest "The laryngeal feature is nothing for [θ]."
     (toTextLaryngealFeature (Consonant Voiceless Dental Fricative PulmonicEgressive) == Nothing)
  runTest "The laryngeal feature is nothing for [s]."
     (toTextLaryngealFeature (Consonant Voiceless Alveolar Fricative PulmonicEgressive) == Nothing)
  runTest "The laryngeal feature is nothing for [ʃ]."
     (toTextLaryngealFeature (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) == Nothing)
  runTest "The laryngeal feature is nothing for [ʂ]."
     (toTextLaryngealFeature (Consonant Voiceless Retroflex Fricative PulmonicEgressive) == Nothing)
  runTest "The laryngeal feature is nothing for [ɕ]"
     (toTextLaryngealFeature (analyzeIPA "ɕ") == Nothing)
  runTest "The laryngeal feature is nothing for [ç]" 
     (toTextLaryngealFeature (analyzeIPA "ç") == Nothing)
  runTest "The laryngeal feature is nothing for [x]"
     (toTextLaryngealFeature (Consonant Voiceless Velar Fricative PulmonicEgressive) == Nothing)
  runTest "The laryngeal feature is nothing for [χ]"
     (toTextLaryngealFeature (Consonant Voiceless Uvular Fricative PulmonicEgressive) == Nothing)
  runTest "The laryngeal feature is nothing for [ħ]"
     (toTextLaryngealFeature (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) == Nothing)
  runTest "The laryngeal feature is present for [h]"
     (toTextLaryngealFeature (Consonant Voiceless Glottal Fricative PulmonicEgressive) == Just "laryngeal")
  runTest "The laryngeal feature is nothing for [m]"
    (toTextLaryngealFeature (Consonant Voiced Bilabial Nasal PulmonicEgressive) == Nothing)
  runTest "The laryngeal feature is nothing for [ɱ]"
    (toTextLaryngealFeature (Consonant Voiced LabioDental Nasal PulmonicEgressive) == Nothing)
  runTest "The laryngeal feature is nothing for [n]"
    (toTextLaryngealFeature (Consonant Voiced Alveolar Nasal PulmonicEgressive) == Nothing)
  runTest "The laryngeal feature is nothing for [ɳ]"
    (toTextLaryngealFeature (Consonant Voiced Retroflex Nasal PulmonicEgressive) == Nothing)
  runTest "The laryngeal feature is nothing for [ɲ]"
    (toTextLaryngealFeature (Consonant Voiced Palatal Nasal PulmonicEgressive) == Nothing)
  runTest "The laryngeal feature is nothing for [ŋ]"
    (toTextLaryngealFeature (Consonant Voiced Velar Nasal PulmonicEgressive) == Nothing)
  runTest "The laryngeal feature is nothing for [ɴ]"
    (toTextLaryngealFeature (Consonant Voiced Uvular Nasal PulmonicEgressive) == Nothing)


  -- Go to page 270

  runTest "The back feature is - for [i]."
    (toTextBackFeature (analyzeIPA "i") == Just "- back")
  runTest "The high feature is + for [i]."
    (toTextHighFeature (analyzeIPA "i") == Just "+ high")
  runTest "The low feature is - for [i]."
    (toTextLowFeature (analyzeIPA "i") == Just "- low")
  runTest "The ATR feature is + for [i]."
    (toTextATRFeature (analyzeIPA "i") == Just "+ ATR")
  runTest "The round feature is - for [i]."
    (toTextRoundFeature (analyzeIPA "i") == Just "- round")


  runTest "The back feature is - for [ɪ]."
    (toTextBackFeature (analyzeIPA "ɪ") == Just "- back")
  runTest "The high feature is + for [ɪ]."
    (toTextHighFeature (analyzeIPA "ɪ") == Just "+ high")
  runTest "The low feature is - for [ɪ]."
    (toTextLowFeature (analyzeIPA "ɪ") == Just "- low")
  runTest "The ATR feature is - for [ɪ]."
    (toTextATRFeature (analyzeIPA "ɪ") == Just "- ATR")
  runTest "The round feature is - for [ɪ]."
    (toTextRoundFeature (analyzeIPA "ɪ") == Just "- round")


  runTest "The back feature is - for [e]."
    (toTextBackFeature (analyzeIPA "e") == Just "- back")
  runTest "The high feature is - for [e]."
    (toTextHighFeature (analyzeIPA "e") == Just "- high")
  runTest "The low feature is - for [e]."
    (toTextLowFeature (analyzeIPA "e") == Just "- low")
  runTest "The ATR feature is + for [e]."
    (toTextATRFeature (analyzeIPA "e") == Just "+ ATR")
  runTest "The round feature is - for [e]."
    (toTextRoundFeature (analyzeIPA "e") == Just "- round")




  runTest "The back feature is - for [ɛ]."
    (toTextBackFeature (analyzeIPA "ɛ") == Just "- back")
  runTest "The high feature is - for [ɛ]."
    (toTextHighFeature (analyzeIPA "ɛ") == Just "- high")
  runTest "The low feature is - for [ɛ]."
    (toTextLowFeature (analyzeIPA "ɛ") == Just "- low")
  runTest "The ATR feature is - for [ɛ]."
    (toTextATRFeature (analyzeIPA "ɛ") == Just "- ATR")
  runTest "The round feature is - for [ɛ]."
    (toTextRoundFeature (analyzeIPA "ɛ") == Just "- round")
    
    

  runTest "The back feature is - for [æ]."
    (toTextBackFeature (analyzeIPA "æ") == Just "- back")
  runTest "The high feature is - for [æ]."
    (toTextHighFeature (analyzeIPA "æ") == Just "- high")
  runTest "The low feature is + for [æ]."
    (toTextLowFeature (analyzeIPA "æ") == Just "+ low")
  runTest "The ATR feature is - for [æ]." -- It has a parentheses
    (toTextATRFeature (analyzeIPA "æ") == Just "- ATR")
  runTest "The round feature is - for [æ]."
    (toTextRoundFeature (analyzeIPA "æ") == Just "- round")
    
 
  runTest "The back feature is + for [u]."
    (toTextBackFeature (analyzeIPA "u") == Just "+ back")
  runTest "The high feature is + for [u]."
    (toTextHighFeature (analyzeIPA "u") == Just "+ high")
  runTest "The low feature is - for [u]."
    (toTextLowFeature (analyzeIPA "u") == Just "- low")
  runTest "The ATR feature is + for [u]."
    (toTextATRFeature (analyzeIPA "u") == Just "+ ATR")
  runTest "The round feature is + for [u]."
    (toTextRoundFeature (analyzeIPA "u") == Just "+ round")





  runTest "The back feature is + for [ʊ]."
    (toTextBackFeature (analyzeIPA "ʊ") == Just "+ back")
  runTest "The high feature is + for [ʊ]."
    (toTextHighFeature (analyzeIPA "ʊ") == Just "+ high")
  runTest "The low feature is - for [ʊ]."
    (toTextLowFeature (analyzeIPA "ʊ") == Just "- low")
  runTest "The ATR feature is - for [ʊ]."
    (toTextATRFeature (analyzeIPA "ʊ") == Just "- ATR")
  runTest "The round feature is + for [ʊ]."
    (toTextRoundFeature (analyzeIPA "ʊ") == Just "+ round")



  runTest "The back feature is + for [o]."
    (toTextBackFeature (analyzeIPA "o") == Just "+ back")
  runTest "The high feature is - for [o]."
    (toTextHighFeature (analyzeIPA "o") == Just "- high")
  runTest "The low feature is - for [o]."
    (toTextLowFeature (analyzeIPA "o") == Just "- low")
  runTest "The ATR feature is + for [o]."
    (toTextATRFeature (analyzeIPA "o") == Just "+ ATR")
  runTest "The round feature is + for [o]."
    (toTextRoundFeature (analyzeIPA "o") == Just "+ round")



  runTest "The back feature is + for [ɔ]."
    (toTextBackFeature (analyzeIPA "ɔ") == Just "+ back")
  runTest "The high feature is - for [ɔ]."
    (toTextHighFeature (analyzeIPA "ɔ") == Just "- high")
  runTest "The low feature is - for [ɔ]."
    (toTextLowFeature (analyzeIPA "ɔ") == Just "- low")
  runTest "The ATR feature is - for [ɔ]."
    (toTextATRFeature (analyzeIPA "ɔ") == Just "- ATR")
  runTest "The round feature is + for [ɔ]."
    (toTextRoundFeature (analyzeIPA "ɔ") == Just "+ round")




  runTest "The back feature is + for [ɑ]."
    (toTextBackFeature (analyzeIPA "ɑ") == Just "+ back")
  runTest "The high feature is - for [ɑ]."
    (toTextHighFeature (analyzeIPA "ɑ") == Just "- high")
  runTest "The low feature is + for [ɑ]."
    (toTextLowFeature (analyzeIPA "ɑ") == Just "+ low")
  runTest "The ATR feature is - for [ɑ]."
    (toTextATRFeature (analyzeIPA "ɑ") == Just "- ATR") -- In brackets
  runTest "The round feature is - for [ɑ]."
    (toTextRoundFeature (analyzeIPA "ɑ") == Just "- round")




  runTest "The back feature is - for [y]."
    (toTextBackFeature (analyzeIPA "y") == Just "- back")
  runTest "The high feature is + for [y]."
    (toTextHighFeature (analyzeIPA "y") == Just "+ high")
  runTest "The low feature is - for [y]."
    (toTextLowFeature (analyzeIPA "y") == Just "- low")
  runTest "The ATR feature is + for [y]."
    (toTextATRFeature (analyzeIPA "y") == Just "+ ATR")
  runTest "The round feature is + for [y]."
    (toTextRoundFeature (analyzeIPA "y") == Just "+ round")



  runTest "The back feature is - for [ø]."
    (toTextBackFeature (analyzeIPA "ø") == Just "- back")
  runTest "The high feature is - for [ø]."
    (toTextHighFeature (analyzeIPA "ø") == Just "- high")
  runTest "The low feature is - for [ø]."
    (toTextLowFeature (analyzeIPA "ø") == Just "- low")
  runTest "The ATR feature is + for [ø]."
    (toTextATRFeature (analyzeIPA "ø") == Just "+ ATR")
  runTest "The round feature is + for [ø]."
    (toTextRoundFeature (analyzeIPA "ø") == Just "+ round")



  runTest "The back feature is + for [ɨ]."
    (toTextBackFeature (analyzeIPA "ɨ") == Just "+ back")
  runTest "The high feature is + for [ɨ]."
    (toTextHighFeature (analyzeIPA "ɨ") == Just "+ high")
  runTest "The low feature is - for [ɨ]."
    (toTextLowFeature (analyzeIPA "ɨ") == Just "- low")
  runTest "The ATR feature is - for [ɨ]."  -- In parentheses
    (toTextATRFeature (analyzeIPA "ɨ") == Just "- ATR")
  runTest "The round feature is - for [ɨ]."
    (toTextRoundFeature (analyzeIPA "ɨ") == Just "- round")
    
    
 
  runTest "The back feature is + for [ʌ]."
    (toTextBackFeature (analyzeIPA "ʌ") == Just "+ back")
  runTest "The high feature is - for [ʌ]."
    (toTextHighFeature (analyzeIPA "ʌ") == Just "- high")
  runTest "The low feature is - for [ʌ]."
    (toTextLowFeature (analyzeIPA "ʌ") == Just "- low")
  runTest "The ATR feature is - for [ʌ]."
    (toTextATRFeature (analyzeIPA "ʌ") == Just "- ATR") -- in parentheses
  runTest "The round feature is - for [ʌ]."
    (toTextRoundFeature (analyzeIPA "ʌ") == Just "- round")


  runTest "The ATR feature is nothing for [z]."
    (toTextATRFeature (analyzeIPA "z") == Nothing)
  runTest "The ATR feature is nothing for [p]."
    (toTextATRFeature (analyzeIPA "p") == Nothing)


  runTest "The nasal feature is unary."
    (isUnary NasalFeature)
  runTest "The lateral feature is unary."
    (isUnary LateralFeature)
  runTest "The delayed release feature is unary."
    (isUnary DelayedReleaseFeature)
  runTest "The spread glottis feature is unary."
    (isUnary SpreadGlottisFeature)
  runTest "The constricted glottis feature is unary."
    (isUnary ConstrictedGlottisFeature)
  runTest "The labial feature is unary."
    (isUnary LabialFeature)
  runTest "The coronal feature is unary."
    (isUnary CoronalFeature)
  runTest "The dorsal feature is unary."
    (isUnary DorsalFeature)
  runTest "The pharyngeal feature is unary."
    (isUnary PharyngealFeature)
  runTest "The LaryngealFeature is unary."
    (isUnary LaryngealFeature)
