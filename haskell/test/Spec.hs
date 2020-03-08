module Spec (main) where

import Prelude ((++), IO, length, Bool(True, False), putStrLn, putStr, (==), (&&), String, Maybe(Just, Nothing))
import PhonemeFeature (isGlide, toTextLowFeature)
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

