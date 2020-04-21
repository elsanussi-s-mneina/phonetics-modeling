{-# LANGUAGE UnicodeSyntax #-}

module Spec (main) where
import Data.Maybe (fromJust) 
import Prelude (IO, length, Bool(True, False), putStrLn, putStr, show, String, Maybe(Just, Nothing))
import Prelude.Unicode
  ( (≡), (⧺), (∧), (∨), (∘)
  )

import Lib

import Tester (printLegend, runTest)
sectionDivider = "\n\n"


main ∷ IO ()
main = do
  printLegend
  glideSpec
  analyzeIPASpec
  devoicedIPASpec
  spirantizedIPASpec
  showEnglishPhonemeInventorySpec
  speFeaturesSpec
  graphemeGrammarSpec
  putStrLn "\n\nProgram (Test suite) terminated normally."
  -- We need at least one PutStrLn so that all output makes it to the console before the program terminates.

-- TODO: make the console output when running the tests using
-- `stack test` 
-- not say that the tests passed, when there is actually a failure.


graphemeGrammarSpec ∷ IO ()
graphemeGrammarSpec = do
  putStrLn sectionDivider
  runTest "t̥ (t with under-ring) should remain t̥ when fixing misplaced diacritics."
    (preventProhibitedCombination "t̥" ≡ "t̥")
  runTest "z̥ (z with under-ring) should remain z̥ when fixing misplaced diacritics."
    (preventProhibitedCombination "z̥" ≡ "z̥")
  runTest "p̥ (p with under-ring) should become p̊ (p with over-ring) when fixing misplaced diacritics."
    (preventProhibitedCombination "p̥" ≡ "p̊")
  runTest "z̊ (z with over-ring) should not change when fixing misplaced diacritics."
    (preventProhibitedCombination "z̊" ≡ "z̊")
  runTest "l̊ (l with over-ring) should become l̥ (l with under-ring) when fixing misplaced diacritics."
    (preventProhibitedCombination "l̊" ≡ "l̥")


glideSpec ∷ IO ()
glideSpec = do
  putStrLn sectionDivider
  putStrLn "recognizing a glide"
  runTest "[j] the voiced palatal approximant is a glide." (isGlide (analyzeIPAHelper "j") ≡ True ) 
  runTest "[ʝ] the voiced palatal fricative is not a glide." (isGlide (analyzeIPAHelper "ʝ") ≡ False) 
  runTest "[w] is a glide." (isGlide (analyzeIPAHelper "w") ≡ True) 
  runTest "[c] is not a glide." (isGlide (analyzeIPAHelper "c") ≡ False) 

analyzeIPASpec ∷ IO ()
analyzeIPASpec = do
      putStrLn sectionDivider

      putStrLn "analyzing place, voicing, manner, and airstream mechanism of sound represented by IPA symbols"
      runTest ("[p] is a voiceless bilabial plosive consonant" ⧺ 
                " with pulmonic egressive airstream mechanism.")
                (show (analyzeIPAHelper "p") ≡ "voiceless bilabial plosive pulmonic egressive consonant" ∧
                constructIPA     (Consonant Voiceless Bilabial Plosive PulmonicEgressive) ≡ "p") 
 
      runTest ("[pʰ] is a voiceless aspirated bilabial plosive consonant" ⧺ 
                " with pulmonic egressive airstream mechanism.")
                (show (analyzeIPAHelper "pʰ") ≡ "voiceless aspirated bilabial plosive pulmonic egressive consonant" ∧
                constructIPA     (Consonant VoicelessAspirated Bilabial Plosive PulmonicEgressive) ≡ "pʰ") 
                
      runTest ("should be that: [s] is a voiceless alveolar fricative consonant" ⧺
                "with pulmonic egressive airstream mechanism.") 
                (show (analyzeIPAHelper "s") ≡ "voiceless alveolar fricative pulmonic egressive consonant" ∧
                    constructIPA             (Consonant Voiceless Alveolar Fricative PulmonicEgressive) ≡ "s")
      runTest ("should be that: [b] is a voiceless bilabial plosive consonant with " ⧺
          "pulmonic egressive airstream mechanism") 
            (show (analyzeIPAHelper "b") ≡ "voiced bilabial plosive pulmonic egressive consonant" ∧ -- Consonant  Voiced    Bilabial  Plosive PulmonicEgressive ∧
            constructIPA             (Consonant  Voiced    Bilabial  Plosive PulmonicEgressive) ≡ "b")
      runTest ("should be that: [t] is a voiceless alveloar plosive consonant with " ⧺
          "pulmonic egressive airstream mechanism") 
            (show (analyzeIPAHelper "t") ≡ "voiceless alveolar plosive pulmonic egressive consonant" ∧ 
            constructIPA             (Consonant  Voiceless Alveolar  Plosive PulmonicEgressive) ≡ "t")
-- Plosives:
      runTest ("should be that: [p] is a voiceless bilabial plosive consonant with " ⧺ 
          "pulmonic egressive airstream mechanism") 
           (show (analyzeIPAHelper "p") ≡ "voiceless bilabial plosive pulmonic egressive consonant" ∧
           constructIPA (Consonant Voiceless Bilabial Plosive PulmonicEgressive) ≡ "p")
      runTest ("should be that: [t] is a voiceless alveolar plosive with " ⧺ 
          "pulmonic egressive airstream mechanism")  
          (show (analyzeIPAHelper "t")  ≡ "voiceless alveolar plosive pulmonic egressive consonant" ∧
          constructIPA              (Consonant  Voiceless Alveolar  Plosive PulmonicEgressive) ≡ "t")
      runTest ("should be that: [d] is a voiced alveolar plosive with " ⧺ 
          "pulmonic egressive airstream mechanism") 
          (show (analyzeIPAHelper "d")  ≡ "voiced alveolar plosive pulmonic egressive consonant" ∧
          constructIPA              (Consonant  Voiced    Alveolar  Plosive PulmonicEgressive) ≡ "d")
      runTest ("should be that: [ʈ] is a voiceless retroflex plosive with " ⧺ 
          "pulmonic egressive airstream mechanism") 
          (show (analyzeIPAHelper "ʈ")  ≡ "voiceless retroflex plosive pulmonic egressive consonant" ∧
          constructIPA              (Consonant  Voiceless Retroflex Plosive PulmonicEgressive) ≡ "ʈ")
      runTest ("should be that: [ɖ] is a voiced retroflex plosive with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (show (analyzeIPAHelper "ɖ")  ≡ "voiced retroflex plosive pulmonic egressive consonant")
      runTest ("should be that: [c] is a voiceless palatal plosive with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "c"  ≡ Consonant  Voiceless Palatal   Plosive PulmonicEgressive)
      runTest ("should be that: [ɟ] is a voiced palatal plosive with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɟ"  ≡ Consonant  Voiced    Palatal   Plosive PulmonicEgressive)
      runTest ("should be that: [k] is a voiceless velar plosive with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "k"  ≡ Consonant  Voiceless Velar     Plosive PulmonicEgressive)
      runTest ("should be that: [g] is a voiced velar plosive with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "g"  ≡ Consonant  Voiced    Velar     Plosive PulmonicEgressive)
      runTest ("should be that: [q] is a voiceless uvular plosive with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "q"  ≡ Consonant  Voiceless Uvular    Plosive PulmonicEgressive)
      runTest ("should be that: [ɢ] is a voiced uvular plosive with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɢ"  ≡ Consonant  Voiced    Uvular    Plosive PulmonicEgressive)
      runTest ("should be that: [ʔ] is a voiceless glottal plosive with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ʔ"  ≡ Consonant  Voiceless Glottal   Plosive PulmonicEgressive)
     -- Nasals:
      runTest ("should be that: [m] is a voiced bilabial nasal with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "m"  ≡ Consonant  Voiced Bilabial    Nasal PulmonicEgressive)
      runTest ("should be that: [ɱ] is a voiced labio-dental nasal with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɱ"  ≡ Consonant  Voiced LabioDental Nasal PulmonicEgressive)
      runTest ("should be that: [n] is a voiced alveolar nasal with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "n"  ≡ Consonant  Voiced Alveolar    Nasal PulmonicEgressive)
      runTest ("should be that: [ɳ] is a voiced retroflex nasal with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɳ"  ≡ Consonant  Voiced Retroflex   Nasal PulmonicEgressive)
      runTest ("should be that: [ɲ] is a voiced palatal nasal with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɲ"  ≡ Consonant  Voiced Palatal     Nasal PulmonicEgressive)
      runTest ("should be that: [ŋ] is a voiced velar nasal with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ŋ"  ≡ Consonant  Voiced Velar       Nasal PulmonicEgressive)
      runTest ("should be that: [ɴ] is a voiced uvular nasal with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɴ"  ≡ Consonant  Voiced Uvular      Nasal PulmonicEgressive)
     -- Trills:
      runTest ("should be that: [ʙ] is a voiced bilabial trill with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ʙ"  ≡ Consonant  Voiced Bilabial Trill PulmonicEgressive)
      runTest ("should be that: [r] is a voiced alveolar trill with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "r"  ≡ Consonant  Voiced Alveolar Trill PulmonicEgressive)
      runTest ("should be that: [ʀ] is a voiced uvular trill with " ⧺ 
          "pulmonic egressive airstream mechanism") 
          (analyzeIPAHelper "ʀ"  ≡ Consonant  Voiced Uvular   Trill PulmonicEgressive ∧
          constructIPA              (Consonant  Voiced Uvular   Trill PulmonicEgressive) ≡ "ʀ")
     -- Taps or flaps:
      runTest ("should be that: [ⱱ] is a voiced labio-dental tap or flap with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ⱱ"  ≡ Consonant  Voiced LabioDental TapOrFlap PulmonicEgressive)
      runTest ("should be that: [ɾ] is a voiced alveolar tap or flap with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɾ"  ≡ Consonant  Voiced Alveolar    TapOrFlap PulmonicEgressive)
      runTest ("should be that: [ɽ] is a voiced retroflex tap or flap with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɽ"  ≡ Consonant  Voiced Retroflex   TapOrFlap PulmonicEgressive)
      -- Fricatives:
      runTest ("should be that: [ɸ] is a voiceless bilabial fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɸ"  ≡ Consonant  Voiceless Bilabial     Fricative PulmonicEgressive)
      runTest ("should be that: [β] is a voiced bilabial fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "β"  ≡ Consonant  Voiced    Bilabial     Fricative PulmonicEgressive)
      runTest ("should be that: [f] is a voiceless labio-dental fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "f"  ≡ Consonant  Voiceless LabioDental  Fricative PulmonicEgressive)
      runTest ("should be that: [v] is a voiced labio-dental fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "v"  ≡ Consonant  Voiced    LabioDental  Fricative PulmonicEgressive)
      runTest ("should be that: [θ] is a voiceless dental fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "θ"  ≡ Consonant  Voiceless Dental       Fricative PulmonicEgressive)
      runTest ("should be that: [ð] is a voiced dental fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ð"  ≡ Consonant  Voiced    Dental       Fricative PulmonicEgressive)
      runTest ("should be that: [s] is a voiceless alveolar fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "s"  ≡ Consonant  Voiceless Alveolar     Fricative PulmonicEgressive)
      runTest ("should be that: [z] is a voiced alveolar fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "z"  ≡ Consonant  Voiced    Alveolar     Fricative PulmonicEgressive)
      runTest ("should be that: [ʃ] is a voiceless post-alveolar fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ʃ"  ≡ Consonant  Voiceless PostAlveolar Fricative PulmonicEgressive)
      runTest ("should be that: [ʒ] is a voiced post-alveolar fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ʒ"  ≡ Consonant  Voiced    PostAlveolar Fricative PulmonicEgressive)
      runTest ("should be that: [ʂ] is a voiceless retroflex fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ʂ"  ≡ Consonant  Voiceless Retroflex    Fricative PulmonicEgressive)
      runTest ("should be that: [ʐ] is a voiced retroflex fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ʐ"  ≡ Consonant  Voiced    Retroflex    Fricative PulmonicEgressive)
      runTest ("should be that: [ç] is a voiceless palatal fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ç"  ≡ Consonant  Voiceless Palatal      Fricative PulmonicEgressive)
      runTest ("should be that: [ʝ] is a voiced palatal fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ʝ"  ≡ Consonant  Voiced    Palatal      Fricative PulmonicEgressive)
      runTest ("should be that: [x] is a voiceless velar fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "x"  ≡ Consonant  Voiceless Velar        Fricative PulmonicEgressive)
      runTest ("should be that: [ɣ] is a voiced velar fricative with " ⧺ 
          "pulmonic egressive airstream mechanism") 
          (analyzeIPAHelper "ɣ"  ≡ Consonant  Voiced    Velar        Fricative PulmonicEgressive)
      runTest ("should be that: [χ] is a voiceless uvular fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "χ"  ≡ Consonant  Voiceless Uvular       Fricative PulmonicEgressive)
      runTest ("should be that: [ʁ] is a voiced uvular fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ʁ"  ≡ Consonant  Voiced    Uvular       Fricative PulmonicEgressive)
      runTest ("should be that: [ħ] is a voiceless pharyngeal fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ħ"  ≡ Consonant  Voiceless Pharyngeal   Fricative PulmonicEgressive)
      runTest ("should be that: [ʕ] is a voiced pharyngeal fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ʕ"  ≡ Consonant  Voiced    Pharyngeal   Fricative PulmonicEgressive)
      runTest ("should be that: [h] is a voiceless glottal fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "h"  ≡ Consonant  Voiceless Glottal      Fricative PulmonicEgressive)
      runTest ("should be that: [ɦ] is a voiced glottal fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɦ"  ≡ Consonant  Voiced    Glottal      Fricative PulmonicEgressive)
      -- Lateral Fricatives:
      runTest ("should be that: [ɬ] is a voiceless alveolar lateral fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɬ" ≡ Consonant  Voiceless Alveolar LateralFricative PulmonicEgressive)
      runTest ("should be that: [ɮ] is a voiced alveolar lateral fricative with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɮ" ≡ Consonant  Voiced    Alveolar LateralFricative PulmonicEgressive)
      -- Approximants:
      runTest ("should be that: [ʋ] is a voiced labio-dental approximant with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ʋ"  ≡ Consonant  Voiced LabioDental  Approximant PulmonicEgressive)
      runTest ("should be that: [ɹ] is a voiced alveolar approximant with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɹ"  ≡ Consonant  Voiced Alveolar Approximant PulmonicEgressive)
      runTest ("should be that: [ɻ] is a voiced retroflex approximant with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɻ"  ≡ Consonant  Voiced Retroflex    Approximant PulmonicEgressive)
      runTest ("should be that: [j] is a voiced palatal approximant with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "j"  ≡ Consonant  Voiced Palatal      Approximant PulmonicEgressive)
      runTest ("should be that: [ɰ] is a voiced velar approximant with " ⧺
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɰ"  ≡ Consonant  Voiced Velar        Approximant PulmonicEgressive)
      -- Lateral Approximants:
      runTest ("should be that: [l] is a voiced alveolar lateral approximant with " ⧺ 
          "pulmonic egressive airstream mechanism") 
          (analyzeIPAHelper "l"  ≡ Consonant  Voiced Alveolar  LateralApproximant PulmonicEgressive)
      runTest ("should be that: [ɭ] is a voiced retroflex lateral approximant with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ɭ"  ≡ Consonant  Voiced Retroflex LateralApproximant PulmonicEgressive)
      runTest ("should be that: [ʎ] is a voiced palatal lateral approximant with " ⧺ 
          "pulmonic egressive airstream mechanism")
          (analyzeIPAHelper "ʎ"  ≡ Consonant  Voiced Palatal   LateralApproximant PulmonicEgressive)
      runTest ("should be that: [ʟ] is a voiced velar lateral approximant with " ⧺ 
          "pulmonic egressive airstream mechanism")
           (analyzeIPAHelper "ʟ"  ≡ Consonant  Voiced Velar     LateralApproximant PulmonicEgressive)




spirantizedIPASpec ∷ IO()
spirantizedIPASpec = do
      putStrLn sectionDivider
      putStrLn "spirantizing a sound (represented in IPA)" 
      runTest ("should be that: [β] is spirantized [b].")
            (spirantizedIPA "b" ≡ "β")
      runTest ("should be that: [ɸ] is spirantized [p].")
            (spirantizedIPA "p" ≡ "ɸ")
      runTest ("should be that: [x] is spirantized [k].")
            (spirantizedIPA "k" ≡ "x")

devoicedIPASpec ∷ IO()
devoicedIPASpec = do
      putStrLn sectionDivider
      putStrLn "devoicing a sound (represented in IPA)" 
      runTest ("should be that: [p] is devoiced [b].")
            (devoicedIPA "b" ≡ "p")
      runTest ("should be that: [n̥] is devoiced [n].")
            (devoicedIPA "n" ≡ "n̥")

showEnglishPhonemeInventorySpec ∷ IO ()
showEnglishPhonemeInventorySpec = do
  putStrLn sectionDivider
  putStrLn "Showing english phoneme inventory"
  runTest "Should show all English phonemes."
    (showIPA englishPhonetInventory ≡ 
     "bpdtgkʔvfðθzsʒʃhd͡ʒt͡ʃmnŋɹ̠jwiuɪʊeoəɛɜʌɔæɐɑɒ")

speFeaturesSpec ∷ IO ()
speFeaturesSpec = do
  putStrLn sectionDivider
  putStrLn "SPE Features"
  -- Go to page 267 of the textbook.
  runTest "The low feature value is nothing for [β]." 
    (low (Consonant Voiced Bilabial Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The low feature value for [χ] is [+ low]."
    (low (Consonant Voiceless Uvular Fricative PulmonicEgressive) ≡ Just (LowFeature Plus))
  runTest "The low feature value for [ɣ] is [+ low]."
    (low (Consonant Voiced Uvular Fricative PulmonicEgressive) ≡ Just (LowFeature Plus))
  runTest "The low feature value for [ħ] is [+ low]."
    (low (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) ≡ Just (LowFeature Plus))
  runTest "The low feature value for [ʕ] is [+ low]."
    (low (Consonant Voiced Pharyngeal Fricative PulmonicEgressive) ≡ Just (LowFeature Plus))
  runTest "The low feature value for [ʔ] is [+ low]."
    (low (Consonant Voiceless Glottal Fricative PulmonicEgressive) ≡ Just (LowFeature Plus))
  runTest "The low feature value is nothing for [k]."
    (low (Consonant Voiceless Velar Plosive PulmonicEgressive) ≡ Nothing)

  runTest "The anterior feature is nothing for [b]."
     (anterior (Consonant Voiceless Bilabial Plosive PulmonicEgressive) ≡ Nothing)
  runTest "The anterior feature is nothing for [f]."
     (anterior (Consonant Voiceless LabioDental Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The anterior feature is + for [θ]."
     (anterior (Consonant Voiceless Dental Fricative PulmonicEgressive) ≡ Just (AnteriorFeature Plus))
  runTest "The anterior feature is + for [s]."
     (anterior (Consonant Voiceless Alveolar Fricative PulmonicEgressive) ≡ Just (AnteriorFeature Plus))
  runTest "The anterior feature is − for [ʃ]."
     (anterior (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) ≡ Just (AnteriorFeature Minus))
  runTest "The anterior feature is − for [ʂ]."
     (anterior (Consonant Voiceless Retroflex Fricative PulmonicEgressive) ≡ Just (AnteriorFeature Minus))
  runTest "The anterior feature is − for [ɕ]" 
     (anterior (analyzeIPAHelper "ɕ") ≡ Just (AnteriorFeature Minus))
  runTest "The anterior feature is − for [ç]" 
     (anterior (analyzeIPAHelper "ç") ≡ Just (AnteriorFeature Minus)) -- TODO: Check this, is it true .ccording to the textbook?
  runTest "The anterior feature is nothing for [x]"
     (anterior (Consonant Voiceless Velar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The anterior feature is nothing for [χ]"
     (anterior (Consonant Voiceless Uvular Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The anterior feature is nothing for [ħ]"
     (anterior (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The anterior feature is nothing for [h]"
     (anterior (Consonant Voiceless Glottal Fricative PulmonicEgressive) ≡ Nothing)
    

  runTest "The distributed feature is nothing for [b]."
     (distributed (Consonant Voiceless Bilabial Plosive PulmonicEgressive) ≡ Nothing)
  runTest "The distributed feature is nothing for [f]."
     (distributed (Consonant Voiceless LabioDental Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The distributed feature is + for [θ]."
     (distributed (Consonant Voiceless Dental Fricative PulmonicEgressive) ≡ Just (DistributedFeature Plus))
  runTest "The distributed feature is − for [s]."
     (distributed (Consonant Voiceless Alveolar Fricative PulmonicEgressive) ≡ Just (DistributedFeature Minus))
  runTest "The distributed feature is + for [ʃ]."
     (distributed (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) ≡ Just (DistributedFeature Plus))
  runTest "The distributed feature is − for [ʂ]."
     (distributed (Consonant Voiceless Retroflex Fricative PulmonicEgressive) ≡ Just (DistributedFeature Minus))
  runTest "The distributed feature is + for [ɕ]" 
     (distributed (analyzeIPAHelper "ɕ") ≡ Just (DistributedFeature Plus))
  runTest "The distributed feature is + for [ç]" 
     (distributed (analyzeIPAHelper "ç") ≡ Just (DistributedFeature Plus)) -- TODO: Check this, is it true according to the textbook?
  runTest "The distributed feature is nothing for [x]"
     (distributed (Consonant Voiceless Velar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The distributed feature is nothing for [χ]"
     (distributed (Consonant Voiceless Uvular Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The distributed feature is nothing for [ħ]"
     (distributed (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The distributed feature is nothing for [h]"
     (distributed (Consonant Voiceless Glottal Fricative PulmonicEgressive) ≡ Nothing)
    

  runTest "The strident feature is − for [ɸ]."
     (strident (Consonant Voiceless Bilabial Fricative PulmonicEgressive) ≡ Just (StridentFeature Minus))
  runTest "The strident feature is + for [f]."
     (strident (Consonant Voiceless LabioDental Fricative PulmonicEgressive) ≡ Just (StridentFeature Plus))
  runTest "The strident feature is − for [θ]."
     (strident (Consonant Voiceless Dental Fricative PulmonicEgressive) ≡ Just (StridentFeature Minus))
  runTest "The strident feature is + for [s]."
     (strident (Consonant Voiceless Alveolar Fricative PulmonicEgressive) ≡ Just (StridentFeature Plus))
  runTest "The strident feature is + for [ʃ]."
     (strident (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) ≡ Just (StridentFeature Plus))
  runTest "The strident feature is − for [ʂ]."
     (strident (Consonant Voiceless Retroflex Fricative PulmonicEgressive) ≡ Just (StridentFeature Minus))
  runTest "The strident feature is − for [ɕ]" 
     (strident (analyzeIPAHelper "ɕ") ≡ Just (StridentFeature Minus))
  runTest "The strident feature is − for [ç]" 
     (strident (analyzeIPAHelper "ç") ≡ Just (StridentFeature Minus)) -- TODO: Check this, is it true according to the textbook?
  runTest "The strident feature is − for [x]"
     (strident (Consonant Voiceless Velar Fricative PulmonicEgressive) ≡ Just (StridentFeature Minus))
  runTest "The strident feature is + for [χ]"
     (strident (Consonant Voiceless Uvular Fricative PulmonicEgressive) ≡ Just (StridentFeature Plus))
  runTest "The strident feature is − for [ħ]"
     (strident (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) ≡ Just (StridentFeature Minus))
  runTest "The strident fetaure is − for [h]"
     (strident (Consonant Voiceless Glottal Fricative PulmonicEgressive) ≡ Just (StridentFeature Minus))
    
  runTest "The high feature is nothing for [ɸ]."
     (high (Consonant Voiceless Bilabial Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The high feature is nothing for [f]."
     (high (Consonant Voiceless LabioDental Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The high feature is nothing for [θ]."
     (high (Consonant Voiceless Dental Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The high feature is nothing for [s]."
     (high (Consonant Voiceless Alveolar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The high feature is nothing for [ʃ]."
     (high (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The high feature is nothing for [ʂ]."
     (high (Consonant Voiceless Retroflex Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The high feature is + for [ɕ]" 
     (high (analyzeIPAHelper "ɕ") ≡ Just (HighFeature Plus))
  runTest "The high feature is + for [ç]" 
     (high (analyzeIPAHelper "ç") ≡ Just (HighFeature Plus)) -- TODO: Check this, is it true according to the textbook?
  runTest "The high feature is + for [x]"
     (high (Consonant Voiceless Velar Fricative PulmonicEgressive) ≡ Just (HighFeature Plus))
  runTest "The high feature is − for [χ]"
     (high (Consonant Voiceless Uvular Fricative PulmonicEgressive) ≡ Just (HighFeature Minus))
  runTest "The high feature is nothing for [ħ]"
     (high (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The high feature is nothing for [h]"
     (high (Consonant Voiceless Glottal Fricative PulmonicEgressive) ≡ Nothing)
    
  runTest "The nasal feature is nothing for [ɸ]."
     (nasal (Consonant Voiceless Bilabial Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The nasal feature is nothing for [f]."
     (nasal (Consonant Voiceless LabioDental Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The nasal feature is nothing for [θ]."
     (nasal (Consonant Voiceless Dental Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The nasal feature is nothing for [s]."
     (nasal (Consonant Voiceless Alveolar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The nasal feature is nothing for [ʃ]."
     (nasal (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The nasal feature is nothing for [ʂ]."
     (nasal (Consonant Voiceless Retroflex Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The nasal feature is nothing for [ɕ]" 
     (nasal (analyzeIPAHelper "ɕ") ≡ Nothing)
  runTest "The nasal feature is nothing for [ç]" 
     (nasal (analyzeIPAHelper "ç") ≡ Nothing)
  runTest "The nasal feature is nothing for [x]"
     (nasal (Consonant Voiceless Velar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The nasal feature is nothing for [χ]"
     (nasal (Consonant Voiceless Uvular Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The nasal feature is nothing for [ħ]"
     (nasal (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The nasal feature is nothing for [h]"
     (nasal (Consonant Voiceless Glottal Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The nasal feature is present for [m]"
    (nasal (Consonant Voiced Bilabial Nasal PulmonicEgressive) ≡ Just NasalFeature)
  runTest "The nasal feature is present for [ɱ]"
    (nasal (Consonant Voiced LabioDental Nasal PulmonicEgressive) ≡ Just NasalFeature)
  runTest "The nasal feature is present for [n]"
    (nasal (Consonant Voiced Alveolar Nasal PulmonicEgressive) ≡ Just NasalFeature)
  runTest "The nasal feature is present for [ɳ]"
    (nasal (Consonant Voiced Retroflex Nasal PulmonicEgressive) ≡ Just NasalFeature)
  runTest "The nasal feature is present for [ɲ]"
    (nasal (Consonant Voiced Palatal Nasal PulmonicEgressive) ≡ Just NasalFeature)
  runTest "The nasal feature is present for [ŋ]"
    (nasal (Consonant Voiced Velar Nasal PulmonicEgressive) ≡ Just NasalFeature)
  runTest "The nasal feature is present for [ɴ]"
    (nasal (Consonant Voiced Uvular Nasal PulmonicEgressive) ≡ Just NasalFeature)

  runTest "The labial feature is present for [ɸ]."
     (labial (Consonant Voiceless Bilabial Fricative PulmonicEgressive) ≡ Just LabialFeature)
  runTest "The labial feature is present for [f]."
     (labial (Consonant Voiceless LabioDental Fricative PulmonicEgressive) ≡ Just LabialFeature)
  runTest "The labial feature is nothing for [θ]."
     (labial (Consonant Voiceless Dental Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The labial feature is nothing for [s]."
     (labial (Consonant Voiceless Alveolar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The labial feature is nothing for [ʃ]."
     (labial (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The labial feature is nothing for [ʂ]."
     (labial (Consonant Voiceless Retroflex Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The labial feature is nothing for [ɕ]" 
     (labial (analyzeIPAHelper "ɕ") ≡ Nothing)
  runTest "The labial feature is nothing for [ç]" 
     (labial (analyzeIPAHelper "ç") ≡ Nothing)
  runTest "The labial feature is nothing for [x]"
     (labial (Consonant Voiceless Velar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The labial feature is nothing for [χ]"
     (labial (Consonant Voiceless Uvular Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The labial feature is nothing for [ħ]"
     (labial (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The labial feature is nothing for [h]"
     (labial (Consonant Voiceless Glottal Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The labial feature is present for [m]"
    (labial (Consonant Voiced Bilabial Nasal PulmonicEgressive) ≡ Just LabialFeature)
  runTest "The labial feature is present for [ɱ]"
    (labial (Consonant Voiced LabioDental Nasal PulmonicEgressive) ≡ Just LabialFeature)
  runTest "The labial feature is nothing for [n]"
    (labial (Consonant Voiced Alveolar Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The labial feature is nothing for [ɳ]"
    (labial (Consonant Voiced Retroflex Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The labial feature is nothing for [ɲ]"
    (labial (Consonant Voiced Palatal Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The labial feature is nothing for [ŋ]"
    (labial (Consonant Voiced Velar Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The labial feature is nothing for [ɴ]"
    (labial (Consonant Voiced Uvular Nasal PulmonicEgressive) ≡ Nothing)


  runTest "The coronal feature is nothing for [ɸ]."
     (coronal (Consonant Voiceless Bilabial Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The coronal feature is nothing for [f]."
     (coronal (Consonant Voiceless LabioDental Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The coronal feature is present for [θ]."
     (coronal (Consonant Voiceless Dental Fricative PulmonicEgressive) ≡ Just CoronalFeature)
  runTest "The coronal feature is present for [s]."
     (coronal (Consonant Voiceless Alveolar Fricative PulmonicEgressive) ≡ Just CoronalFeature)
  runTest "The coronal feature is present for [ʃ]."
     (coronal (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) ≡ Just CoronalFeature)
  runTest "The coronal feature is present for [ʂ]."
     (coronal (Consonant Voiceless Retroflex Fricative PulmonicEgressive) ≡ Just CoronalFeature)
  runTest "The coronal feature is present for [ɕ]" 
     (coronal (analyzeIPAHelper "ɕ") ≡ Just CoronalFeature)
  runTest "The coronal feature is present for [ç]" 
     (coronal (analyzeIPAHelper "ç") ≡ Just CoronalFeature)
  runTest "The coronal feature is nothing for [x]"
     (coronal (Consonant Voiceless Velar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The coronal feature is nothing for [χ]"
     (coronal (Consonant Voiceless Uvular Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The coronal feature is nothing for [ħ]"
     (coronal (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The coronal feature is nothing for [h]"
     (coronal (Consonant Voiceless Glottal Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The coronal feature is nothing for [m]"
    (coronal (Consonant Voiced Bilabial Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The coronal feature is nothing for [ɱ]"
    (coronal (Consonant Voiced LabioDental Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The coronal feature is present for [n]"
    (coronal (Consonant Voiced Alveolar Nasal PulmonicEgressive) ≡ Just CoronalFeature)
  runTest "The coronal feature is present for [ɳ]"
    (coronal (Consonant Voiced Retroflex Nasal PulmonicEgressive) ≡ Just CoronalFeature)
  runTest "The coronal feature is present for [ɲ]"
    (coronal (Consonant Voiced Palatal Nasal PulmonicEgressive) ≡ Just CoronalFeature)
  runTest "The coronal feature is nothing for [ŋ]"
    (coronal (Consonant Voiced Velar Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The coronal feature is nothing for [ɴ]"
    (coronal (Consonant Voiced Uvular Nasal PulmonicEgressive) ≡ Nothing)

  runTest "The dorsal feature is nothing for [ɸ]."
     (dorsal (Consonant Voiceless Bilabial Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The dorsal feature is nothing for [f]."
     (dorsal (Consonant Voiceless LabioDental Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The dorsal feature is nothing for [θ]."
     (dorsal (Consonant Voiceless Dental Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The dorsal feature is nothing for [s]."
     (dorsal (Consonant Voiceless Alveolar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The dorsal feature is nothing for [ʃ]."
     (dorsal (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The dorsal feature is nothing for [ʂ]."
     (dorsal (Consonant Voiceless Retroflex Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The dorsal feature is nothing for [ɕ]" -- May need to double check this.
     (dorsal (analyzeIPAHelper "ɕ") ≡ Nothing)
  runTest "The dorsal feature is present for [ç]" 
     (dorsal (analyzeIPAHelper "ç") ≡ Just DorsalFeature)
  runTest "The dorsal feature is present for [x]"
     (dorsal (Consonant Voiceless Velar Fricative PulmonicEgressive) ≡ Just DorsalFeature)
  runTest "The dorsal feature is present for [χ]"
     (dorsal (Consonant Voiceless Uvular Fricative PulmonicEgressive) ≡ Just DorsalFeature)
  runTest "The dorsal feature is nothing for [ħ]"
     (dorsal (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The dorsal feature is nothing for [h]"
     (dorsal (Consonant Voiceless Glottal Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The dorsal feature is nothing for [m]"
    (dorsal (Consonant Voiced Bilabial Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The dorsal feature is nothing for [ɱ]"
    (dorsal (Consonant Voiced LabioDental Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The dorsal feature is nothing for [n]"
    (dorsal (Consonant Voiced Alveolar Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The dorsal feature is nothing for [ɳ]"
    (dorsal (Consonant Voiced Retroflex Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The dorsal feature is present for [ɲ]"
    (dorsal (Consonant Voiced Palatal Nasal PulmonicEgressive) ≡ Just DorsalFeature)
  runTest "The dorsal feature is present for [ŋ]"
    (dorsal (Consonant Voiced Velar Nasal PulmonicEgressive) ≡ Just DorsalFeature)
  runTest "The dorsal feature is present for [ɴ]"
    (dorsal (Consonant Voiced Uvular Nasal PulmonicEgressive) ≡ Just DorsalFeature)




  runTest "The pharyngeal feature is nothing for [ɸ]."
     (pharyngeal (Consonant Voiceless Bilabial Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [f]."
     (pharyngeal (Consonant Voiceless LabioDental Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [θ]."
     (pharyngeal (Consonant Voiceless Dental Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [s]."
     (pharyngeal (Consonant Voiceless Alveolar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [ʃ]."
     (pharyngeal (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [ʂ]."
     (pharyngeal (Consonant Voiceless Retroflex Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [ɕ]"
     (pharyngeal (analyzeIPAHelper "ɕ") ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [ç]" 
     (pharyngeal (analyzeIPAHelper "ç") ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [x]"
     (pharyngeal (Consonant Voiceless Velar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [χ]"
     (pharyngeal (Consonant Voiceless Uvular Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The pharyngeal feature is present for [ħ]"
     (pharyngeal (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) ≡ Just PharyngealFeature)
  runTest "The pharyngeal feature is nothing for [h]"
     (pharyngeal (Consonant Voiceless Glottal Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [m]"
    (pharyngeal (Consonant Voiced Bilabial Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [ɱ]"
    (pharyngeal (Consonant Voiced LabioDental Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [n]"
    (pharyngeal (Consonant Voiced Alveolar Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [ɳ]"
    (pharyngeal (Consonant Voiced Retroflex Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [ɲ]"
    (pharyngeal (Consonant Voiced Palatal Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [ŋ]"
    (pharyngeal (Consonant Voiced Velar Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The pharyngeal feature is nothing for [ɴ]"
    (pharyngeal (Consonant Voiced Uvular Nasal PulmonicEgressive) ≡ Nothing)



  runTest "The laryngeal feature is nothing for [ɸ]."
     (laryngeal (Consonant Voiceless Bilabial Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The laryngeal feature is nothing for [f]."
     (laryngeal (Consonant Voiceless LabioDental Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The laryngeal feature is nothing for [θ]."
     (laryngeal (Consonant Voiceless Dental Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The laryngeal feature is nothing for [s]."
     (laryngeal (Consonant Voiceless Alveolar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The laryngeal feature is nothing for [ʃ]."
     (laryngeal (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The laryngeal feature is nothing for [ʂ]."
     (laryngeal (Consonant Voiceless Retroflex Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The laryngeal feature is nothing for [ɕ]"
     (laryngeal (analyzeIPAHelper "ɕ") ≡ Nothing)
  runTest "The laryngeal feature is nothing for [ç]" 
     (laryngeal (analyzeIPAHelper "ç") ≡ Nothing)
  runTest "The laryngeal feature is nothing for [x]"
     (laryngeal (Consonant Voiceless Velar Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The laryngeal feature is nothing for [χ]"
     (laryngeal (Consonant Voiceless Uvular Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The laryngeal feature is nothing for [ħ]"
     (laryngeal (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) ≡ Nothing)
  runTest "The laryngeal feature is present for [h]"
     (laryngeal (Consonant Voiceless Glottal Fricative PulmonicEgressive) ≡ Just LaryngealFeature)
  runTest "The laryngeal feature is nothing for [m]"
    (laryngeal (Consonant Voiced Bilabial Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The laryngeal feature is nothing for [ɱ]"
    (laryngeal (Consonant Voiced LabioDental Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The laryngeal feature is nothing for [n]"
    (laryngeal (Consonant Voiced Alveolar Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The laryngeal feature is nothing for [ɳ]"
    (laryngeal (Consonant Voiced Retroflex Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The laryngeal feature is nothing for [ɲ]"
    (laryngeal (Consonant Voiced Palatal Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The laryngeal feature is nothing for [ŋ]"
    (laryngeal (Consonant Voiced Velar Nasal PulmonicEgressive) ≡ Nothing)
  runTest "The laryngeal feature is nothing for [ɴ]"
    (laryngeal (Consonant Voiced Uvular Nasal PulmonicEgressive) ≡ Nothing)


  -- Go to page 270

  runTest "The back feature is − for [i]."
    (back (analyzeIPAHelper "i") ≡ Just (BackFeature Minus))
  runTest "The high feature is + for [i]."
    (high (analyzeIPAHelper "i") ≡ Just (HighFeature Plus))
  runTest "The low feature is − for [i]."
    (low (analyzeIPAHelper "i") ≡ Just (LowFeature Minus))
  runTest "The ATR feature is + for [i]."
    (atr (analyzeIPAHelper "i") ≡ Just (AdvancedTongueRootFeature Plus))
  runTest "The round feature is − for [i]."
    (round (analyzeIPAHelper "i") ≡ Just (RoundFeature Minus))


  runTest "The back feature is − for [ɪ]."
    (back (analyzeIPAHelper "ɪ") ≡ Just (BackFeature Minus))
  runTest "The high feature is + for [ɪ]."
    (high (analyzeIPAHelper "ɪ") ≡ Just (HighFeature Plus))
  runTest "The low feature is − for [ɪ]."
    (low (analyzeIPAHelper "ɪ") ≡ Just (LowFeature Minus))
  runTest "The ATR feature is − for [ɪ]."
    (atr (analyzeIPAHelper "ɪ") ≡ Just (AdvancedTongueRootFeature Minus))
  runTest "The round feature is − for [ɪ]."
    (round (analyzeIPAHelper "ɪ") ≡ Just (RoundFeature Minus))


  runTest "The back feature is − for [e]."
    (back (analyzeIPAHelper "e") ≡ Just (BackFeature Minus))
  runTest "The high feature is − for [e]."
    (high (analyzeIPAHelper "e") ≡ Just (HighFeature Minus))
  runTest "The low feature is − for [e]."
    (low (analyzeIPAHelper "e") ≡ Just (LowFeature Minus))
  runTest "The ATR feature is + for [e]."
    (atr (analyzeIPAHelper "e") ≡ Just (AdvancedTongueRootFeature Plus))
  runTest "The round feature is − for [e]."
    (round (analyzeIPAHelper "e") ≡ Just (RoundFeature Minus))




  runTest "The back feature is − for [ɛ]."
    (back (analyzeIPAHelper "ɛ") ≡ Just (BackFeature Minus))
  runTest "The high feature is − for [ɛ]."
    (high (analyzeIPAHelper "ɛ") ≡ Just (HighFeature Minus))
  runTest "The low feature is − for [ɛ]."
    (low (analyzeIPAHelper "ɛ") ≡ Just (LowFeature Minus))
  runTest "The ATR feature is − for [ɛ]."
    (atr (analyzeIPAHelper "ɛ") ≡ Just (AdvancedTongueRootFeature Minus))
  runTest "The round feature is − for [ɛ]."
    (round (analyzeIPAHelper "ɛ") ≡ Just (RoundFeature Minus))
    
    

  runTest "The back feature is − for [æ]."
    (back (analyzeIPAHelper "æ") ≡ Just (BackFeature Minus))
  runTest "The high feature is − for [æ]."
    (high (analyzeIPAHelper "æ") ≡ Just (HighFeature Minus))
  runTest "The low feature is + for [æ]."
    (low (analyzeIPAHelper "æ") ≡ Just (LowFeature Plus))
  runTest "The ATR feature is − for [æ]." -- It has a parentheses
    (atr (analyzeIPAHelper "æ") ≡ Just (AdvancedTongueRootFeature Minus))
  runTest "The round feature is − for [æ]."
    (round (analyzeIPAHelper "æ") ≡ Just (RoundFeature Minus))
    
 
  runTest "The back feature is + for [u]."
    (back (analyzeIPAHelper "u") ≡ Just (BackFeature Plus))
  runTest "The high feature is + for [u]."
    (high (analyzeIPAHelper "u") ≡ Just (HighFeature Plus))
  runTest "The low feature is − for [u]."
    (low (analyzeIPAHelper "u") ≡ Just (LowFeature Minus))
  runTest "The ATR feature is + for [u]."
    (atr (analyzeIPAHelper "u") ≡ Just (AdvancedTongueRootFeature Plus))
  runTest "The round feature is + for [u]."
    (round (analyzeIPAHelper "u") ≡ Just (RoundFeature Plus))





  runTest "The back feature is + for [ʊ]."
    (back (analyzeIPAHelper "ʊ") ≡ Just (BackFeature Plus))
  runTest "The high feature is + for [ʊ]."
    (high (analyzeIPAHelper "ʊ") ≡ Just (HighFeature Plus))
  runTest "The low feature is − for [ʊ]."
    (low (analyzeIPAHelper "ʊ") ≡ Just (LowFeature Minus))
  runTest "The ATR feature is − for [ʊ]."
    (atr (analyzeIPAHelper "ʊ") ≡ Just (AdvancedTongueRootFeature Minus))
  runTest "The round feature is + for [ʊ]."
    (round (analyzeIPAHelper "ʊ") ≡ Just (RoundFeature Plus))



  runTest "The back feature is + for [o]."
    (back (analyzeIPAHelper "o") ≡ Just (BackFeature Plus))
  runTest "The high feature is − for [o]."
    (high (analyzeIPAHelper "o") ≡ Just (HighFeature Minus))
  runTest "The low feature is − for [o]."
    (low (analyzeIPAHelper "o") ≡ Just (LowFeature Minus))
  runTest "The ATR feature is + for [o]."
    (atr (analyzeIPAHelper "o") ≡ Just (AdvancedTongueRootFeature Plus))
  runTest "The round feature is + for [o]."
    (round (analyzeIPAHelper "o") ≡ Just (RoundFeature Plus))



  runTest "The back feature is + for [ɔ]."
    (back (analyzeIPAHelper "ɔ") ≡ Just (BackFeature Plus))
  runTest "The high feature is − for [ɔ]."
    (high (analyzeIPAHelper "ɔ") ≡ Just (HighFeature Minus))
  runTest "The low feature is − for [ɔ]."
    (low (analyzeIPAHelper "ɔ") ≡ Just (LowFeature Minus))
  runTest "The ATR feature is − for [ɔ]."
    (atr (analyzeIPAHelper "ɔ") ≡ Just (AdvancedTongueRootFeature Minus))
  runTest "The round feature is + for [ɔ]."
    (round (analyzeIPAHelper "ɔ") ≡ Just (RoundFeature Plus))




  runTest "The back feature is + for [ɑ]."
    (back (analyzeIPAHelper "ɑ") ≡ Just (BackFeature Plus))
  runTest "The high feature is − for [ɑ]."
    (high (analyzeIPAHelper "ɑ") ≡ Just (HighFeature Minus))
  runTest "The low feature is + for [ɑ]."
    (low (analyzeIPAHelper "ɑ") ≡ Just (LowFeature Plus))
  runTest "The ATR feature is − for [ɑ]."
    (atr (analyzeIPAHelper "ɑ") ≡ Just (AdvancedTongueRootFeature Minus)) -- In brackets
  runTest "The round feature is − for [ɑ]."
    (round (analyzeIPAHelper "ɑ") ≡ Just (RoundFeature Minus))




  runTest "The back feature is − for [y]."
    (back (analyzeIPAHelper "y") ≡ Just (BackFeature Minus))
  runTest "The high feature is + for [y]."
    (high (analyzeIPAHelper "y") ≡ Just (HighFeature Plus))
  runTest "The low feature is − for [y]."
    (low (analyzeIPAHelper "y") ≡ Just (LowFeature Minus))
  runTest "The ATR feature is + for [y]."
    (atr (analyzeIPAHelper "y") ≡ Just (AdvancedTongueRootFeature Plus))
  runTest "The round feature is + for [y]."
    (round (analyzeIPAHelper "y") ≡ Just (RoundFeature Plus))



  runTest "The back feature is − for [ø]."
    (back (analyzeIPAHelper "ø") ≡ Just (BackFeature Minus))
  runTest "The high feature is − for [ø]."
    (high (analyzeIPAHelper "ø") ≡ Just (HighFeature Minus))
  runTest "The low feature is − for [ø]."
    (low (analyzeIPAHelper "ø") ≡ Just (LowFeature Minus))
  runTest "The ATR feature is + for [ø]."
    (atr (analyzeIPAHelper "ø") ≡ Just (AdvancedTongueRootFeature Plus))
  runTest "The round feature is + for [ø]."
    (round (analyzeIPAHelper "ø") ≡ Just (RoundFeature Plus))



  runTest "The back feature is + for [ɨ]."
    (back (analyzeIPAHelper "ɨ") ≡ Just (BackFeature Plus))
  runTest "The high feature is + for [ɨ]."
    (high (analyzeIPAHelper "ɨ") ≡ Just (HighFeature Plus))
  runTest "The low feature is − for [ɨ]."
    (low (analyzeIPAHelper "ɨ") ≡ Just (LowFeature Minus))
  runTest "The ATR feature is − for [ɨ]."  -- In parentheses
    (atr (analyzeIPAHelper "ɨ") ≡ Just (AdvancedTongueRootFeature Minus))
  runTest "The round feature is − for [ɨ]."
    (round (analyzeIPAHelper "ɨ") ≡ Just (RoundFeature Minus))
    
    
 
  runTest "The back feature is + for [ʌ]."
    (back (analyzeIPAHelper "ʌ") ≡ Just (BackFeature Plus))
  runTest "The high feature is − for [ʌ]."
    (high (analyzeIPAHelper "ʌ") ≡ Just (HighFeature Minus))
  runTest "The low feature is − for [ʌ]."
    (low (analyzeIPAHelper "ʌ") ≡ Just (LowFeature Minus))
  runTest "The ATR feature is − for [ʌ]."
    (atr (analyzeIPAHelper "ʌ") ≡ Just (AdvancedTongueRootFeature Minus)) -- in parentheses
  runTest "The round feature is − for [ʌ]."
    (round (analyzeIPAHelper "ʌ") ≡ Just (RoundFeature Minus))


  runTest "The ATR feature is nothing for [z]."
    (atr (analyzeIPAHelper "z") ≡ Nothing)
  runTest "The ATR feature is nothing for [p]."
    (  atr (analyzeIPAHelper "p") ≡ Nothing)


analyzeIPAHelper = fromJust ∘ analyzeIPA