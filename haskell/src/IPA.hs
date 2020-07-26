{-# LANGUAGE OverloadedStrings #-}
module IPA where

import Prelude ()
import Relude ((+), (.), (==), NonEmpty((:|)), Maybe(Just, Nothing), Text, (<), (<>),  Natural,
               fromMaybe, map, fmap, sconcat, maybe, not, zip)

import qualified Data.Text     as T

import EnglishUSText
import Lib_Types (Phonet(Consonant, Vowel), VocalFolds(..), Place(..), Manner(..), Airstream(..),
                      Height(..), Backness(..), Rounding(..), PhonetInventory(..))
import Lib_Functions (showPhonet,
  spirantizedPhonet, devoicedPhonet,
  voicedPhonet, decreak, deaspirate,
  retractPhonet)

import PhoneticFeatures(showFeatures, analyzeFeatures)
import           LanguageSpecific.EnglishSpecific (englishPhonetInventory)

import GraphemeGrammar(splitByPhonetes, isDescender)

englishPhonetInventoryReport :: Text
englishPhonetInventoryReport = ipaTextToPhonetListReport (showIPA englishPhonetInventory)

analyzeIPAToSPE :: Text -> Text
analyzeIPAToSPE ipaText =
  maybe sorryUnableToCalculate (showFeatures . analyzeFeatures) (analyzeIPA ipaText)





-- | Given text containing international phonetic alphabet symbols
--   returns text with every phonetic alphabet symbol or sequence
--   of symbols for a sound
--   followed by the description of the sound it represents.
ipaTextToPhonetListReport :: Text -> Text
ipaTextToPhonetListReport text =
  let listA = ipaTextToPhonetList text
   in T.unlines (map ipaAndPhonetFormat listA)

ipaAndPhonetFormat :: (Text, Maybe Phonet) -> Text
ipaAndPhonetFormat (ipaText, phonet) =
  "/" <> ipaText <> "/" <> " " <> phonetSummary
  where
    phonetSummary =
      maybe "(n/a)" showPhonet phonet

ipaTextToPhonetList :: Text -> [(Text, Maybe Phonet)]
ipaTextToPhonetList text =
  let ipaChunks = splitByPhonetes text
      phonetes = map analyzeIPA ipaChunks
   in zip ipaChunks phonetes

-- | This function will allow us to convert an IPA symbol
--   to its analyzed form (its phonetic features)
analyzeIPA :: Text -> Maybe Phonet
-- Plosives:
analyzeIPA p = case p of
  "p" -> Just (Consonant Voiceless Bilabial Plosive PulmonicEgressive)
  "b" -> Just (Consonant Voiced Bilabial Plosive PulmonicEgressive)
  "t" -> Just (Consonant Voiceless Alveolar Plosive PulmonicEgressive)
  "d" -> Just (Consonant Voiced Alveolar Plosive PulmonicEgressive)
  "ʈ" -> Just (Consonant Voiceless Retroflex Plosive PulmonicEgressive)
  "ɖ" -> Just (Consonant Voiced Retroflex Plosive PulmonicEgressive)
  "c" -> Just (Consonant Voiceless Palatal Plosive PulmonicEgressive)
  "ɟ" -> Just (Consonant Voiced Palatal Plosive PulmonicEgressive)
  "k" -> Just (Consonant Voiceless Velar Plosive PulmonicEgressive)
  "g" -> Just (Consonant Voiced Velar Plosive PulmonicEgressive)
  "q" -> Just (Consonant Voiceless Uvular Plosive PulmonicEgressive)
  "ɢ" -> Just (Consonant Voiced Uvular Plosive PulmonicEgressive)
  "ʔ" -> Just (Consonant Voiceless Glottal Plosive PulmonicEgressive)
  -- Nasals:
  "m" -> Just (Consonant Voiced Bilabial Nasal PulmonicEgressive)
  "ɱ" -> Just (Consonant Voiced LabioDental Nasal PulmonicEgressive)
  "n" -> Just (Consonant Voiced Alveolar Nasal PulmonicEgressive)
  "ɳ" -> Just (Consonant Voiced Retroflex Nasal PulmonicEgressive)
  "ɲ" -> Just (Consonant Voiced Palatal Nasal PulmonicEgressive)
  "ŋ" -> Just (Consonant Voiced Velar Nasal PulmonicEgressive)
  "ɴ" -> Just (Consonant Voiced Uvular Nasal PulmonicEgressive)
  -- Trills:
  "ʙ" -> Just (Consonant Voiced Bilabial Trill PulmonicEgressive)
  "r" -> Just (Consonant Voiced Alveolar Trill PulmonicEgressive)
  "ʀ" -> Just (Consonant Voiced Uvular Trill PulmonicEgressive)
  -- Taps or flaps:
  "ⱱ" -> Just (Consonant Voiced LabioDental TapOrFlap PulmonicEgressive)
  "ɾ" -> Just (Consonant Voiced Alveolar TapOrFlap PulmonicEgressive)
  "ɽ" -> Just (Consonant Voiced Retroflex TapOrFlap PulmonicEgressive)
  -- Fricatives:
  "ɸ" -> Just (Consonant Voiceless Bilabial Fricative PulmonicEgressive)
  "β" -> Just (Consonant Voiced Bilabial Fricative PulmonicEgressive)
  "f" -> Just (Consonant Voiceless LabioDental Fricative PulmonicEgressive)
  "v" -> Just (Consonant Voiced LabioDental Fricative PulmonicEgressive)
  "θ" -> Just (Consonant Voiceless Dental Fricative PulmonicEgressive)
  "ð" -> Just (Consonant Voiced Dental Fricative PulmonicEgressive)
  "s" -> Just (Consonant Voiceless Alveolar Fricative PulmonicEgressive)
  "z" -> Just (Consonant Voiced Alveolar Fricative PulmonicEgressive)
  "ʃ" -> Just (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive)
  "ʒ" -> Just (Consonant Voiced PostAlveolar Fricative PulmonicEgressive)
  "ʂ" -> Just (Consonant Voiceless Retroflex Fricative PulmonicEgressive)
  "ʐ" -> Just (Consonant Voiced Retroflex Fricative PulmonicEgressive)
  "ç" -> Just (Consonant Voiceless Palatal Fricative PulmonicEgressive)
  "ʝ" -> Just (Consonant Voiced Palatal Fricative PulmonicEgressive)
  "x" -> Just (Consonant Voiceless Velar Fricative PulmonicEgressive)
  "ɣ" -> Just (Consonant Voiced Velar Fricative PulmonicEgressive)
  "χ" -> Just (Consonant Voiceless Uvular Fricative PulmonicEgressive)
  "ʁ" -> Just (Consonant Voiced Uvular Fricative PulmonicEgressive)
  "ħ" -> Just (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive)
  "ʕ" -> Just (Consonant Voiced Pharyngeal Fricative PulmonicEgressive)
  "h" -> Just (Consonant Voiceless Glottal Fricative PulmonicEgressive)
  "ɦ" -> Just (Consonant Voiced Glottal Fricative PulmonicEgressive)
  -- Lateral Fricatives:
  "ɬ" -> Just (Consonant Voiceless Alveolar LateralFricative PulmonicEgressive)
  "ɮ" -> Just (Consonant Voiced Alveolar LateralFricative PulmonicEgressive)
  -- Approximants:
  "ʋ" -> Just (Consonant Voiced LabioDental Approximant PulmonicEgressive)
  "ɹ" -> Just (Consonant Voiced Alveolar Approximant PulmonicEgressive)
  "ɻ" -> Just (Consonant Voiced Retroflex Approximant PulmonicEgressive)
  "j" -> Just (Consonant Voiced Palatal Approximant PulmonicEgressive)
  "ɰ" -> Just (Consonant Voiced Velar Approximant PulmonicEgressive)
  -- Lateral Approximants:
  "l" -> Just (Consonant Voiced Alveolar LateralApproximant PulmonicEgressive)
  "ɭ" -> Just (Consonant Voiced Retroflex LateralApproximant PulmonicEgressive)
  "ʎ" -> Just (Consonant Voiced Palatal LateralApproximant PulmonicEgressive)
  "ʟ" -> Just (Consonant Voiced Velar LateralApproximant PulmonicEgressive)
  -- Affricates
  "t͡ʃ" -> Just (Consonant Voiceless PostAlveolar Affricate PulmonicEgressive)
  "t͜ʃ" -> Just (Consonant Voiceless PostAlveolar Affricate PulmonicEgressive)
  "d͡ʒ" -> Just (Consonant Voiced PostAlveolar Affricate PulmonicEgressive)
  "d͜ʒ" -> Just (Consonant Voiced PostAlveolar Affricate PulmonicEgressive)
  -- We should probably enforce use of the tie-bar underneath, otherwise
  -- it would not be deterministic to determine whether two graphemes here
  -- represent affricates or a plosive followed by a fricative.

  -- Under the Other Symbols part of the IPA chart:

  "w" -> Just (Consonant Voiced LabialVelar Approximant PulmonicEgressive)
  "ʍ" -> Just (Consonant Voiceless LabialVelar Fricative PulmonicEgressive)
  "ɥ" -> Just (Consonant Voiced LabialPalatal Approximant PulmonicEgressive)
  "ʜ" -> Just (Consonant Voiceless Epiglottal Fricative PulmonicEgressive)
  "ʢ" -> Just (Consonant Voiced Epiglottal Fricative PulmonicEgressive)
  "ʡ" -> Just (Consonant Voiceless Epiglottal Plosive PulmonicEgressive)
  -- Is the epiglottal plosive voiceless? The IPA chart does not specify.
  "ɕ" -> Just (Consonant Voiceless AlveoloPalatal Fricative PulmonicEgressive)
  "ʑ" -> Just (Consonant Voiced AlveoloPalatal Fricative PulmonicEgressive)
  "ɺ" -> Just (Consonant Voiced Alveolar LateralFlap PulmonicEgressive)
  "ɧ" ->
    Just
      ( Consonant
          Voiceless
          (Places (PostAlveolar :| [Velar]))
          Fricative
          PulmonicEgressive
      )
  -- Other Consonants:
  "ʘ" -> Just (Consonant Voiceless Bilabial Plosive Click)
  "ǀ" -> Just (Consonant Voiceless Dental Plosive Click)
  "ǃ" -> Just (Consonant Voiceless Alveolar Plosive Click)
  --  "ǃ" could also be PostAlveolar.
  "ǂ" -> Just (Consonant Voiceless PalatoAlveolar Plosive Click)
  "ǁ" -> Just (Consonant Voiceless Alveolar Lateral Click)
  "ɓ" -> Just (Consonant Voiced Bilabial Plosive Implosive)
  "ɗ" -> Just (Consonant Voiced Dental Plosive Implosive)
  -- "ɗ" could also be Alveolar
  "ʄ" -> Just (Consonant Voiced Palatal Plosive Implosive)
  "ɠ" -> Just (Consonant Voiced Velar Plosive Implosive)
  "ʛ" -> Just (Consonant Voiced Uvular Plosive Implosive)
  -- Close Vowels:
  "i" -> Just (Vowel Close Front Unrounded Voiced)
  "y" -> Just (Vowel Close Front Rounded Voiced)
  "ɨ" -> Just (Vowel Close Central Unrounded Voiced)
  "ʉ" -> Just (Vowel Close Central Rounded Voiced)
  "ɯ" -> Just (Vowel Close Back Unrounded Voiced)
  "u" -> Just (Vowel Close Back Rounded Voiced)
  -- Near-close Vowels:
  "ɪ" -> Just (Vowel NearClose Front Unrounded Voiced)
  "ʏ" -> Just (Vowel NearClose Front Rounded Voiced)
  "ʊ" -> Just (Vowel NearClose Back Rounded Voiced)
  -- Close-mid Vowels:
  "e" -> Just (Vowel CloseMid Front Unrounded Voiced)
  "ø" -> Just (Vowel CloseMid Front Rounded Voiced)
  "ɘ" -> Just (Vowel CloseMid Central Unrounded Voiced)
  "ɵ" -> Just (Vowel CloseMid Central Rounded Voiced)
  "ɤ" -> Just (Vowel CloseMid Back Unrounded Voiced)
  "o" -> Just (Vowel CloseMid Back Rounded Voiced)
  -- Mid Vowels:
  "ə" -> Just (Vowel Mid Central Unrounded Voiced)
  -- Open-mid Vowels:
  "ɛ" -> Just (Vowel OpenMid Front Unrounded Voiced)
  "œ" -> Just (Vowel OpenMid Front Rounded Voiced)
  "ɜ" -> Just (Vowel OpenMid Central Unrounded Voiced)
  "ɞ" -> Just (Vowel OpenMid Central Rounded Voiced)
  "ʌ" -> Just (Vowel OpenMid Back Unrounded Voiced)
  "ɔ" -> Just (Vowel OpenMid Back Rounded Voiced)
  -- Near-open
  "æ" -> Just (Vowel NearOpen Front Unrounded Voiced)
  "ɐ" -> Just (Vowel NearOpen Central Unrounded Voiced)
  -- Open Vowels:
  "a" -> Just (Vowel Open Front Unrounded Voiced)
  "ɶ" -> Just (Vowel Open Front Rounded Voiced)
  "ɑ" -> Just (Vowel Open Back Unrounded Voiced)
  "ɒ" -> Just (Vowel Open Back Rounded Voiced)
  -- Handle Diacritics:
  ipaText | not (T.null ipaText) ->
    case [T.last ipaText] of
      "̥" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in case fullGrapheme of
              Just (Consonant _ place manner airstream) ->
                Just (Consonant Voiceless place manner airstream)
              Just (Vowel height backness rounding _) ->
                Just (Vowel height backness rounding Voiceless)
              _ ->
                Nothing
      "̊" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in case fullGrapheme of
              Just (Consonant _ place manner airstream) ->
                Just (Consonant Voiceless place manner airstream)
              Just (Vowel height backness rounding _) ->
                Just (Vowel height backness rounding Voiceless)
              _ ->
                Nothing

      "̬" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in case fullGrapheme of
              Just (Consonant _ place manner airstream) ->
                Just (Consonant Voiced place manner airstream)
              Just (Vowel height backness rounding _) ->
                Just (Vowel height backness rounding Voiced)
              _ ->
                Nothing
      "ʰ" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in case fullGrapheme of
              Just (Consonant Voiced place manner airstream) ->
                Just (Consonant VoicedAspirated place manner airstream)
              Just (Consonant Voiceless place manner airstream) ->
                Just (Consonant VoicelessAspirated place manner airstream)
              Just (Vowel height backness rounding voicing) ->
                Just (Vowel height backness rounding voicing)
              anythingElse ->
                anythingElse
      -- (About the preceding line:) It is strange but we will just
      -- do nothing if they give us an aspirated vowel.
      -- since we have no way to represent it in the type system.
      -- to do: determine
      -- if the idea of an aspirated vowel makes sense
      "̠" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in retractPhonet fullGrapheme
      _ -> Nothing -- not recognized.
  _ -> Nothing

constructIPA :: Phonet -> Text
constructIPA phoneme =
  fromMaybe "∅" (constructIPARecursive 3 0 phoneme)

constructIPARecursive :: Natural -> Natural -> Phonet -> Maybe Text
-- Plosives:
constructIPARecursive recursionLimit recursionLevel p = case p of
  _ | recursionLevel == recursionLimit -> Nothing
  (Consonant Voiceless Bilabial Plosive PulmonicEgressive) ->
    Just "p"
  (Consonant Voiced Bilabial Plosive PulmonicEgressive) ->
    Just "b"
  (Consonant Voiceless Alveolar Plosive PulmonicEgressive) ->
    Just "t"
  (Consonant Voiced Alveolar Plosive PulmonicEgressive) ->
    Just "d"
  (Consonant Voiceless Retroflex Plosive PulmonicEgressive) ->
    Just "ʈ"
  (Consonant Voiced Retroflex Plosive PulmonicEgressive) ->
    Just "ɖ"
  (Consonant Voiceless Palatal Plosive PulmonicEgressive) ->
    Just "c"
  (Consonant Voiced Palatal Plosive PulmonicEgressive) ->
    Just "ɟ"
  (Consonant Voiceless Velar Plosive PulmonicEgressive) ->
    Just "k"
  (Consonant Voiced Velar Plosive PulmonicEgressive) ->
    Just "g"
  (Consonant Voiceless Uvular Plosive PulmonicEgressive) ->
    Just "q"
  (Consonant Voiced Uvular Plosive PulmonicEgressive) ->
    Just "ɢ"
  (Consonant Voiceless Glottal Plosive PulmonicEgressive) ->
    Just "ʔ" -- Nasals (next line):
  (Consonant Voiced Bilabial Nasal PulmonicEgressive) ->
    Just "m"
  (Consonant Voiced LabioDental Nasal PulmonicEgressive) ->
    Just "ɱ"
  (Consonant Voiced Alveolar Nasal PulmonicEgressive) ->
    Just "n"
  (Consonant Voiced Retroflex Nasal PulmonicEgressive) ->
    Just "ɳ"
  (Consonant Voiced Palatal Nasal PulmonicEgressive) ->
    Just "ɲ"
  (Consonant Voiced Velar Nasal PulmonicEgressive) ->
    Just "ŋ"
  (Consonant Voiced Uvular Nasal PulmonicEgressive) ->
    Just "ɴ" -- Trills (next line):
  (Consonant Voiced Bilabial Trill PulmonicEgressive) ->
    Just "ʙ"
  (Consonant Voiced Alveolar Trill PulmonicEgressive) ->
    Just "r"
  (Consonant Voiced Uvular Trill PulmonicEgressive) ->
    Just "ʀ" -- Taps or flaps (next line):
  (Consonant Voiced LabioDental TapOrFlap PulmonicEgressive) ->
    Just "ⱱ"
  (Consonant Voiced Alveolar TapOrFlap PulmonicEgressive) ->
    Just "ɾ"
  (Consonant Voiced Retroflex TapOrFlap PulmonicEgressive) ->
    Just "ɽ" -- Fricatives (next line):
  (Consonant Voiceless Bilabial Fricative PulmonicEgressive) ->
    Just "ɸ"
  (Consonant Voiced Bilabial Fricative PulmonicEgressive) ->
    Just "β"
  (Consonant Voiceless LabioDental Fricative PulmonicEgressive) ->
    Just "f"
  (Consonant Voiced LabioDental Fricative PulmonicEgressive) ->
    Just "v"
  (Consonant Voiceless Dental Fricative PulmonicEgressive) ->
    Just "θ"
  (Consonant Voiced Dental Fricative PulmonicEgressive) ->
    Just "ð"
  (Consonant Voiceless Alveolar Fricative PulmonicEgressive) ->
    Just "s"
  (Consonant Voiced Alveolar Fricative PulmonicEgressive) ->
    Just "z"
  (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive) ->
    Just "ʃ"
  (Consonant Voiced PostAlveolar Fricative PulmonicEgressive) ->
    Just "ʒ"
  (Consonant Voiceless Retroflex Fricative PulmonicEgressive) ->
    Just "ʂ"
  (Consonant Voiced Retroflex Fricative PulmonicEgressive) ->
    Just "ʐ"
  (Consonant Voiceless Palatal Fricative PulmonicEgressive) ->
    Just "ç"
  (Consonant Voiced Palatal Fricative PulmonicEgressive) ->
    Just "ʝ"
  (Consonant Voiceless Velar Fricative PulmonicEgressive) ->
    Just "x"
  (Consonant Voiced Velar Fricative PulmonicEgressive) ->
    Just "ɣ"
  (Consonant Voiceless Uvular Fricative PulmonicEgressive) ->
    Just "χ"
  (Consonant Voiced Uvular Fricative PulmonicEgressive) ->
    Just "ʁ"
  (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive) ->
    Just "ħ"
  (Consonant Voiced Pharyngeal Fricative PulmonicEgressive) ->
    Just "ʕ"
  (Consonant Voiceless Glottal Fricative PulmonicEgressive) ->
    Just "h"
  (Consonant Voiced Glottal Fricative PulmonicEgressive) ->
    Just "ɦ" -- Lateral Fricatives (next line):
  (Consonant Voiceless Alveolar LateralFricative PulmonicEgressive) ->
    Just "ɬ"
  (Consonant Voiced Alveolar LateralFricative PulmonicEgressive) ->
    Just "ɮ" -- Approximants (next line):
  (Consonant Voiced LabioDental Approximant PulmonicEgressive) ->
    Just "ʋ"
  (Consonant Voiced Alveolar Approximant PulmonicEgressive) ->
    Just "ɹ"
  (Consonant Voiced Retroflex Approximant PulmonicEgressive) ->
    Just "ɻ"
  (Consonant Voiced Palatal Approximant PulmonicEgressive) ->
    Just "j"
  (Consonant Voiced Velar Approximant PulmonicEgressive) ->
    Just "ɰ" -- Lateral Approximants (next line):
  (Consonant Voiced Alveolar LateralApproximant PulmonicEgressive) ->
    Just "l"
  (Consonant Voiced Retroflex LateralApproximant PulmonicEgressive) ->
    Just "ɭ"
  (Consonant Voiced Palatal LateralApproximant PulmonicEgressive) ->
    Just "ʎ"
  (Consonant Voiced Velar LateralApproximant PulmonicEgressive) ->
    Just "ʟ" -- Affricates (next line)
  (Consonant Voiceless PostAlveolar Affricate PulmonicEgressive) ->
    Just "t͡ʃ"
  (Consonant Voiced PostAlveolar Affricate PulmonicEgressive) ->
    Just "d͡ʒ"
  (Consonant Voiceless Bilabial Affricate PulmonicEgressive) ->
    Just "p͡ɸ"
  (Consonant Voiceless Alveolar Affricate PulmonicEgressive) ->
    Just "t͜s"
  (Consonant Voiced Alveolar Affricate PulmonicEgressive) ->
    Just "d͡z"
  (Consonant Voiceless Velar Affricate PulmonicEgressive) ->
    Just "k͡x"
  (Consonant Voiceless Uvular Affricate PulmonicEgressive) ->
    Just "q͡χ" -- Under the Other Symbols part of the IPA chart:
  (Consonant Voiced LabialVelar Approximant PulmonicEgressive) ->
    Just "w"
  (Consonant Voiceless LabialVelar Fricative PulmonicEgressive) ->
    Just "ʍ"
  (Consonant Voiced LabialPalatal Approximant PulmonicEgressive) ->
    Just "ɥ"
  (Consonant Voiceless Epiglottal Fricative PulmonicEgressive) ->
    Just "ʜ"
  (Consonant Voiced Epiglottal Fricative PulmonicEgressive) ->
    Just "ʢ"
  (Consonant Voiceless Epiglottal Plosive PulmonicEgressive) ->
    Just "ʡ" -- Is the epiglottal plosive voiceless? The IPA chart
    -- does not specify.
  (Consonant Voiceless AlveoloPalatal Fricative PulmonicEgressive) ->
    Just "ɕ"
  (Consonant Voiced AlveoloPalatal Fricative PulmonicEgressive) ->
    Just "ʑ"
  (Consonant Voiced Alveolar LateralFlap PulmonicEgressive) ->
    Just "ɺ"
  ( Consonant
      Voiceless
      (Places (PostAlveolar :| [Velar]))
      Fricative
      PulmonicEgressive
    ) ->
      Just "ɧ" -- Other Consonants:
  (Consonant Voiceless Bilabial Plosive Click) ->
    Just "ʘ"
  (Consonant Voiceless Dental Plosive Click) ->
    Just "ǀ"
  (Consonant Voiceless Alveolar Plosive Click) ->
    Just "ǃ" -- Or it could be PostAlveolar.
  (Consonant Voiceless PalatoAlveolar Plosive Click) ->
    Just "ǂ"
  (Consonant Voiceless Alveolar Lateral Click) ->
    Just "ǁ"
  (Consonant Voiced Bilabial Plosive Implosive) ->
    Just "ɓ"
  (Consonant Voiced Dental Plosive Implosive) ->
    Just "ɗ" -- Or Alveolar
  (Consonant Voiced Palatal Plosive Implosive) ->
    Just "ʄ"
  (Consonant Voiced Velar Plosive Implosive) ->
    Just "ɠ"
  (Consonant Voiced Uvular Plosive Implosive) ->
    Just "ʛ" -- Close Vowels (next line):
  (Vowel Close Front Unrounded Voiced) ->
    Just "i"
  (Vowel Close Front Rounded Voiced) ->
    Just "y"
  (Vowel Close Central Unrounded Voiced) ->
    Just "ɨ"
  (Vowel Close Central Rounded Voiced) ->
    Just "ʉ"
  (Vowel Close Back Unrounded Voiced) ->
    Just "ɯ"
  (Vowel Close Back Rounded Voiced) ->
    Just "u" -- Near-close Vowels (next line):
  (Vowel NearClose Front Unrounded Voiced) ->
    Just "ɪ"
  (Vowel NearClose Front Rounded Voiced) ->
    Just "ʏ"
  (Vowel NearClose Back Rounded Voiced) ->
    Just "ʊ" -- Close-mid Vowels (next line):
  (Vowel CloseMid Front Unrounded Voiced) ->
    Just "e"
  (Vowel CloseMid Front Rounded Voiced) ->
    Just "ø"
  (Vowel CloseMid Central Unrounded Voiced) ->
    Just "ɘ"
  (Vowel CloseMid Central Rounded Voiced) ->
    Just "ɵ"
  (Vowel CloseMid Back Unrounded Voiced) ->
    Just "ɤ"
  (Vowel CloseMid Back Rounded Voiced) ->
    Just "o" -- Mid Vowels (next line):
  (Vowel Mid Central Unrounded Voiced) ->
    Just "ə" -- Open-mid Vowels (next line):
  (Vowel OpenMid Front Unrounded Voiced) ->
    Just "ɛ"
  (Vowel OpenMid Front Rounded Voiced) ->
    Just "œ"
  (Vowel OpenMid Central Unrounded Voiced) ->
    Just "ɜ"
  (Vowel OpenMid Central Rounded Voiced) ->
    Just "ɞ"
  (Vowel OpenMid Back Unrounded Voiced) ->
    Just "ʌ"
  (Vowel OpenMid Back Rounded Voiced) ->
    Just "ɔ" -- Near-open (next line)
  (Vowel NearOpen Front Unrounded Voiced) ->
    Just "æ"
  (Vowel NearOpen Central Unrounded Voiced) ->
    Just "ɐ" -- Open Vowels (next line):
  (Vowel Open Front Unrounded Voiced) ->
    Just "a"
  (Vowel Open Front Rounded Voiced) ->
    Just "ɶ"
  (Vowel Open Back Unrounded Voiced) ->
    Just "ɑ"
  (Vowel Open Back Rounded Voiced) ->
    Just "ɒ"
  -- The following two lines are commented out, because I am unsure
  -- about their place of articulation:
  -- constructIPARecursive _ _ (Consonant  Voiceless LabialVelar? Affricate
  --     PulmonicEgressive) = "k͡p"
  -- constructIPARecursive _ _ (Consonant  Voiceless Palatal (or AlveoloPalatal?)
  --     Affricate PulmonicEgressive) = "c͡ɕ"

  -- If it can represent it as a single character it will
  -- return the single character result (i.e. without diacritics),
  -- otherwise
  -- it will try to represent it in IPA with more than
  -- one character
  (Consonant x PostAlveolar y z)
    | recursionLevel < recursionLimit ->
      case constructIPARecursive
        recursionLimit
        (1 + recursionLevel)
        (Consonant x Alveolar y z) of
        Nothing         -> Nothing
        Just regularIPA -> Just (regularIPA <> "̠")
  -- Add the diacritic for "retracted"

  -- If there isn't a symbol, and the consonant we want is voiceless,
  -- Just take the symbol for a voiced consonant,
  -- and then put that diacritic that means voiceless after.
  -- (The following two definitions are intended to implement that)
  -- Add the small circle diacritic to consonants to make them voiceless.
  (Consonant Voiceless x y z)
    | recursionLevel < recursionLimit ->
      case constructIPARecursive
        recursionLimit
        (1 + recursionLevel)
        (Consonant Voiced x y z) of
        Nothing         -> Nothing
        Just regularIPA ->
                            Just (if isDescender regularIPA
                                 then regularIPA <> "̊"
                                 else regularIPA <> "̥")
  -- add diacritic for voiceless

  -- Add the small circle diacritic to vowels to make them voiceless.
  (Vowel x y z Voiceless)
    | recursionLevel < recursionLimit ->
      case constructIPARecursive
        recursionLimit
        (1 + recursionLevel)
        (Vowel x y z Voiced) of
        Nothing         -> Nothing
        Just regularIPA -> if isDescender regularIPA
                           then Just (regularIPA <> "̊")
                           else Just (regularIPA <> "̥")
  -- If there is no way to express a voiced consonant in a single
  -- grapheme add a diacritic to the grapheme that represents
  -- the voiceless counterpart.
  (Consonant Voiced x y z)
    | recursionLevel < recursionLimit ->
      case constructIPARecursive
        recursionLimit
        (1 + recursionLevel)
        (Consonant Voiceless x y z) of
        Nothing         -> Nothing
        Just regularIPA -> Just (regularIPA <> "̬")
  (Vowel x y z Voiced)
    | recursionLevel < recursionLimit ->
      case constructIPARecursive
        recursionLimit
        (1 + recursionLevel)
        (Vowel x y z Voiceless) of
        Nothing         -> Nothing
        Just regularIPA -> Just (regularIPA <> "̬")
  (Consonant VoicedAspirated _ _ PulmonicEgressive)
    | recursionLevel < recursionLimit ->
      let result =
            constructIPARecursive
              recursionLimit
              (1 + recursionLevel)
              (deaspirate p)
       in case result of
            Nothing         -> Nothing
            Just regularIPA -> Just (regularIPA <> "ʰ")
  (Consonant VoicelessAspirated _ _ PulmonicEgressive)
    | recursionLevel < recursionLimit ->
      let result =
            constructIPARecursive
              recursionLimit
              (1 + recursionLevel)
              (deaspirate p)
       in case result of
            Nothing         -> Nothing
            Just regularIPA -> Just (regularIPA <> "ʰ")
  (Consonant CreakyVoiced _ _ PulmonicEgressive)
    | recursionLevel < recursionLimit ->
      let result =
            constructIPARecursive
              recursionLimit
              (1 + recursionLevel)
              (decreak p)
       in case result of
            Just regularIPA -> Just (regularIPA <> "̰")
            Nothing         -> Nothing
  _ -> Nothing

constructDeconstruct :: (Phonet -> Phonet) -> Text -> Text
constructDeconstruct func x =
  let something = analyzeIPA x
   in case something of
        Nothing     -> "∅"
        Just phonet -> constructIPA (func phonet)

voicedIPA :: Text -> Text
voicedIPA = constructDeconstruct voicedPhonet

devoicedIPA :: Text -> Text
devoicedIPA = constructDeconstruct devoicedPhonet

spirantizedIPA :: Text -> Text
spirantizedIPA = constructDeconstruct spirantizedPhonet

-- |
-- Return an english description of a phoneme,
-- given a phoneme's representation in the
-- international phonetic alphabet.
describeIPA :: Text -> Text
describeIPA x =
  maybe noEnglishDescriptionFoundMessage showPhonet (analyzeIPA x)


showIPA :: PhonetInventory -> Text
showIPA (PhonetInventory phonetes) = sconcat (fmap constructIPA phonetes)

