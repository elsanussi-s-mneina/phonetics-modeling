{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module IPA where

import Relude ((+), (.), (==), (/=), (&&), NonEmpty((:|)), Maybe(Just, Nothing), Text, (<), (<>),  Natural,
               fromMaybe, map, fmap, sconcat, maybe, not, zip)

import qualified Data.Text     as T

import EnglishUSText
import Lib_Types (Phonet(Consonant, Vowel), VocalFolds(..), Place(..), Manner(..), Airstream(..),
                      SecondaryArticulation(..),
                      Height(..), Backness(..), Rounding(..), PhonetInventory(..),
                      VowelLength(ExtraShort, NormalLength, HalfLong, Long))
import Lib_PseudoLens (toExtraShort, toHalfLong, toLabialized, toLong,
                       toPalatalized, toPharyngealized, toVelarized,
                       toVoiced, toVoiceless, isConsonant)
import Lib_Functions (aspirate, showPhonet,
  spirantizedPhonet, devoicedPhonet,
  voicedPhonet, decreak, deaspirate,
  retractPhonet)

import PhoneticFeatures(showFeatures, analyzeFeatures)
import           LanguageSpecific.EnglishSpecific (englishPhonetInventory)

import GraphemeGrammar(splitIntoPhonemes, isDescenderText)


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
  let ipaChunks = splitIntoPhonemes text
      phonetes = map analyzeIPA ipaChunks
   in zip ipaChunks phonetes

-- | This function will allow us to convert an IPA symbol
--   to its analyzed form (its phonetic features)
analyzeIPA :: Text -> Maybe Phonet
-- Plosives:
analyzeIPA p = case p of
  "p" -> Just (Consonant Voiceless Bilabial Plosive PulmonicEgressive Normal)
  "b" -> Just (Consonant Voiced Bilabial Plosive PulmonicEgressive Normal)
  "t" -> Just (Consonant Voiceless Alveolar Plosive PulmonicEgressive Normal)
  "d" -> Just (Consonant Voiced Alveolar Plosive PulmonicEgressive Normal)
  "ʈ" -> Just (Consonant Voiceless Retroflex Plosive PulmonicEgressive Normal)
  "ɖ" -> Just (Consonant Voiced Retroflex Plosive PulmonicEgressive Normal)
  "c" -> Just (Consonant Voiceless Palatal Plosive PulmonicEgressive Normal)
  "ɟ" -> Just (Consonant Voiced Palatal Plosive PulmonicEgressive Normal)
  "k" -> Just (Consonant Voiceless Velar Plosive PulmonicEgressive Normal)
  "g" -> Just (Consonant Voiced Velar Plosive PulmonicEgressive Normal)
  "q" -> Just (Consonant Voiceless Uvular Plosive PulmonicEgressive Normal)
  "ɢ" -> Just (Consonant Voiced Uvular Plosive PulmonicEgressive Normal)
  "ʔ" -> Just (Consonant Voiceless Glottal Plosive PulmonicEgressive Normal)
  -- Nasals:
  "m" -> Just (Consonant Voiced Bilabial Nasal PulmonicEgressive Normal)
  "ɱ" -> Just (Consonant Voiced LabioDental Nasal PulmonicEgressive Normal)
  "n" -> Just (Consonant Voiced Alveolar Nasal PulmonicEgressive Normal)
  "ɳ" -> Just (Consonant Voiced Retroflex Nasal PulmonicEgressive Normal)
  "ɲ" -> Just (Consonant Voiced Palatal Nasal PulmonicEgressive Normal)
  "ŋ" -> Just (Consonant Voiced Velar Nasal PulmonicEgressive Normal)
  "ɴ" -> Just (Consonant Voiced Uvular Nasal PulmonicEgressive Normal)
  -- Trills:
  "ʙ" -> Just (Consonant Voiced Bilabial Trill PulmonicEgressive Normal)
  "r" -> Just (Consonant Voiced Alveolar Trill PulmonicEgressive Normal)
  "ʀ" -> Just (Consonant Voiced Uvular Trill PulmonicEgressive Normal)
  -- Taps or flaps:
  "ⱱ" -> Just (Consonant Voiced LabioDental TapOrFlap PulmonicEgressive Normal)
  "ɾ" -> Just (Consonant Voiced Alveolar TapOrFlap PulmonicEgressive Normal)
  "ɽ" -> Just (Consonant Voiced Retroflex TapOrFlap PulmonicEgressive Normal)
  -- Fricatives:
  "ɸ" -> Just (Consonant Voiceless Bilabial Fricative PulmonicEgressive Normal)
  "β" -> Just (Consonant Voiced Bilabial Fricative PulmonicEgressive Normal)
  "f" -> Just (Consonant Voiceless LabioDental Fricative PulmonicEgressive Normal)
  "v" -> Just (Consonant Voiced LabioDental Fricative PulmonicEgressive Normal)
  "θ" -> Just (Consonant Voiceless Dental Fricative PulmonicEgressive Normal)
  "ð" -> Just (Consonant Voiced Dental Fricative PulmonicEgressive Normal)
  "s" -> Just (Consonant Voiceless Alveolar Fricative PulmonicEgressive Normal)
  "z" -> Just (Consonant Voiced Alveolar Fricative PulmonicEgressive Normal)
  "ʃ" -> Just (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive Normal)
  "ʒ" -> Just (Consonant Voiced PostAlveolar Fricative PulmonicEgressive Normal)
  "ʂ" -> Just (Consonant Voiceless Retroflex Fricative PulmonicEgressive Normal)
  "ʐ" -> Just (Consonant Voiced Retroflex Fricative PulmonicEgressive Normal)
  "ç" -> Just (Consonant Voiceless Palatal Fricative PulmonicEgressive Normal)
  "ʝ" -> Just (Consonant Voiced Palatal Fricative PulmonicEgressive Normal)
  "x" -> Just (Consonant Voiceless Velar Fricative PulmonicEgressive Normal)
  "ɣ" -> Just (Consonant Voiced Velar Fricative PulmonicEgressive Normal)
  "χ" -> Just (Consonant Voiceless Uvular Fricative PulmonicEgressive Normal)
  "ʁ" -> Just (Consonant Voiced Uvular Fricative PulmonicEgressive Normal)
  "ħ" -> Just (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive Normal)
  "ʕ" -> Just (Consonant Voiced Pharyngeal Fricative PulmonicEgressive Normal)
  "h" -> Just (Consonant Voiceless Glottal Fricative PulmonicEgressive Normal)
  "ɦ" -> Just (Consonant Voiced Glottal Fricative PulmonicEgressive Normal)
  -- Lateral Fricatives:
  "ɬ" -> Just (Consonant Voiceless Alveolar LateralFricative PulmonicEgressive Normal)
  "ɮ" -> Just (Consonant Voiced Alveolar LateralFricative PulmonicEgressive Normal)
  -- Approximants:
  "ʋ" -> Just (Consonant Voiced LabioDental Approximant PulmonicEgressive Normal)
  "ɹ" -> Just (Consonant Voiced Alveolar Approximant PulmonicEgressive Normal)
  "ɻ" -> Just (Consonant Voiced Retroflex Approximant PulmonicEgressive Normal)
  "j" -> Just (Consonant Voiced Palatal Approximant PulmonicEgressive Normal)
  "ɰ" -> Just (Consonant Voiced Velar Approximant PulmonicEgressive Normal)
  -- Lateral Approximants:
  "l" -> Just (Consonant Voiced Alveolar LateralApproximant PulmonicEgressive Normal)
  "ɭ" -> Just (Consonant Voiced Retroflex LateralApproximant PulmonicEgressive Normal)
  "ʎ" -> Just (Consonant Voiced Palatal LateralApproximant PulmonicEgressive Normal)
  "ʟ" -> Just (Consonant Voiced Velar LateralApproximant PulmonicEgressive Normal)
  -- Affricates
  "t͡ʃ" -> Just (Consonant Voiceless PostAlveolar Affricate PulmonicEgressive Normal)
  "t͜ʃ" -> Just (Consonant Voiceless PostAlveolar Affricate PulmonicEgressive Normal)
  "d͡ʒ" -> Just (Consonant Voiced PostAlveolar Affricate PulmonicEgressive Normal)
  "d͜ʒ" -> Just (Consonant Voiced PostAlveolar Affricate PulmonicEgressive Normal)
  -- We should probably enforce use of the tie-bar underneath, otherwise
  -- it would not be deterministic to determine whether two graphemes here
  -- represent affricates or a plosive followed by a fricative.

  -- Under the Other Symbols part of the IPA chart:

  "w" -> Just (Consonant Voiced LabialVelar Approximant PulmonicEgressive Normal)
  "ʍ" -> Just (Consonant Voiceless LabialVelar Fricative PulmonicEgressive Normal)
  "ɥ" -> Just (Consonant Voiced LabialPalatal Approximant PulmonicEgressive Normal)
  "ʜ" -> Just (Consonant Voiceless Epiglottal Fricative PulmonicEgressive Normal)
  "ʢ" -> Just (Consonant Voiced Epiglottal Fricative PulmonicEgressive Normal)
  "ʡ" -> Just (Consonant Voiceless Epiglottal Plosive PulmonicEgressive Normal)
  -- Is the epiglottal plosive voiceless? The IPA chart does not specify.
  "ɕ" -> Just (Consonant Voiceless AlveoloPalatal Fricative PulmonicEgressive Normal)
  "ʑ" -> Just (Consonant Voiced AlveoloPalatal Fricative PulmonicEgressive Normal)
  "ɺ" -> Just (Consonant Voiced Alveolar LateralFlap PulmonicEgressive Normal)
  "ɧ" ->
    Just
      ( Consonant
          Voiceless
          (Places (PostAlveolar :| [Velar]))
          Fricative
          PulmonicEgressive
          Normal
      )
  -- Other Consonants:
  "ʘ" -> Just (Consonant Voiceless Bilabial Plosive Click Normal)
  "ǀ" -> Just (Consonant Voiceless Dental Plosive Click Normal)
  "ǃ" -> Just (Consonant Voiceless Alveolar Plosive Click Normal)
  --  "ǃ" could also be PostAlveolar.
  "ǂ" -> Just (Consonant Voiceless PalatoAlveolar Plosive Click Normal)
  "ǁ" -> Just (Consonant Voiceless Alveolar Lateral Click Normal)
  "ɓ" -> Just (Consonant Voiced Bilabial Plosive Implosive Normal)
  "ɗ" -> Just (Consonant Voiced Dental Plosive Implosive Normal)
  -- "ɗ" could also be Alveolar
  "ʄ" -> Just (Consonant Voiced Palatal Plosive Implosive Normal)
  "ɠ" -> Just (Consonant Voiced Velar Plosive Implosive Normal)
  "ʛ" -> Just (Consonant Voiced Uvular Plosive Implosive Normal)
  -- Close Vowels:
  "i" -> Just (Vowel Close Front Unrounded Voiced NormalLength)
  "y" -> Just (Vowel Close Front Rounded Voiced NormalLength)
  "ɨ" -> Just (Vowel Close Central Unrounded Voiced NormalLength)
  "ʉ" -> Just (Vowel Close Central Rounded Voiced NormalLength)
  "ɯ" -> Just (Vowel Close Back Unrounded Voiced NormalLength)
  "u" -> Just (Vowel Close Back Rounded Voiced NormalLength)
  -- Near-close Vowels:
  "ɪ" -> Just (Vowel NearClose Front Unrounded Voiced NormalLength)
  "ʏ" -> Just (Vowel NearClose Front Rounded Voiced NormalLength)
  "ʊ" -> Just (Vowel NearClose Back Rounded Voiced NormalLength)
  -- Close-mid Vowels:
  "e" -> Just (Vowel CloseMid Front Unrounded Voiced NormalLength)
  "ø" -> Just (Vowel CloseMid Front Rounded Voiced NormalLength)
  "ɘ" -> Just (Vowel CloseMid Central Unrounded Voiced NormalLength)
  "ɵ" -> Just (Vowel CloseMid Central Rounded Voiced NormalLength)
  "ɤ" -> Just (Vowel CloseMid Back Unrounded Voiced NormalLength)
  "o" -> Just (Vowel CloseMid Back Rounded Voiced NormalLength)
  -- Mid Vowels:
  "ə" -> Just (Vowel Mid Central Unrounded Voiced NormalLength)
  -- Open-mid Vowels:
  "ɛ" -> Just (Vowel OpenMid Front Unrounded Voiced NormalLength)
  "œ" -> Just (Vowel OpenMid Front Rounded Voiced NormalLength)
  "ɜ" -> Just (Vowel OpenMid Central Unrounded Voiced NormalLength)
  "ɞ" -> Just (Vowel OpenMid Central Rounded Voiced NormalLength)
  "ʌ" -> Just (Vowel OpenMid Back Unrounded Voiced NormalLength)
  "ɔ" -> Just (Vowel OpenMid Back Rounded Voiced NormalLength)
  -- Near-open
  "æ" -> Just (Vowel NearOpen Front Unrounded Voiced NormalLength)
  "ɐ" -> Just (Vowel NearOpen Central Unrounded Voiced NormalLength)
  -- Open Vowels:
  "a" -> Just (Vowel Open Front Unrounded Voiced NormalLength)
  "ɶ" -> Just (Vowel Open Front Rounded Voiced NormalLength)
  "ɑ" -> Just (Vowel Open Back Unrounded Voiced NormalLength)
  "ɒ" -> Just (Vowel Open Back Rounded Voiced NormalLength)
  -- Handle Diacritics:
  ipaText | not (T.null ipaText) ->
    case [T.last ipaText] of
      "̥" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in case fullGrapheme of
              Just x -> Just (toVoiceless x)
              _      -> Nothing
      "̊" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in case fullGrapheme of
              Just x -> Just (toVoiceless x)
              _      -> Nothing

      "̬" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in case fullGrapheme of
              Just x -> Just (toVoiced x)
              _      -> Nothing
      "ʷ" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in case fullGrapheme of
              Just x -> Just (toLabialized x)
              _      -> Nothing

      "ʲ" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in case fullGrapheme of
              Just x -> Just (toPalatalized x)
              _      -> Nothing

      "ˠ" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in case fullGrapheme of
              Just x -> Just (toVelarized x)
              _      -> Nothing

      "ˤ" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in case fullGrapheme of
              Just x -> Just (toPharyngealized x)
              _      -> Nothing
      "ː" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in case fullGrapheme of
              Just x -> Just (toLong x)
              _      -> Nothing
      "ˑ" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in case fullGrapheme of
              Just x -> Just (toHalfLong x)
              _      -> Nothing
      "̆" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in case fullGrapheme of
              Just x -> Just (toExtraShort x)
              _      -> Nothing

      "ʰ" ->
        let fullGrapheme = analyzeIPA (T.init ipaText)
         in case fullGrapheme of
              Just x -> Just (aspirate x)
              _      -> Nothing
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

-- | convert a secondary articulation to its IPA text representation
secondaryArticulationIPA :: SecondaryArticulation -> Text
secondaryArticulationIPA articulation =
  case articulation of
     Normal     -> ""
     Palatalized -> "ʲ"
     Labialized -> "ʷ"
     Velarized  -> "ˠ"
     Pharyngealized -> "ˤ"

vowelLengthIPA :: VowelLength -> Text
vowelLengthIPA vowelLength =
  case vowelLength of
    NormalLength -> ""
    ExtraShort -> "̆"
    HalfLong -> "ˑ"
    Long -> "ː"

constructIPARecursive :: Natural -> Natural -> Phonet -> Maybe Text
-- Plosives:
constructIPARecursive recursionLimit recursionLevel p =
  case p of
    _ | recursionLevel == recursionLimit -> Nothing
    _| isConsonant p -> constructIPAConsonant recursionLimit recursionLevel p
    _                -> constructIPAVowel recursionLimit recursionLevel p

-- When you know it is a consonant
constructIPAConsonant :: Natural -> Natural -> Phonet -> Maybe Text
constructIPAConsonant recursionLimit recursionLevel p = case p of
  (Consonant Voiceless Bilabial Plosive PulmonicEgressive sa) ->
    Just ("p" <> secondaryArticulationIPA sa)
  (Consonant Voiced Bilabial Plosive PulmonicEgressive sa) ->
    Just ("b" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Alveolar Plosive PulmonicEgressive sa) ->
    Just ("t" <> secondaryArticulationIPA sa)
  (Consonant Voiced Alveolar Plosive PulmonicEgressive sa) ->
    Just ("d" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Retroflex Plosive PulmonicEgressive sa) ->
    Just ("ʈ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Retroflex Plosive PulmonicEgressive sa) ->
    Just ("ɖ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Palatal Plosive PulmonicEgressive sa) ->
    Just ("c" <> secondaryArticulationIPA sa)
  (Consonant Voiced Palatal Plosive PulmonicEgressive sa) ->
    Just ("ɟ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Velar Plosive PulmonicEgressive sa) ->
    Just ("k" <> secondaryArticulationIPA sa)
  (Consonant Voiced Velar Plosive PulmonicEgressive sa) ->
    Just ("g" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Uvular Plosive PulmonicEgressive sa) ->
    Just ("q" <> secondaryArticulationIPA sa)
  (Consonant Voiced Uvular Plosive PulmonicEgressive sa) ->
    Just ("ɢ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Glottal Plosive PulmonicEgressive sa) ->
    Just ("ʔ" <> secondaryArticulationIPA sa) -- Nasals (next line):
  (Consonant Voiced Bilabial Nasal PulmonicEgressive sa) ->
    Just ("m" <> secondaryArticulationIPA sa)
  (Consonant Voiced LabioDental Nasal PulmonicEgressive sa) ->
    Just ("ɱ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Alveolar Nasal PulmonicEgressive sa) ->
    Just ("n" <> secondaryArticulationIPA sa)
  (Consonant Voiced Retroflex Nasal PulmonicEgressive sa) ->
    Just ("ɳ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Palatal Nasal PulmonicEgressive sa) ->
    Just ("ɲ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Velar Nasal PulmonicEgressive sa) ->
    Just ("ŋ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Uvular Nasal PulmonicEgressive sa) ->
    Just ("ɴ" <> secondaryArticulationIPA sa) -- Trills (next line):
  (Consonant Voiced Bilabial Trill PulmonicEgressive sa) ->
    Just ("ʙ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Alveolar Trill PulmonicEgressive sa) ->
    Just ("r" <> secondaryArticulationIPA sa)
  (Consonant Voiced Uvular Trill PulmonicEgressive sa) ->
    Just ("ʀ" <> secondaryArticulationIPA sa) -- Taps or flaps (next line):
  (Consonant Voiced LabioDental TapOrFlap PulmonicEgressive sa) ->
    Just ("ⱱ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Alveolar TapOrFlap PulmonicEgressive sa) ->
    Just ("ɾ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Retroflex TapOrFlap PulmonicEgressive sa) ->
    Just ("ɽ" <> secondaryArticulationIPA sa) -- Fricatives (next line):
  (Consonant Voiceless Bilabial Fricative PulmonicEgressive sa) ->
    Just ("ɸ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Bilabial Fricative PulmonicEgressive sa) ->
    Just ("β" <> secondaryArticulationIPA sa)
  (Consonant Voiceless LabioDental Fricative PulmonicEgressive sa) ->
    Just ("f" <> secondaryArticulationIPA sa)
  (Consonant Voiced LabioDental Fricative PulmonicEgressive sa) ->
    Just ("v" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Dental Fricative PulmonicEgressive sa) ->
    Just ("θ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Dental Fricative PulmonicEgressive sa) ->
    Just ("ð" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Alveolar Fricative PulmonicEgressive sa) ->
    Just ("s" <> secondaryArticulationIPA sa)
  (Consonant Voiced Alveolar Fricative PulmonicEgressive sa) ->
    Just ("z" <> secondaryArticulationIPA sa)
  (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive sa) ->
    Just ("ʃ" <> secondaryArticulationIPA sa)
  (Consonant Voiced PostAlveolar Fricative PulmonicEgressive sa) ->
    Just ("ʒ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Retroflex Fricative PulmonicEgressive sa) ->
    Just ("ʂ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Retroflex Fricative PulmonicEgressive sa) ->
    Just ("ʐ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Palatal Fricative PulmonicEgressive sa) ->
    Just ("ç" <> secondaryArticulationIPA sa)
  (Consonant Voiced Palatal Fricative PulmonicEgressive sa) ->
    Just ("ʝ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Velar Fricative PulmonicEgressive sa) ->
    Just ("x" <> secondaryArticulationIPA sa)
  (Consonant Voiced Velar Fricative PulmonicEgressive sa) ->
    Just ("ɣ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Uvular Fricative PulmonicEgressive sa) ->
    Just ("χ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Uvular Fricative PulmonicEgressive sa) ->
    Just ("ʁ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive sa) ->
    Just ("ħ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Pharyngeal Fricative PulmonicEgressive sa) ->
    Just ("ʕ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Glottal Fricative PulmonicEgressive sa) ->
    Just ("h" <> secondaryArticulationIPA sa)
  (Consonant Voiced Glottal Fricative PulmonicEgressive sa) ->
    Just ("ɦ" <> secondaryArticulationIPA sa) -- Lateral Fricatives (next line):
  (Consonant Voiceless Alveolar LateralFricative PulmonicEgressive sa) ->
    Just ("ɬ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Alveolar LateralFricative PulmonicEgressive sa) ->
    Just ("ɮ" <> secondaryArticulationIPA sa)-- Approximants (next line):
  (Consonant Voiced LabioDental Approximant PulmonicEgressive sa) ->
    Just ("ʋ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Alveolar Approximant PulmonicEgressive sa) ->
    Just ("ɹ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Retroflex Approximant PulmonicEgressive sa) ->
    Just ("ɻ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Palatal Approximant PulmonicEgressive sa) ->
    Just ("j" <> secondaryArticulationIPA sa)
  (Consonant Voiced Velar Approximant PulmonicEgressive sa) ->
    Just ("ɰ" <> secondaryArticulationIPA sa) -- Lateral Approximants (next line):
  (Consonant Voiced Alveolar LateralApproximant PulmonicEgressive sa) ->
    Just ("l" <> secondaryArticulationIPA sa)
  (Consonant Voiced Retroflex LateralApproximant PulmonicEgressive sa) ->
    Just ("ɭ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Palatal LateralApproximant PulmonicEgressive sa) ->
    Just ("ʎ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Velar LateralApproximant PulmonicEgressive sa) ->
    Just ("ʟ" <> secondaryArticulationIPA sa) -- Affricates (next line)
  (Consonant Voiceless PostAlveolar Affricate PulmonicEgressive sa) ->
    Just ("t͡ʃ" <> secondaryArticulationIPA sa)
  (Consonant Voiced PostAlveolar Affricate PulmonicEgressive sa) ->
    Just ("d͡ʒ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Bilabial Affricate PulmonicEgressive sa) ->
    Just ("p͡ɸ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Alveolar Affricate PulmonicEgressive sa) ->
    Just ("t͜s" <> secondaryArticulationIPA sa)
  (Consonant Voiced Alveolar Affricate PulmonicEgressive sa) ->
    Just ("d͡z" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Velar Affricate PulmonicEgressive sa) ->
    Just ("k͡x" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Uvular Affricate PulmonicEgressive sa) ->
    Just ("q͡χ" <> secondaryArticulationIPA sa) -- Under the Other Symbols part of the IPA chart:
  (Consonant Voiced LabialVelar Approximant PulmonicEgressive sa) ->
    Just ("w" <> secondaryArticulationIPA sa)
  (Consonant Voiceless LabialVelar Fricative PulmonicEgressive sa) ->
    Just ("ʍ" <> secondaryArticulationIPA sa)
  (Consonant Voiced LabialPalatal Approximant PulmonicEgressive sa) ->
    Just ("ɥ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Epiglottal Fricative PulmonicEgressive sa) ->
    Just ("ʜ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Epiglottal Fricative PulmonicEgressive sa) ->
    Just ("ʢ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Epiglottal Plosive PulmonicEgressive sa) ->
    Just ("ʡ" <> secondaryArticulationIPA sa) -- Is the epiglottal plosive voiceless? The IPA chart
    -- does not specify.
  (Consonant Voiceless AlveoloPalatal Fricative PulmonicEgressive sa) ->
    Just ("ɕ" <> secondaryArticulationIPA sa)
  (Consonant Voiced AlveoloPalatal Fricative PulmonicEgressive sa) ->
    Just ("ʑ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Alveolar LateralFlap PulmonicEgressive sa) ->
    Just ("ɺ" <> secondaryArticulationIPA sa)
  ( Consonant
      Voiceless
      (Places (PostAlveolar :| [Velar]))
      Fricative
      PulmonicEgressive
      sa
    ) ->
      Just ("ɧ" <> secondaryArticulationIPA sa) -- Other Consonants:
  (Consonant Voiceless Bilabial Plosive Click sa) ->
    Just ("ʘ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Dental Plosive Click sa) ->
    Just ("ǀ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Alveolar Plosive Click sa) ->
    Just ("ǃ" <> secondaryArticulationIPA sa) -- Or it could be PostAlveolar.
  (Consonant Voiceless PalatoAlveolar Plosive Click sa) ->
    Just ("ǂ" <> secondaryArticulationIPA sa)
  (Consonant Voiceless Alveolar Lateral Click sa) ->
    Just ("ǁ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Bilabial Plosive Implosive sa) ->
    Just ("ɓ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Dental Plosive Implosive sa) ->
    Just ("ɗ" <> secondaryArticulationIPA sa) -- Or Alveolar
  (Consonant Voiced Palatal Plosive Implosive sa) ->
    Just ("ʄ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Velar Plosive Implosive sa) ->
    Just ("ɠ" <> secondaryArticulationIPA sa)
  (Consonant Voiced Uvular Plosive Implosive sa) ->
    Just ("ʛ" <> secondaryArticulationIPA sa)
  (Consonant x PostAlveolar y z sa)
    | recursionLevel < recursionLimit ->
      case constructIPARecursive
        recursionLimit
        (1 + recursionLevel)
        (Consonant x Alveolar y z sa) of
        Nothing         -> Nothing
        Just regularIPA -> Just (regularIPA <> "̠")
  -- Add the diacritic for "retracted"
  -- If there isn't a symbol, and the consonant we want is voiceless,
  -- Just take the symbol for a voiced consonant,
  -- and then put that diacritic that means voiceless after.
  -- (The following two definitions are intended to implement that)
  -- Add the small circle diacritic to consonants to make them voiceless.
  (Consonant Voiceless x y z sa)
    | recursionLevel < recursionLimit ->
      case constructIPARecursive
        recursionLimit
        (1 + recursionLevel)
        (Consonant Voiced x y z sa) of
        Nothing         -> Nothing
        Just regularIPA ->
                            Just (if isDescenderText regularIPA
                                 then regularIPA <> "̊"
                                 else regularIPA <> "̥")
  -- add diacritic for voiceless
  -- If there is no way to express a voiced consonant in a single
  -- grapheme add a diacritic to the grapheme that represents
  -- the voiceless counterpart.
  (Consonant Voiced x y z sa)
    | recursionLevel < recursionLimit ->
      case constructIPARecursive
        recursionLimit
        (1 + recursionLevel)
        (Consonant Voiceless x y z sa) of
        Nothing         -> Nothing
        Just regularIPA -> Just (regularIPA <> "̬")
  (Consonant VoicedAspirated _ _ PulmonicEgressive Normal)
    | recursionLevel < recursionLimit ->
      let result =
            constructIPARecursive
              recursionLimit
              (1 + recursionLevel)
              (deaspirate p)
       in case result of
            Nothing         -> Nothing
            Just regularIPA -> Just (regularIPA <> "ʰ")
  (Consonant VoicelessAspirated _ _ PulmonicEgressive Normal)
    | recursionLevel < recursionLimit ->
      let result =
            constructIPARecursive
              recursionLimit
              (1 + recursionLevel)
              (deaspirate p)
       in case result of
            Nothing         -> Nothing
            Just regularIPA -> Just (regularIPA <> "ʰ")
  (Consonant CreakyVoiced _ _ PulmonicEgressive Normal)
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

constructIPACloseVowel :: Phonet -> Maybe Text
constructIPACloseVowel p = case p of
  -- Close Vowels (next line):
  (Vowel Close Front Unrounded Voiced NormalLength) ->
    Just "i"
  (Vowel Close Front Rounded Voiced NormalLength) ->
    Just "y"
  (Vowel Close Central Unrounded Voiced NormalLength) ->
    Just "ɨ"
  (Vowel Close Central Rounded Voiced NormalLength) ->
    Just "ʉ"
  (Vowel Close Back Unrounded Voiced NormalLength) ->
    Just "ɯ"
  (Vowel Close Back Rounded Voiced NormalLength) ->
    Just "u"
  _ -> Nothing

constructIPANearCloseVowel :: Phonet -> Maybe Text
constructIPANearCloseVowel p = case p of
  -- Near-close Vowels (next line):
  (Vowel NearClose Front Unrounded Voiced NormalLength) ->
    Just "ɪ"
  (Vowel NearClose Front Rounded Voiced NormalLength) ->
    Just "ʏ"
  (Vowel NearClose Back Rounded Voiced NormalLength) ->
    Just "ʊ"
  _ ->
    Nothing

constructIPACloseMidVowel :: Phonet -> Maybe Text
constructIPACloseMidVowel p = case p of
  -- Close-mid Vowels (next line):
  (Vowel CloseMid Front Unrounded Voiced NormalLength) ->
    Just "e"
  (Vowel CloseMid Front Rounded Voiced NormalLength) ->
    Just "ø"
  (Vowel CloseMid Central Unrounded Voiced NormalLength) ->
    Just "ɘ"
  (Vowel CloseMid Central Rounded Voiced NormalLength) ->
    Just "ɵ"
  (Vowel CloseMid Back Unrounded Voiced NormalLength) ->
    Just "ɤ"
  (Vowel CloseMid Back Rounded Voiced NormalLength) ->
    Just "o"
  _ ->
    Nothing


constructIPAMidVowel :: Phonet -> Maybe Text
constructIPAMidVowel p = case p of
  -- Mid Vowels (next line):
  (Vowel Mid Central Unrounded Voiced NormalLength) ->
    Just "ə"
  _ ->
    Nothing


constructIPAOpenMidVowel :: Phonet -> Maybe Text
constructIPAOpenMidVowel p = case p of
  -- Open-mid Vowels (next line):
  (Vowel OpenMid Front Unrounded Voiced NormalLength) ->
    Just "ɛ"
  (Vowel OpenMid Front Rounded Voiced NormalLength) ->
    Just "œ"
  (Vowel OpenMid Central Unrounded Voiced NormalLength) ->
    Just "ɜ"
  (Vowel OpenMid Central Rounded Voiced NormalLength) ->
    Just "ɞ"
  (Vowel OpenMid Back Unrounded Voiced NormalLength) ->
    Just "ʌ"
  (Vowel OpenMid Back Rounded Voiced NormalLength) ->
    Just "ɔ"
  _ ->
    Nothing


constructIPANearOpenVowel :: Phonet -> Maybe Text
constructIPANearOpenVowel p = case p of
  -- Near-open (next line):
  (Vowel NearOpen Front Unrounded Voiced NormalLength) ->
    Just "æ"
  (Vowel NearOpen Central Unrounded Voiced NormalLength) ->
    Just "ɐ"
  _ ->
    Nothing


constructIPAOpenVowel :: Phonet -> Maybe Text
constructIPAOpenVowel p = case p of
  -- Open Vowels (next line):
  (Vowel Open Front Unrounded Voiced NormalLength) ->
    Just "a"
  (Vowel Open Front Rounded Voiced NormalLength) ->
    Just "ɶ"
  (Vowel Open Back Unrounded Voiced NormalLength) ->
    Just "ɑ"
  (Vowel Open Back Rounded Voiced NormalLength) ->
    Just "ɒ"
  _ ->
    Nothing

-- | construct IPA vowel
constructIPAVowel :: Natural -> Natural -> Phonet -> Maybe Text
constructIPAVowel recursionLimit recursionLevel p =
  let closeVowelResult = constructIPACloseVowel p
      nearCloseVowelResult = constructIPANearCloseVowel p
      closeMidResult = constructIPACloseMidVowel p
      midResult = constructIPAMidVowel p
      openMidResult = constructIPAOpenMidVowel p
      nearOpenResult = constructIPANearOpenVowel p
      openResult = constructIPAOpenVowel p
  in case p of
    (Vowel Close _ _ _ _) | closeVowelResult /= Nothing -> closeVowelResult
    (Vowel NearClose _ _ _ _) | nearCloseVowelResult /= Nothing -> closeVowelResult
    (Vowel CloseMid _ _ _ _) | closeMidResult /= Nothing -> closeMidResult
    (Vowel Mid _ _ _ _) | midResult /= Nothing -> midResult
    (Vowel OpenMid _ _ _ _) | openMidResult /= Nothing -> openMidResult
    (Vowel NearOpen _ _ _ _) | nearOpenResult /= Nothing -> nearOpenResult
    (Vowel Open _ _ _ _) | openResult /= Nothing -> openResult

    -- If it can represent it as a single character it will
    -- return the single character result (i.e. without diacritics),
    -- otherwise
    -- it will try to represent it in IPA with more than
    -- one character

    (Vowel w x y z vowelLength)
      | vowelLength /= NormalLength
      && recursionLevel < recursionLimit ->
        case constructIPARecursive
          recursionLimit
          (1 + recursionLevel)
          (Vowel w x y z NormalLength) of
          Nothing         -> Nothing
          Just regularIPA -> Just (regularIPA <> vowelLengthIPA vowelLength)


    -- Add the small circle diacritic to vowels to make them voiceless.
    (Vowel x y z Voiceless vowelLength)
      | recursionLevel < recursionLimit ->
        case constructIPARecursive
          recursionLimit
          (1 + recursionLevel)
          (Vowel x y z Voiced vowelLength) of
          Nothing         -> Nothing
          Just regularIPA -> if isDescenderText regularIPA
                             then Just (regularIPA <> "̊")
                             else Just (regularIPA <> "̥")
    -- If there is no way to express a voiced consonant in a single
    -- grapheme add a diacritic to the grapheme that represents
    -- the voiceless counterpart.
    (Vowel x y z Voiced vowelLength)
      | recursionLevel < recursionLimit ->
        case constructIPARecursive
          recursionLimit
          (1 + recursionLevel)
          (Vowel x y z Voiceless vowelLength) of
          Nothing         -> Nothing
          Just regularIPA -> Just (regularIPA <> "̬")

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



  -- The following two lines are commented out, because I am unsure
  -- about their place of articulation:
  -- constructIPARecursive _ _ (Consonant  Voiceless LabialVelar? Affricate
  --     PulmonicEgressive Normal) = "k͡p"
  -- constructIPARecursive _ _ (Consonant  Voiceless Palatal (or AlveoloPalatal?)
  --     Affricate PulmonicEgressive Normal) = "c͡ɕ"

