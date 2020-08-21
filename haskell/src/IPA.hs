{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module IPA where

import Relude ((+), (.), (==), (/=), (&&), NonEmpty((:|)), Maybe(Just, Nothing), Text, (<), (<>),  Natural,
               fromMaybe, map, fmap, sconcat, maybe, not, zip)
import Relude.Container (HashMap, fromList)
import  Relude.Extra.Map (lookup)
import qualified Data.Text     as T

import EnglishUSText
import Lib_Types (Phonet(Consonant, Vowel), VocalFolds(..), Place(..), Manner(..), Airstream(..),
                      SecondaryArticulation(..),
                      Height(..), Backness(..), Rounding(..), PhonetInventory(..),
                      VowelLength(ExtraShort, NormalLength, HalfLong, Long))
import Lib_PseudoLens (toExtraShort, toHalfLong, toLabialized, toLong,
                       toPalatalized, toPharyngealized, toVelarized,
                       toVoiced, toVoiceless)
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

ipaPhonemeMapList :: [(Text, Phonet)]
ipaPhonemeMapList = 
  [ ("p", (Consonant Voiceless Bilabial Plosive PulmonicEgressive Normal))
  , ("b", (Consonant Voiced Bilabial Plosive PulmonicEgressive Normal))
  , ("t", (Consonant Voiceless Alveolar Plosive PulmonicEgressive Normal))
  , ("d", (Consonant Voiced Alveolar Plosive PulmonicEgressive Normal))
  , ("ʈ", (Consonant Voiceless Retroflex Plosive PulmonicEgressive Normal))
  , ("ɖ", (Consonant Voiced Retroflex Plosive PulmonicEgressive Normal))
  , ("c", (Consonant Voiceless Palatal Plosive PulmonicEgressive Normal))
  , ("ɟ", (Consonant Voiced Palatal Plosive PulmonicEgressive Normal))
  , ("k", (Consonant Voiceless Velar Plosive PulmonicEgressive Normal))
  , ("g", (Consonant Voiced Velar Plosive PulmonicEgressive Normal))
  , ("q", (Consonant Voiceless Uvular Plosive PulmonicEgressive Normal))
  , ("ɢ", (Consonant Voiced Uvular Plosive PulmonicEgressive Normal))
  , ("ʔ", (Consonant Voiceless Glottal Plosive PulmonicEgressive Normal))
    -- Nasals:
  , ("m", (Consonant Voiced Bilabial Nasal PulmonicEgressive Normal))
  , ("ɱ", (Consonant Voiced LabioDental Nasal PulmonicEgressive Normal))
  , ("n", (Consonant Voiced Alveolar Nasal PulmonicEgressive Normal))
  , ("ɳ", (Consonant Voiced Retroflex Nasal PulmonicEgressive Normal))
  , ("ɲ", (Consonant Voiced Palatal Nasal PulmonicEgressive Normal))
  , ("ŋ", (Consonant Voiced Velar Nasal PulmonicEgressive Normal))
  , ("ɴ", (Consonant Voiced Uvular Nasal PulmonicEgressive Normal))
    -- Trills:
  , ("ʙ", (Consonant Voiced Bilabial Trill PulmonicEgressive Normal))
  , ("r", (Consonant Voiced Alveolar Trill PulmonicEgressive Normal))
  , ("ʀ", (Consonant Voiced Uvular Trill PulmonicEgressive Normal))
    -- Taps or flaps:
  , ("ⱱ", (Consonant Voiced LabioDental TapOrFlap PulmonicEgressive Normal))
  , ("ɾ", (Consonant Voiced Alveolar TapOrFlap PulmonicEgressive Normal))
  , ("ɽ", (Consonant Voiced Retroflex TapOrFlap PulmonicEgressive Normal))
    -- Fricatives:
  , ("ɸ", (Consonant Voiceless Bilabial Fricative PulmonicEgressive Normal))
  , ("β", (Consonant Voiced Bilabial Fricative PulmonicEgressive Normal))
  , ("f", (Consonant Voiceless LabioDental Fricative PulmonicEgressive Normal))
  , ("v", (Consonant Voiced LabioDental Fricative PulmonicEgressive Normal))
  , ("θ", (Consonant Voiceless Dental Fricative PulmonicEgressive Normal))
  , ("ð", (Consonant Voiced Dental Fricative PulmonicEgressive Normal))
  , ("s", (Consonant Voiceless Alveolar Fricative PulmonicEgressive Normal))
  , ("z", (Consonant Voiced Alveolar Fricative PulmonicEgressive Normal))
  , ("ʃ", (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive Normal))
  , ("ʒ", (Consonant Voiced PostAlveolar Fricative PulmonicEgressive Normal))
  , ("ʂ", (Consonant Voiceless Retroflex Fricative PulmonicEgressive Normal))
  , ("ʐ", (Consonant Voiced Retroflex Fricative PulmonicEgressive Normal))
  , ("ç", (Consonant Voiceless Palatal Fricative PulmonicEgressive Normal))
  , ("ʝ", (Consonant Voiced Palatal Fricative PulmonicEgressive Normal))
  , ("x", (Consonant Voiceless Velar Fricative PulmonicEgressive Normal))
  , ("ɣ", (Consonant Voiced Velar Fricative PulmonicEgressive Normal))
  , ("χ", (Consonant Voiceless Uvular Fricative PulmonicEgressive Normal))
  , ("ʁ", (Consonant Voiced Uvular Fricative PulmonicEgressive Normal))
  , ("ħ", (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive Normal))
  , ("ʕ", (Consonant Voiced Pharyngeal Fricative PulmonicEgressive Normal))
  , ("h", (Consonant Voiceless Glottal Fricative PulmonicEgressive Normal))
  , ("ɦ", (Consonant Voiced Glottal Fricative PulmonicEgressive Normal))
    -- Lateral Fricatives:
  , ("ɬ", (Consonant Voiceless Alveolar LateralFricative PulmonicEgressive Normal))
  , ("ɮ", (Consonant Voiced Alveolar LateralFricative PulmonicEgressive Normal))
    -- Approximants:
  , ("ʋ", (Consonant Voiced LabioDental Approximant PulmonicEgressive Normal))
  , ("ɹ", (Consonant Voiced Alveolar Approximant PulmonicEgressive Normal))
  , ("ɻ", (Consonant Voiced Retroflex Approximant PulmonicEgressive Normal))
  , ("j", (Consonant Voiced Palatal Approximant PulmonicEgressive Normal))
  , ("ɰ", (Consonant Voiced Velar Approximant PulmonicEgressive Normal))
    -- Lateral Approximants:
  , ("l", (Consonant Voiced Alveolar LateralApproximant PulmonicEgressive Normal))
  , ("ɭ", (Consonant Voiced Retroflex LateralApproximant PulmonicEgressive Normal))
  , ("ʎ", (Consonant Voiced Palatal LateralApproximant PulmonicEgressive Normal))
  , ("ʟ", (Consonant Voiced Velar LateralApproximant PulmonicEgressive Normal))
    -- Affricates
  , ("t͡ʃ", (Consonant Voiceless PostAlveolar Affricate PulmonicEgressive Normal))
  , ("t͜ʃ", (Consonant Voiceless PostAlveolar Affricate PulmonicEgressive Normal))
  , ("d͡ʒ", (Consonant Voiced PostAlveolar Affricate PulmonicEgressive Normal))
  , ("d͜ʒ", (Consonant Voiced PostAlveolar Affricate PulmonicEgressive Normal))
    -- We should probably enforce use of the tie-bar underneath, otherwise
    -- it would not be deterministic to determine whether two graphemes here
    -- represent affricates or a plosive followed by a fricative.

    -- Under the Other Symbols part of the IPA chart:
  , ("w", (Consonant Voiced LabialVelar Approximant PulmonicEgressive Normal))
  , ("ʍ", (Consonant Voiceless LabialVelar Fricative PulmonicEgressive Normal))
  , ("ɥ", (Consonant Voiced LabialPalatal Approximant PulmonicEgressive Normal))
  , ("ʜ", (Consonant Voiceless Epiglottal Fricative PulmonicEgressive Normal))
  , ("ʢ", (Consonant Voiced Epiglottal Fricative PulmonicEgressive Normal))
  , ("ʡ", (Consonant Voiceless Epiglottal Plosive PulmonicEgressive Normal))
    -- Is the epiglottal plosive voiceless? The IPA chart does not specify.
  , ("ɕ", (Consonant Voiceless AlveoloPalatal Fricative PulmonicEgressive Normal))
  , ("ʑ", (Consonant Voiced AlveoloPalatal Fricative PulmonicEgressive Normal))
  , ("ɺ", (Consonant Voiced Alveolar LateralFlap PulmonicEgressive Normal))
  , ("ɧ",

        ( Consonant
            Voiceless
            (Places (PostAlveolar :| [Velar]))
            Fricative
            PulmonicEgressive
            Normal
        ))
    -- Other Consonants:
  , ("ʘ", (Consonant Voiceless Bilabial Plosive Click Normal))
  , ("ǀ", (Consonant Voiceless Dental Plosive Click Normal))
  , ("ǃ", (Consonant Voiceless Alveolar Plosive Click Normal))
    --)("ǃ" could also be PostAlveolar.
  , ("ǂ", (Consonant Voiceless PalatoAlveolar Plosive Click Normal))
  , ("ǁ", (Consonant Voiceless Alveolar Lateral Click Normal))
  , ("ɓ", (Consonant Voiced Bilabial Plosive Implosive Normal))
  , ("ɗ", (Consonant Voiced Dental Plosive Implosive Normal))
    -- "ɗ" could also be Alveolar
  , ("ʄ", (Consonant Voiced Palatal Plosive Implosive Normal))
  , ("ɠ", (Consonant Voiced Velar Plosive Implosive Normal))
  , ("ʛ", (Consonant Voiced Uvular Plosive Implosive Normal))
    -- Close Vowels:
  , ("i", (Vowel Close Front Unrounded Voiced NormalLength))
  , ("y", (Vowel Close Front Rounded Voiced NormalLength))
  , ("ɨ", (Vowel Close Central Unrounded Voiced NormalLength))
  , ("ʉ", (Vowel Close Central Rounded Voiced NormalLength))
  , ("ɯ", (Vowel Close Back Unrounded Voiced NormalLength))
  , ("u", (Vowel Close Back Rounded Voiced NormalLength))
    -- Near-close Vowels:
  , ("ɪ", (Vowel NearClose Front Unrounded Voiced NormalLength))
  , ("ʏ", (Vowel NearClose Front Rounded Voiced NormalLength))
  , ("ʊ", (Vowel NearClose Back Rounded Voiced NormalLength))
    -- Close-mid Vowels:
  , ("e", (Vowel CloseMid Front Unrounded Voiced NormalLength))
  , ("ø", (Vowel CloseMid Front Rounded Voiced NormalLength))
  , ("ɘ", (Vowel CloseMid Central Unrounded Voiced NormalLength))
  , ("ɵ", (Vowel CloseMid Central Rounded Voiced NormalLength))
  , ("ɤ", (Vowel CloseMid Back Unrounded Voiced NormalLength))
  , ("o", (Vowel CloseMid Back Rounded Voiced NormalLength))
    -- Mid Vowels:
  , ("ə", (Vowel Mid Central Unrounded Voiced NormalLength))
    -- Open-mid Vowels:
  , ("ɛ", (Vowel OpenMid Front Unrounded Voiced NormalLength))
  , ("œ", (Vowel OpenMid Front Rounded Voiced NormalLength))
  , ("ɜ", (Vowel OpenMid Central Unrounded Voiced NormalLength))
  , ("ɞ", (Vowel OpenMid Central Rounded Voiced NormalLength))
  , ("ʌ", (Vowel OpenMid Back Unrounded Voiced NormalLength))
  , ("ɔ", (Vowel OpenMid Back Rounded Voiced NormalLength))
    -- Near-open
  , ("æ", (Vowel NearOpen Front Unrounded Voiced NormalLength))
  , ("ɐ", (Vowel NearOpen Central Unrounded Voiced NormalLength))
    -- Open Vowels:
  , ("a", (Vowel Open Front Unrounded Voiced NormalLength))
  , ("ɶ", (Vowel Open Front Rounded Voiced NormalLength))
  , ("ɑ", (Vowel Open Back Unrounded Voiced NormalLength))
  , ("ɒ", (Vowel Open Back Rounded Voiced NormalLength))
  ]

ipaTextToPhonetHashMap :: HashMap Text Phonet
ipaTextToPhonetHashMap = fromList ipaPhonemeMapList

reverseTuple :: (a, b) -> (b, a)
reverseTuple (x, y) = (y, x)

phonetToIpaTextHashMap :: HashMap Phonet Text
phonetToIpaTextHashMap = fromList (map reverseTuple ipaPhonemeMapList)

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
analyzeIPA p =
 let p' = lookup p ipaTextToPhonetHashMap
 in case p' of
   Just x  -> Just x
   Nothing ->
     case p of
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

addRetractedDiacritic :: Text -> Text
addRetractedDiacritic = (<> "̠")

addVoicedDiacritic :: Text -> Text
addVoicedDiacritic = (<> "̬")

addCreakyVoicedDiacritic :: Text -> Text
addCreakyVoicedDiacritic = (<> "̰")

addAspirationDiacritic :: Text -> Text
addAspirationDiacritic = (<> "ʰ")

addVoicelessDiacritic :: Text -> Text
addVoicelessDiacritic x =
  if isDescenderText x
    then x <> "̊"
    else x <> "̥"


constructIPARecursive :: Natural -> Natural -> Phonet -> Maybe Text
constructIPARecursive recursionLimit recursionLevel p =
  if recursionLevel == recursionLimit
  then Nothing
  else
    let p' = lookup p phonetToIpaTextHashMap
    in case p' of
      Just x -> Just x
      Nothing -> constructIPAMultichar recursionLimit recursionLevel p


-- When you know it is a consonant
constructIPAMultichar :: Natural -> Natural -> Phonet -> Maybe Text
constructIPAMultichar recursionLimit recursionLevel p = case p of
  (Consonant x PostAlveolar y z sa)
    | recursionLevel < recursionLimit ->
      case constructIPARecursive
        recursionLimit
        (1 + recursionLevel)
        (Consonant x Alveolar y z sa) of
        Nothing         -> Nothing
        Just regularIPA -> Just (addRetractedDiacritic regularIPA)
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
                            Just (addVoicelessDiacritic regularIPA)

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
        Just regularIPA -> Just (addVoicedDiacritic regularIPA)
  (Consonant VoicedAspirated _ _ PulmonicEgressive Normal)
    | recursionLevel < recursionLimit ->
      let result =
            constructIPARecursive
              recursionLimit
              (1 + recursionLevel)
              (deaspirate p)
       in case result of
            Nothing         -> Nothing
            Just regularIPA -> Just (addAspirationDiacritic regularIPA)
  (Consonant VoicelessAspirated _ _ PulmonicEgressive Normal)
    | recursionLevel < recursionLimit ->
      let result =
            constructIPARecursive
              recursionLimit
              (1 + recursionLevel)
              (deaspirate p)
       in case result of
            Nothing         -> Nothing
            Just regularIPA -> Just (addAspirationDiacritic regularIPA)
  (Consonant CreakyVoiced _ _ PulmonicEgressive Normal)
    | recursionLevel < recursionLimit ->
      let result =
            constructIPARecursive
              recursionLimit
              (1 + recursionLevel)
              (decreak p)
       in case result of
            Just regularIPA -> Just (addCreakyVoicedDiacritic regularIPA)
            Nothing         -> Nothing
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
        Just regularIPA -> Just (addVoicelessDiacritic regularIPA)
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
        Just regularIPA -> Just (addVoicedDiacritic regularIPA)

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

