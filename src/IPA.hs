module IPA where

import Prelude(Eq, (+), (.), (==), (/=), (&&), Maybe(..), (<), (<>), otherwise,
  map, zip)
import Data.Maybe (fromMaybe, maybe)
import Numeric.Natural (Natural)
import Data.Text (Text, concat, init, last, null, pack, unlines)

import DefaultLanguageText
    ( sorryUnableToCalculate, noEnglishDescriptionFoundMessage )

import Lib_Types
  ( Airstream(..)
  , Backness(..)
  , Height(..)
  , Manner(..)
  , Phonet(..)
  , PhonetInventory(..)
  , Place(..)
  , Rounding(..)
  , SecondaryArticulation(..)
  , VocalFolds(..)
  , VowelLength(..)
  )

import Lib_PseudoLens (toExtraShort, toHalfLong, toLabialized, toLong,
                       toPalatalized, toPharyngealized, toVelarized,
                       toVoiced, toVoiceless)
import Lib_Functions (aspirate,
  spirantizedPhonet, devoicedPhonet,
  voicedPhonet, decreak, deaspirate,
  retractPhonet)
import ShowFunctions (showPhonet)

import PhoneticFeatures(showFeatures, analyzeFeatures)
import           LanguageSpecific.EnglishSpecific (englishPhonetInventory)

import GraphemeGrammar(splitIntoPhonemes, isDescender)


englishPhonetInventoryReport :: Text
englishPhonetInventoryReport = ipaTextToPhonetListReport (showIPA englishPhonetInventory)

analyzeIPAToSPE :: Text -> Text
analyzeIPAToSPE ipaText =
  maybe sorryUnableToCalculate (showFeatures . analyzeFeatures) (analyzeIPA ipaText)

ipaPhonemeMapList :: [(Text, Phonet)]
ipaPhonemeMapList = 
  [ (pack "p", (Consonant Voiceless Bilabial Plosive PulmonicEgressive Normal))
  , (pack "b", (Consonant Voiced Bilabial Plosive PulmonicEgressive Normal))
  , (pack "t", (Consonant Voiceless Alveolar Plosive PulmonicEgressive Normal))
  , (pack "d", (Consonant Voiced Alveolar Plosive PulmonicEgressive Normal))
  , (pack "ʈ", (Consonant Voiceless Retroflex Plosive PulmonicEgressive Normal))
  , (pack "ɖ", (Consonant Voiced Retroflex Plosive PulmonicEgressive Normal))
  , (pack "c", (Consonant Voiceless Palatal Plosive PulmonicEgressive Normal))
  , (pack "ɟ", (Consonant Voiced Palatal Plosive PulmonicEgressive Normal))
  , (pack "k", (Consonant Voiceless Velar Plosive PulmonicEgressive Normal))
  , (pack "g", (Consonant Voiced Velar Plosive PulmonicEgressive Normal))
  , (pack "q", (Consonant Voiceless Uvular Plosive PulmonicEgressive Normal))
  , (pack "ɢ", (Consonant Voiced Uvular Plosive PulmonicEgressive Normal))
  , (pack "ʔ", (Consonant Voiceless Glottal Plosive PulmonicEgressive Normal))
    -- Nasals:
  , (pack "m", (Consonant Voiced Bilabial Nasal PulmonicEgressive Normal))
  , (pack "ɱ", (Consonant Voiced LabioDental Nasal PulmonicEgressive Normal))
  , (pack "n", (Consonant Voiced Alveolar Nasal PulmonicEgressive Normal))
  , (pack "ɳ", (Consonant Voiced Retroflex Nasal PulmonicEgressive Normal))
  , (pack "ɲ", (Consonant Voiced Palatal Nasal PulmonicEgressive Normal))
  , (pack "ŋ", (Consonant Voiced Velar Nasal PulmonicEgressive Normal))
  , (pack "ɴ", (Consonant Voiced Uvular Nasal PulmonicEgressive Normal))
    -- Trills:
  , (pack "ʙ", (Consonant Voiced Bilabial Trill PulmonicEgressive Normal))
  , (pack "r", (Consonant Voiced Alveolar Trill PulmonicEgressive Normal))
  , (pack "ʀ", (Consonant Voiced Uvular Trill PulmonicEgressive Normal))
    -- Taps or flaps:
  , (pack "ⱱ", (Consonant Voiced LabioDental TapOrFlap PulmonicEgressive Normal))
  , (pack "ɾ", (Consonant Voiced Alveolar TapOrFlap PulmonicEgressive Normal))
  , (pack "ɽ", (Consonant Voiced Retroflex TapOrFlap PulmonicEgressive Normal))
    -- Fricatives:
  , (pack "ɸ", (Consonant Voiceless Bilabial Fricative PulmonicEgressive Normal))
  , (pack "β", (Consonant Voiced Bilabial Fricative PulmonicEgressive Normal))
  , (pack "f", (Consonant Voiceless LabioDental Fricative PulmonicEgressive Normal))
  , (pack "v", (Consonant Voiced LabioDental Fricative PulmonicEgressive Normal))
  , (pack "θ", (Consonant Voiceless Dental Fricative PulmonicEgressive Normal))
  , (pack "ð", (Consonant Voiced Dental Fricative PulmonicEgressive Normal))
  , (pack "s", (Consonant Voiceless Alveolar Fricative PulmonicEgressive Normal))
  , (pack "z", (Consonant Voiced Alveolar Fricative PulmonicEgressive Normal))
  , (pack "ʃ", (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive Normal))
  , (pack "ʒ", (Consonant Voiced PostAlveolar Fricative PulmonicEgressive Normal))
  , (pack "ʂ", (Consonant Voiceless Retroflex Fricative PulmonicEgressive Normal))
  , (pack "ʐ", (Consonant Voiced Retroflex Fricative PulmonicEgressive Normal))
  , (pack "ç", (Consonant Voiceless Palatal Fricative PulmonicEgressive Normal))
  , (pack "ʝ", (Consonant Voiced Palatal Fricative PulmonicEgressive Normal))
  , (pack "x", (Consonant Voiceless Velar Fricative PulmonicEgressive Normal))
  , (pack "ɣ", (Consonant Voiced Velar Fricative PulmonicEgressive Normal))
  , (pack "χ", (Consonant Voiceless Uvular Fricative PulmonicEgressive Normal))
  , (pack "ʁ", (Consonant Voiced Uvular Fricative PulmonicEgressive Normal))
  , (pack "ħ", (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive Normal))
  , (pack "ʕ", (Consonant Voiced Pharyngeal Fricative PulmonicEgressive Normal))
  , (pack "h", (Consonant Voiceless Glottal Fricative PulmonicEgressive Normal))
  , (pack "ɦ", (Consonant Voiced Glottal Fricative PulmonicEgressive Normal))
    -- Lateral Fricatives:
  , (pack "ɬ", (Consonant Voiceless Alveolar LateralFricative PulmonicEgressive Normal))
  , (pack "ɮ", (Consonant Voiced Alveolar LateralFricative PulmonicEgressive Normal))
    -- Approximants:
  , (pack "ʋ", (Consonant Voiced LabioDental Approximant PulmonicEgressive Normal))
  , (pack "ɹ", (Consonant Voiced Alveolar Approximant PulmonicEgressive Normal))
  , (pack "ɻ", (Consonant Voiced Retroflex Approximant PulmonicEgressive Normal))
  , (pack "j", (Consonant Voiced Palatal Approximant PulmonicEgressive Normal))
  , (pack "ɰ", (Consonant Voiced Velar Approximant PulmonicEgressive Normal))
    -- Lateral Approximants:
  , (pack "l", (Consonant Voiced Alveolar LateralApproximant PulmonicEgressive Normal))
  , (pack "ɭ", (Consonant Voiced Retroflex LateralApproximant PulmonicEgressive Normal))
  , (pack "ʎ", (Consonant Voiced Palatal LateralApproximant PulmonicEgressive Normal))
  , (pack "ʟ", (Consonant Voiced Velar LateralApproximant PulmonicEgressive Normal))
    -- Affricates
  , (pack "t͡ʃ", (Consonant Voiceless PostAlveolar Affricate PulmonicEgressive Normal))
  , (pack "t͜ʃ", (Consonant Voiceless PostAlveolar Affricate PulmonicEgressive Normal))
  , (pack "d͡ʒ", (Consonant Voiced PostAlveolar Affricate PulmonicEgressive Normal))
  , (pack "d͜ʒ", (Consonant Voiced PostAlveolar Affricate PulmonicEgressive Normal))
    -- We should probably enforce use of the tie-bar underneath, otherwise
    -- it would not be deterministic to determine whether two graphemes here
    -- represent affricates or a plosive followed by a fricative.

    -- Under the Other Symbols part of the IPA chart:
  , (pack "w", (Consonant Voiced LabialVelar Approximant PulmonicEgressive Normal))
  , (pack "ʍ", (Consonant Voiceless LabialVelar Fricative PulmonicEgressive Normal))
  , (pack "ɥ", (Consonant Voiced LabialPalatal Approximant PulmonicEgressive Normal))
  , (pack "ʜ", (Consonant Voiceless Epiglottal Fricative PulmonicEgressive Normal))
  , (pack "ʢ", (Consonant Voiced Epiglottal Fricative PulmonicEgressive Normal))
  , (pack "ʡ", (Consonant Voiceless Epiglottal Plosive PulmonicEgressive Normal))
    -- Is the epiglottal plosive voiceless? The IPA chart does not specify.
  , (pack "ɕ", (Consonant Voiceless AlveoloPalatal Fricative PulmonicEgressive Normal))
  , (pack "ʑ", (Consonant Voiced AlveoloPalatal Fricative PulmonicEgressive Normal))
  , (pack "ɺ", (Consonant Voiced Alveolar LateralFlap PulmonicEgressive Normal))
  , (pack "ɧ",

        ( Consonant
            Voiceless
            (Places [PostAlveolar, Velar])
            Fricative
            PulmonicEgressive
            Normal
        ))
    -- Other Consonants:
  , (pack "ʘ", (Consonant Voiceless Bilabial Plosive Click Normal))
  , (pack "ǀ", (Consonant Voiceless Dental Plosive Click Normal))
  , (pack "ǃ", (Consonant Voiceless Alveolar Plosive Click Normal))
    -- "ǃ" could also be PostAlveolar.
  , (pack "ǂ", (Consonant Voiceless PalatoAlveolar Plosive Click Normal))
  , (pack "ǁ", (Consonant Voiceless Alveolar Lateral Click Normal))
  , (pack "ɓ", (Consonant Voiced Bilabial Plosive Implosive Normal))
  , (pack "ɗ", (Consonant Voiced Dental Plosive Implosive Normal))
    -- "ɗ" could also be Alveolar
  , (pack "ʄ", (Consonant Voiced Palatal Plosive Implosive Normal))
  , (pack "ɠ", (Consonant Voiced Velar Plosive Implosive Normal))
  , (pack "ʛ", (Consonant Voiced Uvular Plosive Implosive Normal))
    -- Close Vowels:
  , (pack "i", (Vowel Close Front Unrounded Voiced NormalLength))
  , (pack "y", (Vowel Close Front Rounded Voiced NormalLength))
  , (pack "ɨ", (Vowel Close Central Unrounded Voiced NormalLength))
  , (pack "ʉ", (Vowel Close Central Rounded Voiced NormalLength))
  , (pack "ɯ", (Vowel Close Back Unrounded Voiced NormalLength))
  , (pack "u", (Vowel Close Back Rounded Voiced NormalLength))
    -- Near-close Vowels:
  , (pack "ɪ", (Vowel NearClose Front Unrounded Voiced NormalLength))
  , (pack "ʏ", (Vowel NearClose Front Rounded Voiced NormalLength))
  , (pack "ʊ", (Vowel NearClose Back Rounded Voiced NormalLength))
    -- Close-mid Vowels:
  , (pack "e", (Vowel CloseMid Front Unrounded Voiced NormalLength))
  , (pack "ø", (Vowel CloseMid Front Rounded Voiced NormalLength))
  , (pack "ɘ", (Vowel CloseMid Central Unrounded Voiced NormalLength))
  , (pack "ɵ", (Vowel CloseMid Central Rounded Voiced NormalLength))
  , (pack "ɤ", (Vowel CloseMid Back Unrounded Voiced NormalLength))
  , (pack "o", (Vowel CloseMid Back Rounded Voiced NormalLength))
    -- Mid Vowels:
  , (pack "ə", (Vowel Mid Central Unrounded Voiced NormalLength))
    -- Open-mid Vowels:
  , (pack "ɛ", (Vowel OpenMid Front Unrounded Voiced NormalLength))
  , (pack "œ", (Vowel OpenMid Front Rounded Voiced NormalLength))
  , (pack "ɜ", (Vowel OpenMid Central Unrounded Voiced NormalLength))
  , (pack "ɞ", (Vowel OpenMid Central Rounded Voiced NormalLength))
  , (pack "ʌ", (Vowel OpenMid Back Unrounded Voiced NormalLength))
  , (pack "ɔ", (Vowel OpenMid Back Rounded Voiced NormalLength))
    -- Near-open
  , (pack "æ", (Vowel NearOpen Front Unrounded Voiced NormalLength))
  , (pack "ɐ", (Vowel NearOpen Central Unrounded Voiced NormalLength))
    -- Open Vowels:
  , (pack "a", (Vowel Open Front Unrounded Voiced NormalLength))
  , (pack "ɶ", (Vowel Open Front Rounded Voiced NormalLength))
  , (pack "ɑ", (Vowel Open Back Unrounded Voiced NormalLength))
  , (pack "ɒ", (Vowel Open Back Rounded Voiced NormalLength))
  ]

lookupInList :: Eq a => a -> [(a, b)] -> Maybe b
lookupInList givenKey aList =
  case aList of
    [] -> Nothing
    ((key, value) : tailOfList) | key == givenKey -> Just value
                                | otherwise       -> lookupInList givenKey tailOfList

lookupInListFromValue :: Eq b => b -> [(a, b)] -> Maybe a
lookupInListFromValue givenKey aList =
  case aList of
    [] -> Nothing
    ((value, key) : tailOfList) | key == givenKey -> Just value
                                | otherwise       -> lookupInListFromValue givenKey tailOfList



-- | Given text containing international phonetic alphabet symbols
--   returns text with every phonetic alphabet symbol or sequence
--   of symbols for a sound
--   followed by the description of the sound it represents.
ipaTextToPhonetListReport :: Text -> Text
ipaTextToPhonetListReport text =
  let listA = ipaTextToPhonetList text
   in unlines (map ipaAndPhonetFormat listA)

ipaAndPhonetFormat :: (Text, Maybe Phonet) -> Text
ipaAndPhonetFormat (ipaText, phonet) =
  pack "/" <> ipaText <> pack "/" <> pack " " <> phonetSummary
  where
    phonetSummary =
      maybe (pack "(n/a)") showPhonet phonet

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
 let p' = lookupInList p ipaPhonemeMapList
 in case p' of
   Just x  -> Just x
   Nothing ->
     case p of
       ipaText | null ipaText -> Nothing
       -- Handle Diacritics:
       ipaText ->
         case [last ipaText] of
           "̥" ->
             let fullGrapheme = analyzeIPA (init ipaText)
              in case fullGrapheme of
                   Just x -> Just (toVoiceless x)
                   Nothing -> Nothing
           "̊" ->
             let fullGrapheme = analyzeIPA (init ipaText)
              in case fullGrapheme of
                   Just x -> Just (toVoiceless x)
                   Nothing -> Nothing
      
           "̬" ->
             let fullGrapheme = analyzeIPA (init ipaText)
              in case fullGrapheme of
                   Just x -> Just (toVoiced x)
                   Nothing -> Nothing
           "ʷ" ->
             let fullGrapheme = analyzeIPA (init ipaText)
              in case fullGrapheme of
                   Just x -> Just (toLabialized x)
                   Nothing -> Nothing
      
           "ʲ" ->
             let fullGrapheme = analyzeIPA (init ipaText)
              in case fullGrapheme of
                   Just x -> Just (toPalatalized x)
                   Nothing -> Nothing
      
           "ˠ" ->
             let fullGrapheme = analyzeIPA (init ipaText)
              in case fullGrapheme of
                   Just x -> Just (toVelarized x)
                   Nothing -> Nothing
      
           "ˤ" ->
             let fullGrapheme = analyzeIPA (init ipaText)
              in case fullGrapheme of
                   Just x -> Just (toPharyngealized x)
                   Nothing -> Nothing
           "ː" ->
             let fullGrapheme = analyzeIPA (init ipaText)
              in case fullGrapheme of
                   Just x -> Just (toLong x)
                   Nothing -> Nothing
           "ˑ" ->
             let fullGrapheme = analyzeIPA (init ipaText)
              in case fullGrapheme of
                   Just x -> Just (toHalfLong x)
                   Nothing -> Nothing
           "̆" ->
             let fullGrapheme = analyzeIPA (init ipaText)
              in case fullGrapheme of
                   Just x -> Just (toExtraShort x)
                   Nothing -> Nothing
      
           "ʰ" ->
             let fullGrapheme = analyzeIPA (init ipaText)
              in case fullGrapheme of
                   Just x -> Just (aspirate x)
                   Nothing -> Nothing
           -- (About the preceding line:) It is strange but we will just
           -- do nothing if they give us an aspirated vowel.
           -- since we have no way to represent it in the type system.
           -- to do: determine
           -- if the idea of an aspirated vowel makes sense
           "̠" ->
             let fullGrapheme = analyzeIPA (init ipaText)
              in retractPhonet fullGrapheme
           _ -> Nothing -- not recognized.

constructIPA :: Phonet -> Text
constructIPA phoneme =
  fromMaybe (pack "∅") (constructIPARecursive 3 0 phoneme)

-- | convert a secondary articulation to its IPA text representation
secondaryArticulationIPA :: SecondaryArticulation -> Text
secondaryArticulationIPA articulation =
  case articulation of
     Normal     -> pack ""
     Palatalized -> pack "ʲ"
     Labialized -> pack "ʷ"
     Velarized  -> pack "ˠ"
     Pharyngealized -> pack "ˤ"

vowelLengthIPA :: VowelLength -> Text
vowelLengthIPA vowelLength =
  case vowelLength of
    NormalLength -> pack ""
    ExtraShort -> pack "̆"
    HalfLong -> pack "ˑ"
    Long -> pack "ː"

addRetractedDiacritic :: Text -> Text
addRetractedDiacritic = (<> pack "̠")

addVoicedDiacritic :: Text -> Text
addVoicedDiacritic = (<> pack "̬")

addCreakyVoicedDiacritic :: Text -> Text
addCreakyVoicedDiacritic = (<> pack "̰")

addAspirationDiacritic :: Text -> Text
addAspirationDiacritic = (<> pack "ʰ")

addVoicelessDiacritic :: Text -> Text
addVoicelessDiacritic x =
  if isDescender (last x)
    then x <> pack "̊"
    else x <> pack "̥"


constructIPARecursive :: Natural -> Natural -> Phonet -> Maybe Text
constructIPARecursive recursionLimit recursionLevel p =
  if recursionLevel == recursionLimit
  then Nothing
  else
    let p' = lookupInListFromValue p ipaPhonemeMapList
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
  Consonant {} -> Nothing
  Vowel     {} -> Nothing

constructDeconstruct :: (Phonet -> Phonet) -> Text -> Text
constructDeconstruct func x =
  let something = analyzeIPA x
   in case something of
        Nothing     -> pack "∅"
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
showIPA (PhonetInventory phonetes) = concat (map constructIPA phonetes)



  -- The following two lines are commented out, because I am unsure
  -- about their place of articulation:
  -- constructIPARecursive _ _ (Consonant  Voiceless LabialVelar? Affricate
  --     PulmonicEgressive Normal) = "k͡p"
  -- constructIPARecursive _ _ (Consonant  Voiceless Palatal (or AlveoloPalatal?)
  --     Affricate PulmonicEgressive Normal) = "c͡ɕ"

