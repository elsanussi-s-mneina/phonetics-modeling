{-# LANGUAGE OverloadedStrings #-}

module Lib_Functions where

import qualified Data.Text     as T
import           EnglishUSText
import           Lib_Types
import           Prelude       ()
import           Relude        (Bool (False, True), Char, Int,
                                Maybe (Just, Nothing), Natural, NonEmpty ((:|)),
                                Text, catMaybes, elem, filter, fmap, fromList,
                                fromMaybe, map, maybe, not, notElem, one,
                                otherwise, sconcat, toList, unwords, zip, (!!?),
                                (&&), (+), (-), (<), (<>), (==), (||), (/=), (.))

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

equivalentInPlace :: Place -> Place -> Bool
Bilabial `equivalentInPlace` Bilabial = True
LabioDental `equivalentInPlace` LabioDental = True
Dental `equivalentInPlace` Dental = True
Alveolar `equivalentInPlace` Alveolar = True
PostAlveolar `equivalentInPlace` PostAlveolar = True
Retroflex `equivalentInPlace` Retroflex = True
Palatal `equivalentInPlace` Palatal = True
Velar `equivalentInPlace` Velar = True
Uvular `equivalentInPlace` Uvular = True
Pharyngeal `equivalentInPlace` Pharyngeal = True
Glottal `equivalentInPlace` Glottal = True
Epiglottal `equivalentInPlace` Epiglottal = True
x `equivalentInPlace` Places pList = x `elem` pList
Places x `equivalentInPlace` y = y `equivalentInPlace` Places x
_ `equivalentInPlace` _ = False


-- | Given a place of articulation,
--   returns the place of articulation that is
--   the next more retracted.
retractedPlace :: Place -> Place
retractedPlace place =
  case place of
    Bilabial     -> LabioDental
    LabioDental  -> Dental
    Dental       -> Alveolar
    Alveolar     -> PostAlveolar
    PostAlveolar -> Retroflex
    Retroflex    -> Palatal
    Palatal      -> Velar
    Velar        -> Uvular
    Uvular       -> Pharyngeal
    Pharyngeal   -> Glottal
    Glottal      -> Epiglottal
    same         -> same

englishPhonetInventoryReport :: Text
englishPhonetInventoryReport = ipaTextToPhonetListReport (showIPA englishPhonetInventory)

-- | Gives the English description of a phone.
englishDescription :: Phonet -> Text
englishDescription = showPhonet

parseStart, splitByPhonetes :: Text -> [Text]


-- | Splits text in the International Phonetic Alphabet by
--   phones. This is also called tokenization.
--
--   Note: it does not recognize affricates, unless a tie-bar
--   is provided.
splitByPhonetes = parseStart
parseStart x = filter (/= "") (splitByPhonetesPrePostDiacrtic x)

splitByPhonetesPreDiacritic :: Text -> [Text]
splitByPhonetesPreDiacritic text =
  let result = prediacriticParserFunction text
   in case result of
        Nothing     -> splitByPhonetesPostDiacrtic text
        Just (a, b) -> [a] <> parseStart b

-- | Handle "ⁿdʰ", "ⁿdʷʰ" and other text strings
--   where a phoneme is represented in IPA by
--   a segmental preceded and followed by at least
--   one diacritic
splitByPhonetesPrePostDiacrtic :: Text -> [Text]
splitByPhonetesPrePostDiacrtic text =
  let result = prepostdiacriticParserFunction text
   in case result of
        Nothing            -> splitByPhonetesPreDiacritic text
        Just (chunk, rest) -> [chunk] <> parseStart rest

splitByPhonetesPostDiacrtic :: Text -> [Text]
splitByPhonetesPostDiacrtic text =
  let result = postdiacriticParserFunction text
   in case result of
        Nothing            -> splitByPhonetesNonDiacrtic text
        Just (chunk, rest) -> [chunk] <> parseStart rest

splitByPhonetesNonDiacrtic :: Text -> [Text]
splitByPhonetesNonDiacrtic text =
  let result = nondiacriticParserFunction text
   in case result of
        Nothing            -> [text] -- stop parsing!
        Just (chunk, rest) -> [chunk] <> parseStart rest

nondiacriticParserFunction :: Text -> Maybe (Text, Text)
nondiacriticParserFunction text =
  if not (T.null text) && isSegmental (T.head text)
    then
      if isTieBarAt 1 text
        then Just (T.take 3 text, T.drop 3 text)
        else Just (T.take 1 text, T.drop 1 text)
    else Nothing

isConsonantAt :: Int -> Text -> Bool
isConsonantAt = isSuchAt isConsonant


-- | Whether a character is one that is used in the
--   International Phonetic Alphabet to represent a
--   consonant.
isConsonant :: Char -> Bool
isConsonant = elemW consonants

-- | Whether a character in some text, at a specific place
--   within the text is a "segmental" (i.e. not a diacritic or modifier).
isSegmentalAt :: Int -> Text -> Bool
isSegmentalAt = isSuchAt isSegmental

-- | Whether a character is one that is used in the
--   International Phonetic Alphabet to represent something
--   that is not a diacritic, and can stand on its own.
--   This means characters that can represent a
--   consonant or vowel.
isSegmental :: Char -> Bool
isSegmental = elemW strictSegmentals

isExponentialAfterAt :: Int -> Text -> Bool
isExponentialAfterAt = isSuchAt isExponentialAfter

isTieBarAt :: Int -> Text -> Bool
isTieBarAt = isSuchAt isTieBar

isSuchAt :: (Char -> Bool) -> Int -> Text -> Bool
isSuchAt function index text = index < T.length text && function (T.index text index)


-- | Whether a character is a superscript character, that
--   often goes after a full character to modify the full
--   character's meaning.
--   For example in the International Phonetic Alphabet,
--   a superscript `h` causes the phoneme represented by the
--   previous character to
--   be aspirated.
isExponentialAfter :: Char -> Bool
isExponentialAfter = elemW exponentialsAfter

-- | Whether a character is a superscript character, that
--   often goes before a full character to modify the
--   full character's meaning.
--   For example in the International Phonetic Alphabet,
--   a superscript `n`.
isExponentialBefore :: Char -> Bool
isExponentialBefore = elemW exponentialsBefore


-- | Whether a character is used to tie two characters in the
--   international phonetic alphabet together. The tie bar is
--   usually used to indicate an affricate, or double-articulation.
isTieBar :: Char -> Bool
isTieBar x = x `elem` ['͜', '͡']

-- | Create a function that sees whether
-- a character is equal to (the first character in) an element
-- in a list of text
elemW :: NonEmpty Text -> (Char -> Bool)
elemW stringList = (`elem` fmap T.head stringList)

-- | Gets a pre-diacritic exponential with a segmental,
--   the segmental may have a tie bar.
--   If it has a tie-bar the character after the tie-bar
--   is also included. These
--   are returned in the first part of the tuple.
--   the text not yet parsed is in the second part
--   of the tuple.
prediacriticParserFunction :: Text -> Maybe (Text, Text)
prediacriticParserFunction text =
  if not (T.null text) && isExponentialBefore (T.head text)
    && isSegmentalAt 1 text
    then
      if isTieBarAt 2 text
        then Just (T.take 4 text, T.drop 4 text) -- include tie bar and character after it.
        else Just (T.take 2 text, T.drop 2 text)
    else Nothing

prepostdiacriticParserFunction :: Text -> Maybe (Text, Text)
prepostdiacriticParserFunction text =
  let preResult = prediacriticParserFunction text
   in case preResult of
        Nothing -> Nothing
        Just (prePart, middle) ->
          if isExponentialAfterAt 0 middle
            then
              let lengthOfFirst = T.length prePart
                  segmental = T.drop (lengthOfFirst - 1) prePart
                  postResult = postdiacriticParserFunction (segmental <> middle)
               in case postResult of
                    Nothing -> Nothing
                    Just (postPart, rest) -> Just (prePart <> T.tail postPart, rest)
            else Nothing

postdiacriticParserFunction :: Text -> Maybe (Text, Text)
postdiacriticParserFunction text
  | isSegmentalAt 0 text && isExponentialAfterAt 1 text =
    let numberOfPostdiacritics = countPostDiacriticsInARow text 1
        chunkLength = numberOfPostdiacritics + 1
     in Just (T.take chunkLength text, T.drop chunkLength text)
  | isSegmentalAt 0 text
      && isTieBarAt 1 text
      && isExponentialAfterAt 2 text =
    let numberOfPostdiacritics = countPostDiacriticsInARow text 3
        chunkLength = numberOfPostdiacritics + 3
     in Just (T.take chunkLength text, T.drop chunkLength text)
  | otherwise = Nothing

countPostDiacriticsInARow :: Text -> Int -> Int
countPostDiacriticsInARow sText startIndex =
  if isExponentialAfterAt startIndex sText
    then 1 + countPostDiacriticsInARow sText (startIndex + 1)
    else 0

-- | A function that given an IPA symbol will convert it to the voiced
--   equivalent.
voicedPhonet :: Phonet -> Phonet
voicedPhonet p = case p of
  (Consonant VoicelessAspirated x y z) -> Consonant VoicedAspirated x y z
  (Consonant Voiceless x y z)          -> Consonant Voiced x y z
  (Consonant Voiced x y z)             -> Consonant Voiced x y z
  (Consonant VoicedAspirated x y z)    -> Consonant VoicedAspirated x y z
  (Consonant _ x y z)                  -> Consonant Voiced x y z
  (Vowel x y z _)                      -> Vowel x y z Voiced

-- | A function that given an IPA symbol will convert it to the voiceless
--   equivalent.
devoicedPhonet :: Phonet -> Phonet
devoicedPhonet p = case p of
  (Consonant Voiced x y z)             -> Consonant Voiceless x y z
  (Consonant CreakyVoiced x y z)       -> Consonant Voiceless x y z
  (Consonant Voiceless x y z)          -> Consonant Voiceless x y z
  (Consonant VoicedAspirated x y z)    -> Consonant VoicelessAspirated x y z
  (Consonant VoicelessAspirated x y z) -> Consonant VoicelessAspirated x y z
  (Vowel x y z _)                      -> Vowel x y z Voiceless

spirantizedPhonet :: Phonet -> Phonet
-- The following is inelegant, but there is no other way in the system,
-- right now. The part that is inelegant is that,
-- a [t] which is considered alveolar, when spirantized becomes [θ]
-- which is dental.
-- So the following line implements this
-- change in place of articulation.
spirantizedPhonet p = case p of
  (Consonant x Alveolar Plosive z) -> Consonant x Dental Fricative z
  (Consonant x place_1 Plosive z)  -> Consonant x place_1 Fricative z
  other                            -> other

unmarkDifferences :: Phonet -> Phonet -> UnmarkablePhonet
unmarkDifferences p_1 p_2 = case (p_1, p_2) of
  ( Consonant voice_1 place_1 manner_1 airstream_1,
    Consonant voice_2 place_2 manner_2 airstream_2
    ) ->
      let voice' = unmarkVoice voice_1 voice_2
          place' = unmarkPlace place_1 place_2
          manner' = unmarkManner manner_1 manner_2
          airstream' = unmarkAirstream airstream_1 airstream_2
       in UnmarkableConsonant voice' place' manner' airstream'
  ( Vowel height_1 backness_1 rounding_1 voice_1,
    Vowel height_2 backness_2 rounding_2 voice_2
    ) ->
      let voice' = unmarkVoice voice_1 voice_2
          height' = unmarkHeight height_1 height_2
          backness' = unmarkBackness backness_1 backness_2
          rounding' = unmarkRounding rounding_1 rounding_2
       in UnmarkableVowel height' backness' rounding' voice'
  ( Vowel _ _ _ voice_1,
    Consonant voice_2 _ _ _
    ) ->
      let voice' = unmarkVoice voice_1 voice_2
       in UnmarkableVowel UnmarkedHeight UnmarkedBackness UnmarkedRounding voice'
  ( Consonant {},
    Vowel {}
    ) ->
      unmarkDifferences p_2 p_1 -- Change the order of arguments
  where
    unmarkVoice voice_1 voice_2 =
      if voice_1 == voice_2
        then MarkedVocalFolds voice_1
        else UnmarkedVocalFolds

    unmarkPlace place_1 place_2 =
      if place_1 `equivalentInPlace` place_2
        then MarkedPlace place_1
        else UnmarkedPlace

    unmarkManner manner_1 manner_2 =
      if manner_1 == manner_2
        then MarkedManner manner_1
        else UnmarkedManner

    unmarkAirstream airstream_1 airstream_2 =
      if airstream_1 == airstream_2
        then MarkedAirstream airstream_1
        else UnmarkedAirstream

    unmarkHeight height_1 height_2 =
      if height_1 == height_2
        then MarkedHeight height_1
        else UnmarkedHeight

    unmarkBackness backness_1 backness_2 =
      if backness_1 == backness_2
        then MarkedBackness backness_1
        else UnmarkedBackness

    unmarkRounding rounding_1 rounding_2 =
      if rounding_1 == rounding_2
        then MarkedRounding rounding_1
        else UnmarkedRounding

-- This function
-- takes any unmarked attributes in the phoneme definition,
-- and returns a list with all possible phonemes that have that attribute.
similarPhonemesTo :: UnmarkablePhonet -> [Phonet]
similarPhonemesTo (UnmarkableConsonant voice_1 place_1 manner_1 airstream_1) =
  let voice' = toList (similarInVoice voice_1)
      place' = toList (similarInPlace place_1)
      manner' = toList (similarInManner manner_1)
      airstream' = toList (similarInAirstream airstream_1)
   in [Consonant v p m a | p <- place', v <- voice', m <- manner', a <- airstream']
similarPhonemesTo (UnmarkableVowel height_1 backness_1 rounding_1 voice_1) =
  let voice' = toList (similarInVoice voice_1)
      height' = toList (similarInHeight height_1)
      backness' = toList (similarInBackness backness_1)
      rounding' = toList (similarInRounding rounding_1)
   in [Vowel h b r v | h <- height', b <- backness', r <- rounding', v <- voice']

similarInVoice :: UnmarkableVocalFolds -> NonEmpty VocalFolds
similarInVoice voice_1 =
  case voice_1 of
    MarkedVocalFolds x -> one x
    UnmarkedVocalFolds -> vocalFoldStates

similarInPlace :: UnmarkablePlace -> NonEmpty Place
similarInPlace place_1 =
  case place_1 of
    MarkedPlace x -> one x
    UnmarkedPlace -> placeStates

similarInManner :: UnmarkableManner -> NonEmpty Manner
similarInManner manner_1 =
  case manner_1 of
    MarkedManner x -> one x
    UnmarkedManner -> mannerStates

similarInAirstream :: UnmarkableAirstream -> NonEmpty Airstream
similarInAirstream airstream_1 =
  case airstream_1 of
    MarkedAirstream x -> one x
    UnmarkedAirstream -> airstreamStates

similarInHeight :: UnmarkableHeight -> NonEmpty Height
similarInHeight height_1 =
  case height_1 of
    MarkedHeight x -> one x
    UnmarkedHeight -> heightStates

similarInBackness :: UnmarkableBackness -> NonEmpty Backness
similarInBackness backness_1 =
  case backness_1 of
    MarkedBackness x -> one x
    UnmarkedBackness -> backnessStates

similarInRounding :: UnmarkableRounding -> NonEmpty Rounding
similarInRounding rounding_1 =
  case rounding_1 of
    MarkedRounding x -> one x
    UnmarkedRounding -> roundingStates

-- The following function returns whether an articulation is
-- considered impossible according to the IPA (pulmonic) consonants chart.
-- Does not work for other values.
impossible :: Phonet -> Bool
impossible p = case p of
  (Consonant Voiced Pharyngeal Plosive PulmonicEgressive) ->
    True
  (Consonant VoicedAspirated Pharyngeal Plosive PulmonicEgressive) ->
    True
  (Consonant Voiceless Glottal Plosive PulmonicEgressive) ->
    False -- [ʔ] is not impossible.
  (Consonant _ Glottal Fricative PulmonicEgressive) ->
    False -- [h] and [ɦ] are not impossible.
  (Consonant _ Glottal _ PulmonicEgressive) ->
    True -- all other pulmonary egressive consonants are impossible..
  (Consonant _ Pharyngeal Nasal PulmonicEgressive) ->
    True
  (Consonant _ Pharyngeal LateralFricative PulmonicEgressive) ->
    True
  (Consonant _ Pharyngeal LateralApproximant PulmonicEgressive) ->
    True
  (Consonant _ Velar Trill PulmonicEgressive) ->
    True
  (Consonant _ Velar TapOrFlap PulmonicEgressive) ->
    True
  (Consonant _ Bilabial LateralFricative PulmonicEgressive) ->
    True
  (Consonant _ Bilabial LateralApproximant PulmonicEgressive) ->
    True
  (Consonant _ LabioDental LateralFricative PulmonicEgressive) ->
    True
  (Consonant _ LabioDental LateralApproximant PulmonicEgressive) ->
    True
  _ ->
    False -- Everything else is assumed to be possible.

-- | This is a list of the sounds of English. Just the basic ones.
--   It is somewhat more complicated in reality, but for now this will
--   suffice.
--   This following sound inventory of English is from page 20 of
--   (2013, Elizabeth C. Zsiga, The Sounds of Language)
englishPhonetInventory :: PhonetInventory
englishPhonetInventory =
  PhonetInventory
    ( fromList
        [ Consonant Voiced Bilabial Plosive PulmonicEgressive,
          Consonant Voiceless Bilabial Plosive PulmonicEgressive,
          Consonant Voiced Alveolar Plosive PulmonicEgressive,
          Consonant Voiceless Alveolar Plosive PulmonicEgressive,
          Consonant Voiced Velar Plosive PulmonicEgressive,
          Consonant Voiceless Velar Plosive PulmonicEgressive,
          Consonant Voiceless Glottal Plosive PulmonicEgressive,
          Consonant Voiced LabioDental Fricative PulmonicEgressive,
          Consonant Voiceless LabioDental Fricative PulmonicEgressive,
          Consonant Voiced Dental Fricative PulmonicEgressive,
          Consonant Voiceless Dental Fricative PulmonicEgressive,
          Consonant Voiced Alveolar Fricative PulmonicEgressive,
          Consonant Voiceless Alveolar Fricative PulmonicEgressive,
          Consonant Voiced PostAlveolar Fricative PulmonicEgressive,
          Consonant Voiceless PostAlveolar Fricative PulmonicEgressive,
          Consonant Voiceless Glottal Fricative PulmonicEgressive,
          Consonant Voiced PostAlveolar Affricate PulmonicEgressive,
          Consonant Voiceless PostAlveolar Affricate PulmonicEgressive,
          Consonant Voiced Bilabial Nasal PulmonicEgressive,
          Consonant Voiced Alveolar Nasal PulmonicEgressive,
          Consonant Voiced Velar Nasal PulmonicEgressive,
          -- The Postalveolar version is technically correct, even though the convention
          -- is to write it in IPA as if it were alveolar. See
          -- Wikipedia article titled "Voiced alveolar and postalveolar approximants"
          -- at the following URL:
          -- https://en.wikipedia.org/wiki/Voiced_alveolar_and_postalveolar_approximants
          Consonant Voiced PostAlveolar Approximant PulmonicEgressive,
          Consonant Voiced Palatal Approximant PulmonicEgressive,
          Consonant Voiced LabialVelar Approximant PulmonicEgressive,
          Vowel Close Front Unrounded Voiced, -- "i"
          Vowel Close Back Rounded Voiced, -- "u"

          -- Near-close Vowels:
          Vowel NearClose Front Unrounded Voiced, -- "ɪ"
          Vowel NearClose Back Rounded Voiced, --  "ʊ"

          -- Close-mid Vowels:
          Vowel CloseMid Front Unrounded Voiced, -- "e"
          Vowel CloseMid Back Rounded Voiced, -- "o"

          -- Mid Vowels:
          Vowel Mid Central Unrounded Voiced, -- "ə"

          -- Open-mid Vowels:
          Vowel OpenMid Front Unrounded Voiced, -- "ɛ"
          Vowel OpenMid Central Unrounded Voiced, -- "ɜ"
          Vowel OpenMid Back Unrounded Voiced, --  "ʌ"
          Vowel OpenMid Back Rounded Voiced, -- "ɔ"

          -- Near-open
          Vowel NearOpen Front Unrounded Voiced, -- "æ"
          Vowel NearOpen Central Unrounded Voiced, -- "ɐ"

          -- Open Vowels:
          Vowel Open Back Unrounded Voiced, -- "ɑ"
          Vowel Open Back Rounded Voiced -- "ɒ"
        ]
    )

-- I added some English vowels. I did not choose any specific dialect.
-- I got all my information from the Wikipedia page titled
-- "English Phonology"
-- at the following URL: https://en.wikipedia.org/wiki/English_phonology#Vowels
-- on Monday, February 24, 2020.
-- To do: Get better information on English vowels from a more reliable source.
-- To do: model separate dialects of English or only one.

-- To do: find a more suitable name than exponentials.
-- They only look like exponentials if you consider how they
-- look similar to mathematical notation for exponentials.
-- Really, they should be named something different.
exponentials :: NonEmpty Text
exponentials = exponentialsBefore <> exponentialsAfter

exponentialsBefore :: NonEmpty Text
exponentialsBefore = fromList ["ⁿ"]

exponentialsAfter :: NonEmpty Text
exponentialsAfter = diacriticsAndSuprasegmentals <> fromList ["ː", "ˑ"]

-- |
-- Whether an IPA character is written above the base line
-- and to the right of the previous character,
-- like how exponents of a power are written
-- in mathematical notation.
isExponential :: Text -> Bool
isExponential character = character `elem` exponentials

-- |
-- Whether a diacritic goes above
-- the character it is placed on.
isDiacriticAbove :: Text -> Bool
isDiacriticAbove "̊" = True
isDiacriticAbove _   = False

-- |
-- Whether a diacritic goes below
-- the character which it is placed on.
isDiacriticBelow :: Text -> Bool
isDiacriticBelow "̥" = True
isDiacriticBelow _   = False

-- |
-- When given a diacritic that goes above,
-- replaces it with one that goes below,
-- and has the same meaning.
-- otherwise does nothing.
lowerDiacritic :: Text -> Text
lowerDiacritic "̊" = "̥"
lowerDiacritic x   = x

-- |
-- When given a diacritic that goes below,
-- replaces it with one that goes below, and
-- has the same meaning;
-- otherwise it does nothing.
raiseDiacritic :: Text -> Text
raiseDiacritic "̥" = "̊"
raiseDiacritic x   = x

-- |
-- Whether a character (but not a diacritic)
-- takes up space
-- below the imaginary horizontal line
-- on which it is written.
--
-- This could be useful later for determining
-- where to put diacritics so that
-- they are readable.
ascenders :: NonEmpty Text
ascenders =
  fromList
    [ "b",
      "t",
      "d",
      "k",
      "ʔ",
      "f",
      "θ",
      "ð",
      "ħ",
      "ʕ",
      "h",
      "ɦ",
      "ɬ",
      "l",
      "ʎ",
      "ʘ",
      "ɓ",
      "ǀ",
      "ɗ",
      "ǃ",
      "ǂ",
      "ɠ",
      "ʄ",
      "ǁ",
      "ʛ",
      "ɺ",
      "ʢ",
      "ʡ",
      "ɤ",
      "ʈ",
      "ɖ",
      "ɸ",
      "β",
      "ʃ",
      "ɮ",
      "ɭ",
      "ɧ"
    ]

isAscender :: Text -> Bool
isAscender character = character `elem` ascenders

descenders :: NonEmpty Text
descenders =
  fromList
    [ "p",
      "ɟ",
      "g",
      "q",
      "ɱ",
      "ɽ",
      "ʒ",
      "ʂ",
      "ʐ",
      "ç",
      "ʝ",
      "ɣ",
      "χ",
      "ɻ",
      "j",
      "ɰ",
      "ɥ",
      "y",
      "ɳ",
      "ɲ",
      "ŋ",
      "ʈ",
      "ɖ",
      "ɸ",
      "β",
      "ʃ",
      "ɮ",
      "ɧ"
    ]
    -- We don't include the retroflex l i.e <ɭ> because, even though it is a descender,
    -- There is more free space under it than above



-- |
-- Whether a character (but not a diacritic)
-- takes up space
-- below the imaginary horizontal line
-- on which it is written.
--
-- This could be useful later for determining
-- where to put diacritics so that
-- they are readable.
isDescender :: Text -> Bool
isDescender character = character `elem` descenders

-- |
-- Prevent placement of diacritics below a full-width
-- character,
-- when doing so would likely make the result
-- difficult to read, whenever there is another
-- diacritic with the same meaning, but can go above.
-- And vice-versa (above - below).
--
-- Only support the voiceless diacritic so far.
preventProhibitedCombination :: Text -> Text
preventProhibitedCombination ss
  | T.null ss = ""
  | T.length ss == 1 = ss
  | otherwise =
    let firstCharacter = one (T.head ss) :: Text
        secondCharacter = one (T.index ss 1) :: Text
        rest = T.tail (T.tail ss)
     in if isAscender firstCharacter && isDiacriticAbove secondCharacter
          then firstCharacter <> lowerDiacritic secondCharacter <> rest
          else
            if isDescender firstCharacter && isDiacriticBelow secondCharacter
              then firstCharacter <> raiseDiacritic secondCharacter <> rest
              else ss

graphemesOfIPA :: NonEmpty Text
graphemesOfIPA =
  consonantsPulmonic
    <> consonantsNonPulmonic
    <> otherSymbols
    <> vowels
    <> suprasegmentals
    <> toneAndWordAccents
    <> diacriticsAndSuprasegmentals

-- See:
--www.internationalphoneticassociation.org/sites/default/files/IPA_Kiel_2015.pdf
-- For the source of this information..

consonants :: NonEmpty Text
consonants = consonantsPulmonic <> consonantsNonPulmonic <> otherSymbols

-- | IPA text that is not a semantic modifier to what is before or after it.
--   This includes vowels, and consonants. It excludes all diacritics.
strictSegmentals :: NonEmpty Text
strictSegmentals = consonants <> vowels

-- CONSONANTS (PULMONIC)
consonantsPulmonic :: NonEmpty Text
consonantsPulmonic =
  plosivePulmonic
    <> nasalPulmonic
    <> trillPulmonic
    <> tapOrFlapPulmonic
    <> fricativePulmonic
    <> lateralFricativePulmonic
    <> approximantPulmonic
    <> lateralApproximantPulmonic

plosivePulmonic :: NonEmpty Text
plosivePulmonic =
  fromList
    [ "p",
      "b",
      "t",
      "d",
      "ʈ",
      "ɖ",
      "c",
      "ɟ",
      "k",
      "g",
      "q",
      "ɢ",
      "ʔ"
    ]

nasalPulmonic :: NonEmpty Text
nasalPulmonic = fromList ["m", "ɱ", "n", "ɳ", "ɲ", "ŋ", "ɴ"]

trillPulmonic :: NonEmpty Text
trillPulmonic = fromList ["ʙ", "r", "ʀ"]

tapOrFlapPulmonic :: NonEmpty Text
tapOrFlapPulmonic = fromList ["ⱱ", "ɾ", "ɽ"]

fricativePulmonic :: NonEmpty Text
fricativePulmonic =
  fromList
    [ "ɸ",
      "β",
      "f",
      "v",
      "θ",
      "ð",
      "s",
      "z",
      "ʃ",
      "ʒ",
      "ʂ",
      "ʐ",
      "ç",
      "ʝ",
      "x",
      "ɣ",
      "χ",
      "ʁ",
      "ħ",
      "ʕ",
      "h",
      "ɦ"
    ]

lateralFricativePulmonic :: NonEmpty Text
lateralFricativePulmonic = fromList ["ɬ", "ɮ"]

approximantPulmonic :: NonEmpty Text
approximantPulmonic = fromList ["ʋ", "ɹ", "ɻ", "j", "ɰ"]

lateralApproximantPulmonic :: NonEmpty Text
lateralApproximantPulmonic = fromList ["l", "ɭ", "ʎ", "ʟ"]

consonantsNonPulmonic :: NonEmpty Text
consonantsNonPulmonic =
  fromList
    -- Clicks   Voiced implosives
    [ "ʘ",
      "ɓ", -- Bilabial
      "ǀ" {- Dental -},
      "ɗ", -- Dental/alveolar
      "ǃ" {-  (Post)alveolar -},
      "ʄ",
      "ǂ",
      "ɠ",
      "ǁ",
      "ʛ"
    ]

otherSymbols :: NonEmpty Text
otherSymbols =
  fromList
    [ "ʍ",
      "ɕ",
      "w",
      "ʑ",
      "ɥ",
      "ɺ",
      "ʜ",
      "ɧ",
      "ʢ",
      "ʡ"
    ]

vowels :: NonEmpty Text
vowels =
  fromList
    [ "i",
      "y",
      "ɨ",
      "ʉ",
      "ɯ",
      "u", -- Close
      "ɪ",
      "ʏ",
      "ʊ",
      "e",
      "ø",
      "ɘ",
      "ɵ",
      "ɤ",
      "o", -- Close-mid
      "ə",
      "ɛ",
      "œ",
      "ɜ",
      "ɞ",
      "ʌ",
      "ɔ", -- Open-mid
      "æ",
      "ɐ",
      "a",
      "ɶ",
      "ɑ",
      "ɒ" -- Open
    ]

suprasegmentals :: NonEmpty Text
suprasegmentals =
  fromList
    [ "ˈ", -- Primary stress
      "ˌ", -- Secondary stress
      "ː", -- Long
      "ˑ", -- Half long
      "̆", -- Extra short
      "|", -- Minor (foot) group
      "‖", -- Major (intonation) group
      ".", -- Syllable break
      "‿" -- Linking (absence of a break
    ]

toneAndWordAccents :: NonEmpty Text
toneAndWordAccents =
  fromList
    {- Level -}
    [ "˥",
      "̋", -- Extra high
      "˦",
      "́", -- High
      "˧",
      "̄", -- Mid
      "˨",
      "̀", -- Low
      "˩",
      "̏", -- Extra low
      "ꜜ", -- Downstep
      "ꜛ", -- Upstep

      {- Contour -}
      "̌", -- Rising
      "̂", -- Falling
      "᷄", -- High rising
      "᷅", -- Low rising
      "᷈", -- Rising-falling
      "↗", -- Global rise
      "↘" -- Global fall
    ]

diacriticsAndSuprasegmentals :: NonEmpty Text
diacriticsAndSuprasegmentals =
  fromList
    [ "̥", -- Voiceless
      "̊", -- Voiceless (diacritic placed above symbol with descender)
      "̤", -- Breathy voiced
      -- End of first row.
      "̬", -- Voiced
      "̰", -- Creaky voiced
      "̺", -- Apical
      -- End of second row.
      "ʰ", -- Aspirated
      "̼", -- Linguolabial
      "̻", -- Laminal
      -- End of third row.
      "̹", -- More rounded
      "ʷ", -- Labialised
      "̃", -- Nasalised
      -- End of fourth row.
      "̜", -- Less rounded
      "ʲ", -- Palatalised
      "ⁿ", -- Pre/post nasalised
      "̟", -- Advanced
      "ˠ", -- Velarised
      "ˡ", -- Lateral release
      "̠", -- Retracted
      "ˤ", -- Pharyngealised
      "̚", -- No audible release
      "̈", -- Centralised
      -- To do: Figure out if we cannot add a diacritic for "Velarized or Pharyngealized
      "̽", -- Mid centralised
      "̝", -- Raised
      "̩", -- Syllabic
      "̞", -- Lowered
      "̯", -- Non-syllabic
      "̘", -- Advanced tongue root
      "˞", -- Rhoticity
      "̙", -- Retracted tongue root
      "ʼ", -- Ejective
      "̍", -- Syllabic (diacritic placed above)
      "̪", -- Dental
      "̣", -- Closer variety/Fricative
      "̇" -- Palatalization/Centralization
    ]

showIPA :: PhonetInventory -> Text
showIPA (PhonetInventory phonetes) = sconcat (fmap constructIPA phonetes)

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
  "d͡ʒ" -> Just (Consonant Voiced PostAlveolar Affricate PulmonicEgressive)
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

retractPhonet :: Maybe Phonet -> Maybe Phonet
retractPhonet (Just (Consonant v p m a)) = Just (Consonant v (retractedPlace p) m a)
retractPhonet _ = Nothing

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

deaspirate :: Phonet -> Phonet
deaspirate (Consonant VoicedAspirated place manner airstream) =
  Consonant Voiced place manner airstream
deaspirate (Consonant VoicelessAspirated place_1 manner_1 airstream_1) =
  Consonant Voiceless place_1 manner_1 airstream_1
deaspirate x = x

decreak :: Phonet -> Phonet
decreak (Consonant CreakyVoiced place manner airstream) =
  Consonant Voiced place manner airstream
decreak x = x

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

-- Go to Section 12.2 of the textbook to understand
-- the concept of phonological features.

-- Given a binary feature, and another feature.
-- returns whether they are the same kind of feature.
-- They don't have to be the same polarity.
-- For example, [+voice] and [−voice] are mutually relevant features.
--   As are [+sonorant] and [+sonorant].
--   But [+sonorant] and [+voice] are not relevant because
-- "voice" and "sonorant" are different.
relevantBinary :: (Polarity -> PhonemeFeature) -> PhonemeFeature -> Bool
relevantBinary feature otherFeature =
  otherFeature == feature Plus || otherFeature == feature Minus

binaryDifference ::
  (Polarity -> PhonemeFeature) ->
  [PhonemeFeature] ->
  [PhonemeFeature] ->
  (Maybe PhonemeFeature, Maybe PhonemeFeature)
binaryDifference feature list_1 list_2
  | relevantList_1 == relevantList_2 =
    (Nothing, Nothing)
  | otherwise =
    (relevantList_1 !!? 0, relevantList_2 !!? 0)
  where
    relevantList_1 = filter (relevantBinary feature) list_1
    relevantList_2 = filter (relevantBinary feature) list_2

unaryDifference ::
  PhonemeFeature ->
  [PhonemeFeature] ->
  [PhonemeFeature] ->
  (Maybe PhonemeFeature, Maybe PhonemeFeature)
unaryDifference feature list_1 list_2
  | (feature `elem` list_1) == (feature `elem` list_2) = (Nothing, Nothing)
  | feature `elem` list_1 && feature `notElem` list_2 = (Just feature, Nothing)
  | otherwise = (Nothing, Just feature)

-- | This function takes two lists of phoneme features
-- and returns how they differ. Any phonemic
-- feature present in one list, and absent in the other
-- will be represented; and any phonemic
-- feature that is positive in one list but absent
-- in the other will be represented.
difference ::
  [PhonemeFeature] ->
  [PhonemeFeature] ->
  [(Maybe PhonemeFeature, Maybe PhonemeFeature)]
difference list_1 list_2 =
  [ binaryDifference SyllabicFeature list_1 list_2,
    binaryDifference ConsonantalFeature list_1 list_2,
    binaryDifference SonorantFeature list_1 list_2,
    binaryDifference ContinuantFeature list_1 list_2,
    binaryDifference VoiceFeature list_1 list_2,
    binaryDifference AdvancedTongueRootFeature list_1 list_2,
    unaryDifference NasalFeature list_1 list_2,
    unaryDifference LateralFeature list_1 list_2,
    unaryDifference DelayedReleaseFeature list_1 list_2,
    unaryDifference SpreadGlottisFeature list_1 list_2,
    unaryDifference ConstrictedGlottisFeature list_1 list_2,
    unaryDifference LabialFeature list_1 list_2,
    unaryDifference CoronalFeature list_1 list_2,
    unaryDifference DorsalFeature list_1 list_2,
    unaryDifference PharyngealFeature list_1 list_2,
    unaryDifference LaryngealFeature list_1 list_2,
    binaryDifference RoundFeature list_1 list_2,
    binaryDifference AnteriorFeature list_1 list_2,
    binaryDifference DistributedFeature list_1 list_2,
    binaryDifference StridentFeature list_1 list_2,
    binaryDifference HighFeature list_1 list_2,
    binaryDifference LowFeature list_1 list_2,
    binaryDifference BackFeature list_1 list_2
  ]

-- |
-- Vowels are [+syllabic]
-- Consonants (glides included) are [-syllabic].
--
-- (Source: page 258)
--
syllabic :: Phonet -> Maybe PhonemeFeature
syllabic Vowel {}     = Just (SyllabicFeature Plus)
syllabic Consonant {} = Just (SyllabicFeature Minus)

-- |
-- Whether a segment is a glide.
isGlide :: Phonet -> Bool
isGlide p = case p of
  (Consonant _ Palatal Approximant PulmonicEgressive)       -> True
  (Consonant _ LabialVelar Approximant PulmonicEgressive)   -> True
  (Consonant _ LabialPalatal Approximant PulmonicEgressive) -> True
  (Consonant _ Velar Approximant PulmonicEgressive)         -> True
  _                                                         -> False

-- |
-- Vowels are [-consonantal].
-- Glides are [-consonantal].
-- Consonants (that are not glides) are [+consonantal].
--
-- (Source: page 258)
consonantal :: Phonet -> Maybe PhonemeFeature
consonantal p = case p of
  Vowel {} -> Just (ConsonantalFeature Minus)
  Consonant {}
    | isGlide p -> Just (ConsonantalFeature Minus)
    | otherwise -> Just (ConsonantalFeature Plus)

-- |
-- Oral stops are [-sonorant].
-- Affricates are [-sonorant].
-- Fricatives are [-sonorant].
-- Nasals are [+sonorant].
-- Approximants are [+sonorant].
-- Laterals are [+sonorant].
-- Vowels are [+sonorant].
-- Glides are [+sonorant].
--
-- (Source: page 258)
sonorant :: Phonet -> Maybe PhonemeFeature
sonorant p = case p of
  (Consonant _ _ Plosive _) -> Just (SonorantFeature Minus)
  (Consonant _ _ Affricate _) -> Just (SonorantFeature Minus)
  (Consonant _ _ Fricative _) -> Just (SonorantFeature Minus)
  (Consonant _ _ Nasal _) -> Just (SonorantFeature Plus)
  (Consonant _ _ Approximant _) -> Just (SonorantFeature Plus)
  (Consonant _ _ Lateral _) -> Just (SonorantFeature Plus)
  Vowel {} -> Just (SonorantFeature Plus)
  Consonant {}
    | isGlide p -> Just (SonorantFeature Plus)
    | otherwise -> Just (SonorantFeature Minus)

-- |
-- Oral stops are [-continuant].
-- Nasals stops are [-continuant].
-- Affricates are [-continuant].
-- Fricatives are [+continuant].
-- Approximants are [+continuant].
-- Vowels are [+continuant].
-- Glides are [+continuant].
--
-- (Source: page 258)
--
--   Aside: we do not define lateral approximants for [+/-continuant] because the
--   textbook puts it in parentheses. Usually this means, it depends on
--   the language under study or
--   it depends on the linguist.
--   Lateral approximants may be considered [+continuant]. (arguable)
--   (see chart on page 259))
--
continuant :: Phonet -> Maybe PhonemeFeature
continuant p = case p of
  (Consonant _ _ Plosive _) -> Just (ContinuantFeature Minus)
  (Consonant _ _ Nasal _) -> Just (ContinuantFeature Minus)
  (Consonant _ _ Affricate _) -> Just (ContinuantFeature Minus)
  (Consonant _ _ Approximant _) -> Just (ContinuantFeature Plus)
  Vowel {} -> Just (ContinuantFeature Plus)
  Consonant {}
    | isGlide p -> Just (ContinuantFeature Plus)
    | otherwise -> Nothing

-- |
-- Nasal consonants are [nasal].
-- -- to do: add support for nasal vowels.
-- All other segments are not defined for [nasal].
nasal :: Phonet -> Maybe PhonemeFeature
nasal (Consonant _ _ Nasal _) = Just NasalFeature
nasal _                       = Nothing

-- |
-- Lateral consonants are [lateral].
-- Lateral approximant consonants are [lateral].
-- Lateral fricative consonants are [lateral].
-- Lateral flap consonants are [lateral].
-- All other segments are not defined for [lateral].
lateral :: Phonet -> Maybe PhonemeFeature
lateral p = case p of
  (Consonant _ _ Lateral _)            -> Just LateralFeature
  (Consonant _ _ LateralApproximant _) -> Just LateralFeature
  (Consonant _ _ LateralFricative _)   -> Just LateralFeature
  (Consonant _ _ LateralFlap _)        -> Just LateralFeature
  _                                    -> Nothing

-- |
-- Affricates are [+delayed release].
-- All other segments are [-delayed release].
--
-- (Source: page 260)
delayedRelease :: Phonet -> Maybe PhonemeFeature
delayedRelease (Consonant _ _ Affricate _) = Just DelayedReleaseFeature
delayedRelease _                           = Nothing

-- |
-- Bilabial consonants are [labial].
-- Labio-dental consonants are [labial].
-- All other segments are undefined for [labial].
--
-- (Source: page 264)
labial :: Phonet -> Maybe PhonemeFeature
labial p = case p of
  (Consonant _ Bilabial _ _)    -> Just LabialFeature
  (Consonant _ LabioDental _ _) -> Just LabialFeature
  _                             -> Nothing

-- |
-- Dentals are [coronal].
-- Alveolars are [coronal] also.
-- Alveolopalatals are [coronal] also.
-- Retroflexes are [coronal] also.
-- Palatals are [coronal] also.
--
-- Post-alveolars are [coronal] also.
--
-- All other sounds are undefined for [coronal].
--
-- (Source: page 264)
-- (The fact that Post-alveolar consonants are coronal is indicated by
--  Table 12. on page 265.)
coronal :: Phonet -> Maybe PhonemeFeature
coronal p = case p of
  (Consonant _ Dental _ _)         -> Just CoronalFeature
  (Consonant _ Alveolar _ _)       -> Just CoronalFeature
  (Consonant _ AlveoloPalatal _ _) -> Just CoronalFeature
  (Consonant _ Retroflex _ _)      -> Just CoronalFeature
  (Consonant _ Palatal _ _)        -> Just CoronalFeature
  (Consonant _ PostAlveolar _ _)   -> Just CoronalFeature
  _                                -> Nothing

-- |
-- Palatals are [dorsal].
--
--   Aside: alveolo-palatals do not seem to be dorsals,
--   although the table 12.4 is confusing
--   because it uses the IPA symbol for one.
--
-- Velars are [dorsal].
-- Uvulars are [dorsal].
-- All other segments are undefined for [dorsal].
dorsal :: Phonet -> Maybe PhonemeFeature
dorsal p = case p of
  (Consonant _ Palatal _ _) -> Just DorsalFeature
  (Consonant _ Velar _ _)   -> Just DorsalFeature
  (Consonant _ Uvular _ _)  -> Just DorsalFeature
  _                         -> Nothing

-- |
-- Pharyngeal fricatives are [pharyngeal].
-- All other segments are undefined for [pharyngeal].
--
-- (Source: page 264)
pharyngeal :: Phonet -> Maybe PhonemeFeature
pharyngeal (Consonant _ Pharyngeal Fricative _) = Just PharyngealFeature
pharyngeal _                                    = Nothing

-- |
-- Glottal consonants are [laryngeal].
-- All other segments are undefined for [laryngeal].
--
-- (Source: page 265)
laryngeal :: Phonet -> Maybe PhonemeFeature
laryngeal (Consonant _ Glottal _ _) = Just LaryngealFeature
laryngeal _                         = Nothing

-- |
-- Voiced Aspirated consonants are [+voice].
-- Voiced consonants are [+voice].
-- Voiced vowels are [+voice].
-- All other segments are [-voice].
voice :: Phonet -> Maybe PhonemeFeature
voice p = case p of
  (Consonant Voiceless Glottal Plosive PulmonicEgressive) ->
    Just (VoiceFeature Minus) -- The voiceless glottal plosive is [-voice]
  (Consonant VoicedAspirated _ _ _) ->
    Just (VoiceFeature Plus)
  (Consonant Voiced _ _ _) ->
    Just (VoiceFeature Plus)
  (Vowel _ _ _ Voiced) ->
    Just (VoiceFeature Plus)
  _ ->
    Just (VoiceFeature Minus)

-- |
-- Voiceless aspirated plosives are [spread glottis].
-- Voiced aspirated plosives are [spread glottis].
-- All other segments are not defined for [spread glottis].
-- (Source: page 262)
spreadGlottis :: Phonet -> Maybe PhonemeFeature
spreadGlottis p = case p of
  (Consonant VoicelessAspirated _ Plosive _) -> Just SpreadGlottisFeature
  (Consonant VoicedAspirated _ Plosive _)    -> Just SpreadGlottisFeature
  _                                          -> Nothing

-- |
-- Ejectives have the feature [constricted glottis].
-- Glottal stop have the feature [constricted glottis].
-- Creaky voiced sonorants have the feature [constricted glottis].
--
-- (Source: page 262)
constrictedGlottis :: Phonet -> Maybe PhonemeFeature
constrictedGlottis p = case p of
  (Consonant _ Glottal Plosive _) ->
    Just ConstrictedGlottisFeature
  (Consonant CreakyVoiced _ _ _) ->
    if sonorant p == Just (SonorantFeature Plus)
      then Just ConstrictedGlottisFeature
      else Nothing
  (Vowel _ _ _ CreakyVoiced) ->
    if sonorant p == Just (SonorantFeature Plus)
      then Just ConstrictedGlottisFeature
      else Nothing
  _ ->
    Nothing

-- |
-- Dentals are [+anterior].
-- Alveolars are [+anterior].
-- Post-alveolars are [-anterior].
-- Retroflexes are [-anterior].
-- Palatals are [-anterior].
--
-- (Source: page 265)
--
-- TODO: answer the question:
-- Question: Are Alveolo-palatals [+anterior], or [-anterior]?
-- Alveolo-palatals are [-anterior].
-- (SOURCE: not found)
anterior :: Phonet -> Maybe PhonemeFeature
anterior p = case p of
  (Consonant _ Dental _ _)         -> Just (AnteriorFeature Plus)
  (Consonant _ Alveolar _ _)       -> Just (AnteriorFeature Plus)
  (Consonant _ PostAlveolar _ _)   -> Just (AnteriorFeature Minus)
  (Consonant _ Retroflex _ _)      -> Just (AnteriorFeature Minus)
  (Consonant _ Palatal _ _)        -> Just (AnteriorFeature Minus)
  (Consonant _ AlveoloPalatal _ _) -> Just (AnteriorFeature Minus)
  _                                -> Nothing

distributed :: Phonet -> Maybe PhonemeFeature
distributed p = case p of
  (Consonant _ Dental _ _)         -> Just (DistributedFeature Plus)
  (Consonant _ Alveolar _ _)       -> Just (DistributedFeature Minus)
  (Consonant _ PostAlveolar _ _)   -> Just (DistributedFeature Plus)
  (Consonant _ Retroflex _ _)      -> Just (DistributedFeature Minus)
  (Consonant _ Palatal _ _)        -> Just (DistributedFeature Plus)
  (Consonant _ AlveoloPalatal _ _) -> Just (DistributedFeature Plus)
  _                                -> Nothing

-- |
-- Alveolar fricatives are [+strident].
-- Alveolar affricates are [+strident], also.
-- Post-alveolar fricatives are [+strident], also.
-- Post-alveolar affricates are [+strident], also.
-- Labio-dental fricatives are [+strident] , also.
-- Labio-dental affricates are [+strident] , also.
-- Uvular fricatives are [+strident], also.
-- Uvular affricates are [+strident], also.
--
-- All other fricatives are [-strident].
-- All other affricates are [-strident], also.
--
-- All other segments are undefined for [+/-strident].
--
-- (Source: page 266, under [+/-strident] heading, under the subsection
-- "Natural classes".)
strident :: Phonet -> Maybe PhonemeFeature
strident p = case p of
  (Consonant _ Alveolar Fricative _)     -> Just (StridentFeature Plus)
  (Consonant _ Alveolar Affricate _)     -> Just (StridentFeature Plus)
  (Consonant _ PostAlveolar Fricative _) -> Just (StridentFeature Plus)
  (Consonant _ PostAlveolar Affricate _) -> Just (StridentFeature Plus)
  (Consonant _ LabioDental Fricative _)  -> Just (StridentFeature Plus)
  (Consonant _ LabioDental Affricate _)  -> Just (StridentFeature Plus)
  (Consonant _ Uvular Fricative _)       -> Just (StridentFeature Plus)
  (Consonant _ Uvular Affricate _)       -> Just (StridentFeature Plus)
  (Consonant _ _ Fricative _)            -> Just (StridentFeature Minus)
  (Consonant _ _ Affricate _)            -> Just (StridentFeature Minus)
  _                                      -> Nothing

-- |
-- Palatal consonants are [+high].
-- Alveolo-palatal consonants are [+high].
-- Velar consonants are [+high].
--
-- Uvular consonants are [-high].
-- All other consonants are undefined for [+/-high].
-- Close vowels are [+high].
-- Near-close vowels are [+high].
-- All other vowels are [-high].
high :: Phonet -> Maybe PhonemeFeature
high p = case p of
  (Consonant _ Palatal _ _)        -> Just (HighFeature Plus)
  (Consonant _ AlveoloPalatal _ _) -> Just (HighFeature Plus)
  (Consonant _ Velar _ _)          -> Just (HighFeature Plus)
  (Consonant _ Uvular _ _)         -> Just (HighFeature Minus)
  Consonant {}                     -> Nothing
  (Vowel Close _ _ _)              -> Just (HighFeature Plus)
  (Vowel NearClose _ _ _)          -> Just (HighFeature Plus)
  Vowel {}                         -> Just (HighFeature Minus)

-- |
-- Uvular consonants are [+low].
-- Pharyngeal consonants are [+low].
-- Glottal consonants are [+low].
-- All other consonants are undefined for [+/-low].
-- Open vowels are [+low].
-- Near open vowels are [+low].
-- All other vowels are [-low].
low :: Phonet -> Maybe PhonemeFeature
low p = case p of
  (Consonant _ Uvular _ _)     -> Just (LowFeature Plus)
  (Consonant _ Pharyngeal _ _) -> Just (LowFeature Plus)
  (Consonant _ Glottal _ _)    -> Just (LowFeature Plus)
  Consonant {}                 -> Nothing
  (Vowel Open _ _ _)           -> Just (LowFeature Plus)
  (Vowel NearOpen _ _ _)       -> Just (LowFeature Plus)
  Vowel {}                     -> Just (LowFeature Minus)

-- |
-- Back vowels are [+back].
-- Central vowels are [+back].
-- Front vowels are [-back].
-- All other segments are undefined for [+/-back].
back :: Phonet -> Maybe PhonemeFeature
back p = case p of
  (Vowel _ Back _ _)    -> Just (BackFeature Plus)
  (Vowel _ Central _ _) -> Just (BackFeature Plus)
  (Vowel _ Front _ _)   -> Just (BackFeature Minus)
  _                     -> Nothing

-- |
-- Rounded vowels are [+round].
-- All other vowels are [-round].
-- All other segments are [-round].
lipRound :: Phonet -> Maybe PhonemeFeature
lipRound p = case p of
  (Vowel _ _ Rounded _) -> Just (RoundFeature Plus)
  Vowel {}              -> Just (RoundFeature Minus)
  _                     -> Just (RoundFeature Minus)

-- |
-- Advanced tongue root
atr :: Phonet -> Maybe PhonemeFeature
atr p = case p of
  (Vowel Close Front Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Plus)
  (Vowel CloseMid Front Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Plus)
  (Vowel Close Back Rounded Voiced) ->
    Just (AdvancedTongueRootFeature Plus)
  (Vowel CloseMid Front Rounded Voiced) ->
    Just (AdvancedTongueRootFeature Plus)
  (Vowel CloseMid Back Rounded Voiced) ->
    Just (AdvancedTongueRootFeature Plus)
  (Vowel Close Front Rounded Voiced) ->
    Just (AdvancedTongueRootFeature Plus)
  (Vowel NearOpen Front Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  (Vowel Open Back Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  (Vowel Close Central Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  (Vowel OpenMid Back Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  (Vowel NearClose Front Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  (Vowel NearClose Back Rounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  (Vowel OpenMid Front Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  (Vowel OpenMid Back Rounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  _ ->
    Nothing

-- |
-- Given a phoneme (representation)
-- Gives a feature matrix.
--
-- Note: to non-linguists, feature matrices
-- are 1-dimensional, always displayed
-- as a single column.
--
-- For example:
-- /p/
featureMatrix :: Phonet -> [Maybe PhonemeFeature]
featureMatrix phonete =
  [ consonantal phonete,
    syllabic phonete,
    continuant phonete,
    sonorant phonete,
    delayedRelease phonete,
    anterior phonete,
    distributed phonete,
    strident phonete,
    high phonete,
    low phonete,
    nasal phonete,
    lateral phonete,
    labial phonete,
    coronal phonete,
    dorsal phonete,
    pharyngeal phonete,
    laryngeal phonete,
    back phonete,
    lipRound phonete,
    voice phonete,
    atr phonete,
    spreadGlottis phonete,
    constrictedGlottis phonete
  ]

-- | A function that takes data representing
-- how a phoneme is pronounced, and returns
-- a list of phonemic features.
analyzeFeatures :: Phonet -> [PhonemeFeature]
analyzeFeatures phonete =
  catMaybes (featureMatrix phonete)

showFeatures :: [PhonemeFeature] -> Text
showFeatures features =
  let featuresStrings :: [Text]
      featuresStrings = map showPhonemeFeature features
   in "[" <> T.intercalate "; " featuresStrings <> "]"

toTextFeatures :: Phonet -> Text
toTextFeatures phonete =
  let features = analyzeFeatures phonete
   in showFeatures features

showPhonet :: Phonet -> Text
showPhonet phonet =
  case phonet of
    Consonant v p m a ->
      unwords
        [ showVocalFolds v,
          showPlace p,
          showManner m,
          showAirstream a,
          consonantUIText
        ]
    Vowel h b r v ->
      unwords
        [ showVocalFolds v,
          showRounding r,
          showHeight h,
          showBackness b,
          vowelUIText
        ]

showBackness :: Backness -> Text
showBackness Front   = frontBacknessUIText
showBackness Central = centralBacknessUIText
showBackness Back    = backBacknessUIText

showHeight :: Height -> Text
showHeight height =
  case height of
    Close     -> closeHeightUIText
    NearClose -> nearCloseHeightUIText
    CloseMid  -> closeMidHeightUIText
    Mid       -> midHeightUIText
    OpenMid   -> openMidHeightUIText
    NearOpen  -> nearOpenHeightUIText
    Open      -> openHeightUIText

showRounding :: Rounding -> Text
showRounding Rounded   = roundedRoundingUIText
showRounding Unrounded = unroundedRoundingUIText

showPlace :: Place -> Text
showPlace place_1 =
  case place_1 of
    Bilabial       -> bilabialPlaceUIText
    LabioDental    -> labioDentalPlaceUIText
    Dental         -> dentalPlaceUIText
    Alveolar       -> alveolarPlaceUIText
    PostAlveolar   -> postAlveolarPlaceUIText
    Retroflex      -> retroflexPlaceUIText
    Palatal        -> palatalPlaceUIText
    Velar          -> velarPlaceUIText
    Uvular         -> uvularPlaceUIText
    Pharyngeal     -> pharyngealPlaceUIText
    Glottal        -> glottalPlaceUIText
    Epiglottal     -> epiglottalPlaceUIText
    LabialVelar    -> labialVelarPlaceUIText
    LabialPalatal  -> labialPalatalPlaceUIText
    AlveoloPalatal -> alveoloPalatalPlaceUIText
    PalatoAlveolar -> palatoAlveolarPlaceUIText
    Places ps      -> unwords (toList (fmap showPlace ps))

showManner :: Manner -> Text
showManner manner_1 =
  case manner_1 of
    Plosive            -> plosiveMannerUIText
    Nasal              -> nasalMannerUIText
    Trill              -> trillMannerUIText
    TapOrFlap          -> tapOrFlapMannerUIText
    Approximant        -> approximantMannerUIText
    Fricative          -> fricativeMannerUIText
    Affricate          -> affricateMannerUIText
    LateralFricative   -> lateralFricativeMannerUIText
    LateralApproximant -> lateralApproximantMannerUIText
    LateralFlap        -> lateralFlapMannerUIText
    Lateral            -> lateralMannerUIText

showAirstream :: Airstream -> Text
showAirstream airstream_1 =
  case airstream_1 of
    PulmonicEgressive -> pulmonicEgressiveAirstreamUIText
    Click             -> clickAirstreamUIText
    Implosive         -> implosiveAirstreamUIText

showVocalFolds :: VocalFolds -> Text
showVocalFolds vocalFolds_1 =
  case vocalFolds_1 of
    Voiced             -> voicedVocalFoldsUIText
    Voiceless          -> voicelessVocalFoldsUIText
    VoicedAspirated    -> voicedAspiratedVocalFoldsUIText
    VoicelessAspirated -> voicelessAspiratedVocalFoldsUIText
    CreakyVoiced       -> creakyVoicedVocalFoldsUIText

showPhonetInventory :: PhonetInventory -> Text
showPhonetInventory (PhonetInventory phonetes) =
  sconcat (fmap showPhonet phonetes)

showPolarity :: Polarity -> Text
showPolarity Plus  = "+"
showPolarity Minus = "-"

showPhonemeFeature :: PhonemeFeature -> Text
showPhonemeFeature pf =
  case pf of
    (SyllabicFeature p) -> showPolarity p <> syllabicPhonemeFeatureUIText
    (ConsonantalFeature p) -> showPolarity p <> consonantalPhonemeFeatureUIText
    (SonorantFeature p) -> showPolarity p <> sonorantPhonemeFeatureUIText
    (ContinuantFeature p) -> showPolarity p <> continuantPhonemeFeatureUIText
    (VoiceFeature p) -> showPolarity p <> voicePhonemeFeatureUIText
    (AdvancedTongueRootFeature p) -> showPolarity p <> atrPhonemeFeatureUIText
    NasalFeature -> nasalPhonemeFeatureUIText
    LateralFeature -> lateralPhonemeFeatureUIText
    DelayedReleaseFeature -> delayedReleasePhonemeFeatureUIText
    SpreadGlottisFeature -> spreadGlottisPhonemeFeatureUIText
    ConstrictedGlottisFeature -> constrictedGlottisPhonemeFeatureUIText
    LabialFeature -> labialPhonemeFeatureUIText
    CoronalFeature -> coronalPhonemeFeatureUIText
    DorsalFeature -> dorsalPhonemeFeatureUIText
    PharyngealFeature -> pharyngealPhonemeFeatureUIText
    LaryngealFeature -> laryngealPhonemeFeatureUIText
    (RoundFeature p) -> showPolarity p <> roundPhonemeFeatureUIText
    (AnteriorFeature p) -> showPolarity p <> anteriorPhonemeFeatureUIText
    (DistributedFeature p) -> showPolarity p <> distributedPhonemeFeatureUIText
    (StridentFeature p) -> showPolarity p <> stridentPhonemeFeatureUIText
    (HighFeature p) -> showPolarity p <> highPhonemeFeatureUIText
    (LowFeature p) -> showPolarity p <> lowPhonemeFeatureUIText
    (BackFeature p) -> showPolarity p <> backPhonemeFeatureUIText

analyzeIPAToSPE :: Text -> Text
analyzeIPAToSPE ipaText =
  maybe sorryUnableToCalculate (showFeatures . analyzeFeatures) (analyzeIPA ipaText)
