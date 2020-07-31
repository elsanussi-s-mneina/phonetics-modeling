-- | Handle splitting of IPA graphemes into chunks, so that
--   diacritics go with the non-diacritic characters they modify.

{-# LANGUAGE OverloadedStrings #-}
module GraphemeGrammar where

import Prelude()
import Relude((+), (-), (<), (&&), (<>), (/=), (==), Bool(False, True),
              Char, Int, Maybe(Just, Nothing), NonEmpty, Text,
              elem, filter, fmap, fromList, not, otherwise, one)
import qualified Data.Text as T



-- | Splits text in the International Phonetic Alphabet by
--   phones. This is also called tokenization.
--
--   Note: it does not recognize affricates, unless a tie-bar
--   is provided.
splitByPhonetes :: Text -> [Text]
splitByPhonetes = parseStart

-- | Start parsing some IPA text, in order to
--   chunk it into phonemes.
parseStart :: Text   -- ^ some text in IPA
           -> [Text] -- ^ the same text but split into a list
parseStart x = filter (/= "") (splitByPhonetesPrePostDiacrtic x)

-- | Handle situations where the diacritic character occurs
--   before the main character.
--   Handle strings like "ⁿd".
--   If it doesn't find a main character with a diacritic before
--   it, it will look for a diacritic after the main character.
splitByPhonetesPreDiacritic :: Text   -- ^ text that may contain text with prediacrtics
                            -> [Text] -- ^ a list of IPA characters split
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

-- | Try to split IPA text into a list of IPA text, each element
--   representing phonemes. Handle "dʰ", etc.
splitByPhonetesPostDiacrtic :: Text   -- ^ a string of IPA text not yet split into phonemes
                            -> [Text] -- ^ a list of phonemes represented by IPA text
splitByPhonetesPostDiacrtic text =
  let result = postdiacriticParserFunction text
   in case result of
        Nothing            -> splitByPhonetesNonDiacrtic text
        Just (chunk, rest) -> [chunk] <> parseStart rest

-- | Handle "d", "t", etc. and situations where there is no diacritic.
splitByPhonetesNonDiacrtic :: Text   -- ^ text containing IPA text
                           -> [Text] -- ^ a list of phonemes
splitByPhonetesNonDiacrtic text =
  let result = nondiacriticParserFunction text
   in case result of
        Nothing            -> [text] -- stop parsing!
        Just (chunk, rest) -> [chunk] <> parseStart rest

-- | Parse the part containing diacritic (except the tie-bar).
nondiacriticParserFunction :: Text -- ^ text containing IPA
                           -> Maybe (Text, Text) -- ^ a tuple with the part parsed in the frist part, and the
                                                 --  part not yet parsed after.
nondiacriticParserFunction text =
  if not (T.null text) && isSegmental (T.head text)
    then
      if isTieBarAt 1 text
        then Just (T.take 3 text, T.drop 3 text)
        else Just (T.take 1 text, T.drop 1 text)
    else Nothing

-- | Whether the character in the string at a certain place,
--   represents a consonant.
isConsonantAt :: Int  -- ^ an index within the range of 0, and the length of the string argument
              -> Text -- ^ a text string
              -> Bool -- ^ true if it is a consonant
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

-- | Whether a character is a diacritic that can go after
--   the main character.
isExponentialAfterAt :: Int  -- ^ a number indicating where the character is in the text
                     -> Text -- ^ the text that contains the character
                     -> Bool -- ^ true if the character can be a diacritic after the main character
isExponentialAfterAt = isSuchAt isExponentialAfter

-- | Whether a character at a certain place in a string,
--   is the tie-bar diacritic.
isTieBarAt :: Int  -- ^ a number telling which character in the string
           -> Text -- ^ the string (some text)
           -> Bool -- ^ true if it is a tie-bar
isTieBarAt = isSuchAt isTieBar

-- | Whether a character at a string is of a certain class.
isSuchAt :: (Char -> Bool) -- ^ a function
         -> Int  -- ^ a number indicating which character in the text
         -> Text -- ^ a string
         -> Bool -- ^ whether it is true
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

-- | Parse text that contains IPA text, can parse the next phoneme
--   even if it contains diacritics before and after the main character.
prepostdiacriticParserFunction :: Text -- ^ text to parse
                               -> Maybe (Text, Text) -- ^ a tuple or nothing.
                                                     -- The first part of the tuple is a parsed phoneme,
                                                     -- the second part is the part of the text not parsed yet.
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

-- | Parse IPA text that can contain a diacritic after.
postdiacriticParserFunction :: Text -- ^ text to attempt to parse
                            -> Maybe (Text, Text) -- ^ nothing if it was not parsable, otherwise a tuple with what
                                                  --  was parsed first (IPA text representing a phoneme),
                                                  --  and the part not
                                                  --  parsed yet after.

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

-- | Count how many superscript characters occur one after another, at a
--   specific place in a text (that could modify a previous character).
countPostDiacriticsInARow :: Text -> Int -> Int
countPostDiacriticsInARow sText startIndex =
  if isExponentialAfterAt startIndex sText
    then 1 + countPostDiacriticsInARow sText (startIndex + 1)
    else 0


-- |
-- Whether an IPA character is written above the base line
-- and to the right of the previous character,
-- like how exponents of a power are written
-- in mathematical notation.
isExponential :: Text -> Bool
isExponential character = character `elem` exponentials


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

-- To do: find a more suitable name than exponentials.
-- They only look like exponentials if you consider how they
-- look similar to mathematical notation for exponentials.
-- Really, they should be named something different.
exponentials :: NonEmpty Text
exponentials = exponentialsBefore <> exponentialsAfter

exponentialsBefore :: NonEmpty Text
exponentialsBefore = fromList ["ⁿ"]

exponentialsAfter :: NonEmpty Text
exponentialsAfter = diacriticsAndSuprasegmentals <> fromList ["ː", "ˑ", "̆"]

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

consonantsNonPulmonic :: NonEmpty Text
consonantsNonPulmonic =
  fromList
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

consonants :: NonEmpty Text
consonants = consonantsPulmonic <> consonantsNonPulmonic <> otherSymbols

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


-- | IPA text that is not a semantic modifier to what is before or after it.
--   This includes vowels, and consonants. It excludes all diacritics.
strictSegmentals :: NonEmpty Text
strictSegmentals = consonants <> vowels

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
      "‿" -- Linking (absence of a break)
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

isAscender :: Text -> Bool
isAscender character = character `elem` ascenders

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
