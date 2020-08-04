-- | Handle splitting of IPA graphemes into chunks, so that
--   diacritics go with the non-diacritic characters they modify.
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module GraphemeGrammar where
import Relude((+),  (<), (&&), (<>),(==), Bool(False, True),
              Char, Int, Maybe(Just, Nothing), Text,
              elem, fromList, otherwise, one)
import qualified Data.Text as T

{- Context free grammar of IPA (incomplete, but good enough to start)

We want something like this:

BaseDigraph -> BaseCharacter
             | BaseCharacter VoicingDiacritic
             | BaseCharacter SecondaryArticulationDiacritic
             | BaseCharacter VoicingDiacritic SecondaryArticulationDiacritic
             | Digraph
Digraph -> BaseCharacter TieCharacter BaseCharacter
TieCharacter ->  ͜  | ͡
StrictSegmentals -> a | b | c | t | d | s | ʃ | ʒ | f | r
VoicingDiacritic -> ̥ | ̊ | ̬
SuperscriptAfter -> ʰ
SecondaryArticulationDiacritic ->  ʷ | ʲ | ˤ | ˠ
-}

baseCharacters :: [Char]
baseCharacters = strictSegmentals

secondaryArticulationDiacritics :: [Char]
secondaryArticulationDiacritics = ['ʷ', 'ʲ', 'ˤ', 'ˠ']

voicingDiacritics :: [Char]
voicingDiacritics = ['̥', '̊', '̬']



-- | Uses one parser on the text,
--   then uses the next parser on the remaining
--   text from the first parse.
thenParser
  :: (Text -> Maybe (Text, Text))
  -> (Text -> Maybe (Text, Text))
  -> Text
  -> Maybe (Text, Text)
thenParser firstParser secondParser text =
  case firstParser text of
    Nothing -> Nothing
    Just (parsed, rest)
            -> case secondParser rest of
                 Nothing -> Nothing
                 Just (parsed2, rest2) -> Just (parsed <> parsed2, rest2)


-- | combines parsers by using one or the other.
orParser
  :: (Text -> Maybe (Text, Text))
  -> (Text -> Maybe (Text, Text))
  -> Text
  -> Maybe (Text, Text)
orParser firstParser secondParser text =
  case firstParser text of
    Nothing -> case secondParser text of
                      Nothing -> Nothing
                      Just (parsed, rest) -> Just (parsed, rest)
    Just (parsed, rest) -> Just (parsed, rest)

-- | changes a parser by repeating it an indefinite number
--   of times.
--   So a parser that parses only "a", will parse "aaaaa".
--   A parser that parses only "@", will parse "@@@@", "@@@@@" and
--   so on.
manyParser
  :: (Text -> Maybe (Text, Text))
  -> Text
  -> Maybe (Text, Text)
manyParser subParser text =
  case subParser text of
    Nothing -> Nothing
    Just (parsed, rest)
            -> case manyParser subParser rest of
                  Nothing -> Just (parsed, rest)
                  Just (parsed2, rest2) -> Just (parsed <> parsed2, rest2)

-- | This implements the
-- rule expressed in (BNF) grammar as:
-- digraph -> baseCharacter tieBarCharacter baseCharacter
digraphParser
  :: Text
  -> Maybe (Text, Text)
digraphParser =
  baseCharacterParser `thenParser` tieBarParser `thenParser` baseCharacterParser

tieBarParser
  :: Text
  -> Maybe (Text, Text)
tieBarParser = singleCharParser ['͜', '͡']

voicingDiacriticParser
  :: Text
  -> Maybe (Text, Text)
voicingDiacriticParser = singleCharParser voicingDiacritics

superscriptAfterParser
  :: Text
  -> Maybe (Text, Text)
superscriptAfterParser = singleCharParser superscriptsAfter

superscriptBeforeParser
  :: Text
  -> Maybe (Text, Text)
superscriptBeforeParser = singleCharParser superscriptsBefore

phonemeParser
  :: Text
  -> Maybe (Text, Text)
phonemeParser =
  (baseCharacterParser `thenParser` secondaryArticulationDiacriticParser)
  `orParser`
  (baseCharacterParser `thenParser` voicingDiacriticParser `thenParser` secondaryArticulationDiacriticParser)
  `orParser`
  (baseCharacterParser `thenParser` voicingDiacriticParser)
  `orParser`
  (baseCharacterParser `thenParser` superscriptAfterParser)
  `orParser`
  (superscriptBeforeParser `thenParser` baseCharacterParser `thenParser` superscriptAfterParser)
  `orParser`
  (superscriptBeforeParser `thenParser` baseCharacterParser)
  `orParser`
  digraphParser
  `orParser`
  baseCharacterParser

singleCharParser
  :: [Char]
  -> Text
  -> Maybe (Text, Text)
singleCharParser charList text
  | T.length text == 0 = Nothing
  | (T.index text 0) `elem` charList = Just (T.take 1 text, T.drop 1 text)
  | otherwise = Nothing

secondaryArticulationDiacriticParser
  :: Text
  -> Maybe (Text, Text)
secondaryArticulationDiacriticParser =
  singleCharParser secondaryArticulationDiacritics

baseCharacterParser
  :: Text
  -> Maybe (Text, Text)
baseCharacterParser =
  singleCharParser baseCharacters

-- | Splits text in the International Phonetic Alphabet by
--   phones. This is also called tokenization.
--
--   Note: it does not recognize affricates, unless a tie-bar
--   is provided.
splitIntoPhonemes
  :: Text
  -> [Text]
splitIntoPhonemes text = toListParser phonemeParser text

toListParser
  :: (Text -> Maybe (Text, Text))
  -> Text
  -> [Text]
toListParser parser text =
  case parser text  of
    Just (parsed, rest) -> parsed : toListParser parser rest
    Nothing -> []

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
isSuperscriptAfterAt :: Int  -- ^ a number indicating where the character is in the text
                     -> Text -- ^ the text that contains the character
                     -> Bool -- ^ true if the character can be a diacritic after the main character
isSuperscriptAfterAt = isSuchAt isSuperscriptAfter

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
isSuperscriptAfter :: Char -> Bool
isSuperscriptAfter = elemW superscriptsAfter

-- | Whether a character is a superscript character, that
--   often goes before a full character to modify the
--   full character's meaning.
--   For example in the International Phonetic Alphabet,
--   a superscript `n`.
isSuperscriptBefore :: Char -> Bool
isSuperscriptBefore = elemW superscriptsBefore


-- | Whether a character is used to tie two characters in the
--   international phonetic alphabet together. The tie bar is
--   usually used to indicate an affricate, or double-articulation.
isTieBar :: Char -> Bool
isTieBar x = x `elem` ['͜', '͡']

-- | Create a function that sees whether
-- a character is equal to (the first character in) an element
-- in a list of text
elemW :: [Char] -> (Char -> Bool)
elemW charList = (`elem` charList)

-- | Count how many superscript characters occur one after another, at a
--   specific place in a text (that could modify a previous character).
countPostDiacriticsInARow :: Text -> Int -> Int
countPostDiacriticsInARow sText startIndex =
  if isSuperscriptAfterAt startIndex sText
    then 1 + countPostDiacriticsInARow sText (startIndex + 1)
    else 0


-- |
-- Whether an IPA character is written above the base line
-- and to the right of the previous character,
-- like how exponents of a power are written
-- in mathematical notation.
isSuperscript :: Char -> Bool
isSuperscript character = character `elem` superscripts


plosivePulmonic :: [Char]
plosivePulmonic =
  fromList
    [ 'p',
      'b',
      't',
      'd',
      'ʈ',
      'ɖ',
      'c',
      'ɟ',
      'k',
      'g',
      'q',
      'ɢ',
      'ʔ'
    ]

nasalPulmonic :: [Char]
nasalPulmonic = ['m', 'ɱ', 'n', 'ɳ', 'ɲ', 'ŋ', 'ɴ']

trillPulmonic :: [Char]
trillPulmonic = ['ʙ', 'r', 'ʀ']

tapOrFlapPulmonic :: [Char]
tapOrFlapPulmonic = ['ⱱ', 'ɾ', 'ɽ']

fricativePulmonic :: [Char]
fricativePulmonic =
    [ 'ɸ',
      'β',
      'f',
      'v',
      'θ',
      'ð',
      's',
      'z',
      'ʃ',
      'ʒ',
      'ʂ',
      'ʐ',
      'ç',
      'ʝ',
      'x',
      'ɣ',
      'χ',
      'ʁ',
      'ħ',
      'ʕ',
      'h',
      'ɦ'
    ]

lateralFricativePulmonic :: [Char]
lateralFricativePulmonic = ['ɬ', 'ɮ']

approximantPulmonic :: [Char]
approximantPulmonic = ['ʋ', 'ɹ', 'ɻ', 'j', 'ɰ']

lateralApproximantPulmonic :: [Char]
lateralApproximantPulmonic = ['l', 'ɭ', 'ʎ', 'ʟ']


diacriticsAndSuprasegmentals :: [Char]
diacriticsAndSuprasegmentals =
    [ '̥', -- Voiceless
      '̊', -- Voiceless (diacritic placed above symbol with descender)
      '̤', -- Breathy voiced
      -- End of first row.
      '̬', -- Voiced
      '̰', -- Creaky voiced
      '̺', -- Apical
      -- End of second row.
      'ʰ', -- Aspirated
      '̼', -- Linguolabial
      '̻', -- Laminal
      -- End of third row.
      '̹', -- More rounded
      'ʷ', -- Labialised
      '̃', -- Nasalised
      -- End of fourth row.
      '̜', -- Less rounded
      'ʲ', -- Palatalised
      'ⁿ', -- Pre/post nasalised
      '̟', -- Advanced
      'ˠ', -- Velarised
      'ˡ', -- Lateral release
      '̠', -- Retracted
      'ˤ', -- Pharyngealised
      '̚', -- No audible release
      '̈', -- Centralised
      '̽', -- Mid centralised
      '̝', -- Raised
      '̩', -- Syllabic
      '̞', -- Lowered
      '̯', -- Non-syllabic
      '̘', -- Advanced tongue root
      '˞', -- Rhoticity
      '̙', -- Retracted tongue root
      'ʼ', -- Ejective
      '̍', -- Syllabic (diacritic placed above)
      '̪', -- Dental
      '̣', -- Closer variety/Fricative
      '̇' -- Palatalization/Centralization
    ]

-- To do: find a more suitable name than superscripts.
-- They only look like superscripts if you consider how they
-- look similar to mathematical notation for superscripts.
-- Really, they should be named something different.
superscripts :: [Char]
superscripts = superscriptsBefore <> superscriptsAfter

superscriptsBefore :: [Char]
superscriptsBefore = ['ⁿ']

superscriptsAfter :: [Char]
superscriptsAfter = diacriticsAndSuprasegmentals <>  ['ː', 'ˑ', '̆']

-- |
-- Whether a character (but not a diacritic)
-- takes up space
-- below the imaginary horizontal line
-- on which it is written.
--
-- This could be useful later for determining
-- where to put diacritics so that
-- they are readable.
ascenders :: [Char]
ascenders =
    [ 'b',
      't',
      'd',
      'k',
      'ʔ',
      'f',
      'θ',
      'ð',
      'ħ',
      'ʕ',
      'h',
      'ɦ',
      'ɬ',
      'l',
      'ʎ',
      'ʘ',
      'ɓ',
      'ǀ',
      'ɗ',
      'ǃ',
      'ǂ',
      'ɠ',
      'ʄ',
      'ǁ',
      'ʛ',
      'ɺ',
      'ʢ',
      'ʡ',
      'ɤ',
      'ʈ',
      'ɖ',
      'ɸ',
      'β',
      'ʃ',
      'ɮ',
      'ɭ',
      'ɧ'
    ]

descenders :: [Char]
descenders =
  fromList
    [ 'p',
      'ɟ',
      'g',
      'q',
      'ɱ',
      'ɽ',
      'ʒ',
      'ʂ',
      'ʐ',
      'ç',
      'ʝ',
      'ɣ',
      'χ',
      'ɻ',
      'j',
      'ɰ',
      'ɥ',
      'y',
      'ɳ',
      'ɲ',
      'ŋ',
      'ʈ',
      'ɖ',
      'ɸ',
      'β',
      'ʃ',
      'ɮ',
      'ɧ'
    ]
    -- We don't include the retroflex l i.e <ɭ> because, even though it is a descender,
    -- There is more free space under it than above


graphemesOfIPA :: [Char]
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
consonantsPulmonic :: [Char]
consonantsPulmonic =
  plosivePulmonic
    <> nasalPulmonic
    <> trillPulmonic
    <> tapOrFlapPulmonic
    <> fricativePulmonic
    <> lateralFricativePulmonic
    <> approximantPulmonic
    <> lateralApproximantPulmonic

consonantsNonPulmonic :: [Char]
consonantsNonPulmonic =
    [ 'ʘ',
      'ɓ', -- Bilabial
      'ǀ' {- Dental -},
      'ɗ', -- Dental/alveolar
      'ǃ' {-  (Post)alveolar -},
      'ʄ',
      'ǂ',
      'ɠ',
      'ǁ',
      'ʛ'
    ]

otherSymbols :: [Char]
otherSymbols =
    [ 'ʍ',
      'ɕ',
      'w',
      'ʑ',
      'ɥ',
      'ɺ',
      'ʜ',
      'ɧ',
      'ʢ',
      'ʡ'
    ]

consonants :: [Char]
consonants = consonantsPulmonic <> consonantsNonPulmonic <> otherSymbols

vowels :: [Char]
vowels =
  fromList
    [ 'i',
      'y',
      'ɨ',
      'ʉ',
      'ɯ',
      'u', -- Close
      'ɪ',
      'ʏ',
      'ʊ',
      'e',
      'ø',
      'ɘ',
      'ɵ',
      'ɤ',
      'o', -- Close-mid
      'ə',
      'ɛ',
      'œ',
      'ɜ',
      'ɞ',
      'ʌ',
      'ɔ', -- Open-mid
      'æ',
      'ɐ',
      'a',
      'ɶ',
      'ɑ',
      'ɒ' -- Open
    ]


-- | IPA text that is not a semantic modifier to what is before or after it.
--   This includes vowels, and consonants. It excludes all diacritics.
strictSegmentals :: [Char]
strictSegmentals = consonants <> vowels

suprasegmentals :: [Char]
suprasegmentals =
    [ 'ˈ', -- Primary stress
      'ˌ', -- Secondary stress
      'ː', -- Long
      'ˑ', -- Half long
      '̆', -- Extra short
      '|', -- Minor (foot) group
      '‖', -- Major (intonation) group
      '.', -- Syllable break
      '‿' -- Linking (absence of a break)
    ]

toneAndWordAccents :: [Char]
toneAndWordAccents =
    {- Level -}
    [ '˥',
      '̋', -- Extra high
      '˦',
      '́', -- High
      '˧',
      '̄', -- Mid
      '˨',
      '̀', -- Low
      '˩',
      '̏', -- Extra low
      'ꜜ', -- Downstep
      'ꜛ', -- Upstep

      {- Contour -}
      '̌', -- Rising
      '̂', -- Falling
      '᷄', -- High rising
      '᷅', -- Low rising
      '᷈', -- Rising-falling
      '↗', -- Global rise
      '↘' -- Global fall
    ]

isAscender :: Char -> Bool
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
isDescender :: Char -> Bool
isDescender character = character `elem` descenders

isDescenderText :: Text -> Bool
isDescenderText text =
  T.length text == 1 && T.head text `elem` descenders

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
    let firstCharacter =  (T.head ss) :: Char
        secondCharacter =  (T.index ss 1) :: Char
        rest = T.tail (T.tail ss)
     in if isAscender firstCharacter && isDiacriticAbove secondCharacter
          then one firstCharacter <> one (lowerDiacritic secondCharacter) <> rest
          else
            if isDescender firstCharacter && isDiacriticBelow secondCharacter
              then one firstCharacter <> one (raiseDiacritic secondCharacter) <> rest
              else ss


-- |
-- Whether a diacritic goes above
-- the character it is placed on.
isDiacriticAbove :: Char -> Bool
isDiacriticAbove '̊' = True
isDiacriticAbove _   = False

-- |
-- Whether a diacritic goes below
-- the character which it is placed on.
isDiacriticBelow :: Char -> Bool
isDiacriticBelow '̥' = True
isDiacriticBelow _   = False

-- |
-- When given a diacritic that goes above,
-- replaces it with one that goes below,
-- and has the same meaning.
-- otherwise does nothing.
lowerDiacritic :: Char -> Char
lowerDiacritic '̊' = '̥'
lowerDiacritic x   = x

-- |
-- When given a diacritic that goes below,
-- replaces it with one that goes below, and
-- has the same meaning;
-- otherwise it does nothing.
raiseDiacritic :: Char -> Char
raiseDiacritic '̥' = '̊'
raiseDiacritic x   = x
