-- | Handle splitting of IPA graphemes into chunks, so that
--   diacritics go with the non-diacritic characters they modify.
--
module GraphemeGrammar where
import Prelude((+), (>=), (<>), (==), Bool(..),
              Char, Int, Maybe(..),
              elem, otherwise)
import Data.Text (Text, index, length)
import PrimitiveParsers (manyParser, optionalParser, orParser, singleCharParser, thenParser)

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

-- | This implements the
-- rule expressed in the grammar as:
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

-- | A parser that consumes the first
--   character in some IPA text
--   if that character is a superscript
--   (for example ʲ, ʰ)
--   that comes after a base character
--   (for example p, t, c, g).
superscriptAfterParser
  :: Text
  -> Maybe (Text, Text)
superscriptAfterParser = singleCharParser superscriptsAfter

-- | A parser that consumes the first
--   character in some IPA text
--   if that character is a superscript
--   (for example ⁿ)
--   that could come before a base character
--   (for example p, t, c, g).
superscriptBeforeParser
  :: Text
  -> Maybe (Text, Text)
superscriptBeforeParser = singleCharParser superscriptsBefore


-- | represents grammar rule:
--  phoneme -> [subscriptBefore] (digraph | baseCharacter) [voicingDiacritic] [subscriptAfter] [secondaryArticulation]
phonemeParser
  :: Text
  -> Maybe (Text, Text)
phonemeParser =
  (optionalParser superscriptBeforeParser)
  `thenParser`
  (digraphParser `orParser` baseCharacterParser)
  `thenParser`
  (manyParser
    (optionalParser superscriptAfterParser)
   `orParser`
   (optionalParser voicingDiacriticParser)
   `orParser`
   (optionalParser secondaryArticulationDiacriticParser)
  )

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
isConsonant c = elem c consonants

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
isSegmental c = elem c strictSegmentals

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
isSuchAt function indexValue text
  | indexValue >= length text = False  -- index is out of range
  | otherwise                 = function (index text indexValue)

-- | Whether a character is a superscript character, that
--   often goes after a full character to modify the full
--   character's meaning.
--   For example in the International Phonetic Alphabet,
--   a superscript `h` causes the phoneme represented by the
--   previous character to
--   be aspirated.
isSuperscriptAfter :: Char -> Bool
isSuperscriptAfter c = elem c superscriptsAfter

-- | Whether a character is a superscript character, that
--   often goes before a full character to modify the
--   full character's meaning.
--   For example in the International Phonetic Alphabet,
--   a superscript `n`.
isSuperscriptBefore :: Char -> Bool
isSuperscriptBefore c = elem c superscriptsBefore


-- | Whether a character is used to tie two characters in the
--   international phonetic alphabet together. The tie bar is
--   usually used to indicate an affricate, or double-articulation.
isTieBar :: Char -> Bool
isTieBar x = elem x ['͜', '͡']


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

-- |
-- Whether a diacritic goes above
-- the character it is placed on.
isDiacriticAbove :: Char -> Bool
isDiacriticAbove c = c == '̊'

-- |
-- Whether a diacritic goes below
-- the character which it is placed on.
isDiacriticBelow :: Char -> Bool
isDiacriticBelow c = c == '̥'


-- |
-- When given a diacritic that goes above,
-- replaces it with one that goes below,
-- and has the same meaning.
-- otherwise does nothing.
lowerDiacritic :: Char -> Char
lowerDiacritic c =
  case c of 
    '̊'    -> '̥'
    other -> other


-- |
-- When given a diacritic that goes below,
-- replaces it with one that goes below, and
-- has the same meaning;
-- otherwise it does nothing.
raiseDiacritic :: Char -> Char
raiseDiacritic c =
  case c of 
    '̥'    -> '̊'
    other -> other