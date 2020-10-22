-- | Handle splitting of IPA graphemes into chunks, so that
--   diacritics go with the non-diacritic characters they modify.
--
module GraphemeGrammar where
import Prelude((+), (>=), (<>), (==), Bool(..),
	Char, Int, Maybe(..),
	elem, otherwise)
import IPAConstants.IPAUnicodeConstants
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
secondaryArticulationDiacritics = [modifier_letter_small_w, modifier_letter_small_j, modifier_letter_small_reversed_glottal_stop, modifier_letter_small_gamma]

voicingDiacritics :: [Char]
voicingDiacritics = [combining_ring_below, combining_ring_above, combining_caron_below]

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
tieBarParser = singleCharParser tieBars

voicingDiacriticParser
	:: Text
	-> Maybe (Text, Text)
voicingDiacriticParser = singleCharParser voicingDiacritics

-- | A parser that consumes the first
-- character in some IPA text
-- if that character is a superscript
-- (for example ʲ, ʰ)
-- that comes after a base character
-- (for example p, t, c, g).
superscriptAfterParser
	:: Text
	-> Maybe (Text, Text)
superscriptAfterParser = singleCharParser superscriptsAfter

-- | A parser that consumes the first
-- character in some IPA text
-- if that character is a superscript
-- (for example ⁿ)
-- that could come before a base character
-- (for example p, t, c, g).
superscriptBeforeParser
	:: Text
	-> Maybe (Text, Text)
superscriptBeforeParser = singleCharParser superscriptsBefore


-- | represents grammar rule:
-- phoneme -> [subscriptBefore] (digraph | baseCharacter) [voicingDiacritic] [subscriptAfter] [secondaryArticulation]
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
-- phones. This is also called tokenization.
--
-- Note: it does not recognize affricates, unless a tie-bar
-- is provided.
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
-- represents a consonant.
isConsonantAt
	:: Int -- ^ an index within the range of 0, and the length of the string argument
	-> Text -- ^ a text string
	-> Bool -- ^ true if it is a consonant
isConsonantAt = isSuchAt isConsonant


-- | Whether a character is one that is used in the
-- International Phonetic Alphabet to represent a
-- consonant.
isConsonant :: Char -> Bool
isConsonant c = elem c consonants

-- | Whether a character in some text, at a specific place
-- within the text is a "segmental" (i.e. not a diacritic or modifier).
isSegmentalAt :: Int -> Text -> Bool
isSegmentalAt = isSuchAt isSegmental

-- | Whether a character is one that is used in the
-- International Phonetic Alphabet to represent something
-- that is not a diacritic, and can stand on its own.
-- This means characters that can represent a
-- consonant or vowel.
isSegmental :: Char -> Bool
isSegmental c = elem c strictSegmentals

-- | Whether a character is a diacritic that can go after
-- the main character.
isSuperscriptAfterAt
	:: Int -- ^ a number indicating where the character is in the text
	-> Text -- ^ the text that contains the character
	-> Bool -- ^ true if the character can be a diacritic after the main character
isSuperscriptAfterAt = isSuchAt isSuperscriptAfter

-- | Whether a character at a certain place in a string,
--   is the tie-bar diacritic.
isTieBarAt
	:: Int -- ^ a number telling which character in the string
	-> Text -- ^ the string (some text)
	-> Bool -- ^ true if it is a tie-bar
isTieBarAt = isSuchAt isTieBar

-- | Whether a character at a string is of a certain class.
isSuchAt
	:: (Char -> Bool) -- ^ a function
	-> Int -- ^ a number indicating which character in the text
	-> Text -- ^ a string
	-> Bool -- ^ whether it is true
isSuchAt function indexValue text
	| indexValue >= length text = False -- index is out of range
	| otherwise = function (index text indexValue)

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

tieBars :: [Char]
tieBars = [combining_double_breve_below, combining_double_inverted_breve]

-- | Whether a character is used to tie two characters in the
--   international phonetic alphabet together. The tie bar is
--   usually used to indicate an affricate, or double-articulation.
isTieBar :: Char -> Bool
isTieBar x = elem x tieBars


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
	[ latin_small_letter_p,
	latin_small_letter_b,
	latin_small_letter_t,
	latin_small_letter_d,
	latin_small_letter_t_with_retroflex_hook,
	latin_small_letter_d_with_tail,
	latin_small_letter_c,
	latin_small_letter_dotless_j_with_stroke,
	latin_small_letter_k,
	latin_small_letter_g,
	latin_small_letter_q,
	latin_letter_small_capital_g,
	latin_letter_glottal_stop
	]

nasalPulmonic :: [Char]
nasalPulmonic = [latin_small_letter_m, latin_small_letter_m_with_hook, latin_small_letter_n, latin_small_letter_n_with_retroflex_hook, latin_small_letter_n_with_left_hook, latin_small_letter_eng, latin_letter_small_capital_n]

trillPulmonic :: [Char]
trillPulmonic = [latin_letter_small_capital_b, latin_small_letter_r, latin_letter_small_capital_r]

tapOrFlapPulmonic :: [Char]
tapOrFlapPulmonic = [latin_small_letter_v_with_right_hook, latin_small_letter_r_with_fishhook, latin_small_letter_r_with_tail]

fricativePulmonic :: [Char]
fricativePulmonic =
	[
		latin_small_letter_phi,
		greek_small_letter_beta,
		latin_small_letter_f,
		latin_small_letter_v,
		greek_small_letter_theta,
		latin_small_letter_eth,
		latin_small_letter_s,
		latin_small_letter_z,
		latin_small_letter_esh,
		latin_small_letter_ezh,
		latin_small_letter_s_with_hook,
		latin_small_letter_z_with_retroflex_hook,
		latin_small_letter_c_with_cedilla,
		latin_small_letter_j_with_crossed_tail,
		latin_small_letter_x,
		latin_small_letter_gamma,
		greek_small_letter_chi,
		latin_letter_small_capital_inverted_r,
		latin_small_letter_h_with_stroke,
		latin_letter_pharyngeal_voiced_fricative,
		latin_small_letter_h,
		latin_small_letter_h_with_hook
	]

lateralFricativePulmonic :: [Char]
lateralFricativePulmonic = [latin_small_letter_l_with_belt, latin_small_letter_lezh]

approximantPulmonic :: [Char]
approximantPulmonic = [latin_small_letter_v_with_hook, latin_small_letter_turned_r, latin_small_letter_turned_r_with_hook, latin_small_letter_j, latin_small_letter_turned_m_with_long_leg]

lateralApproximantPulmonic :: [Char]
lateralApproximantPulmonic = [latin_small_letter_l, latin_small_letter_l_with_retroflex_hook, latin_small_letter_turned_y, latin_letter_small_capital_l]


diacriticsAndSuprasegmentals :: [Char]
diacriticsAndSuprasegmentals =
	[
		combining_ring_below, -- Voiceless
		combining_ring_above, -- Voiceless (diacritic placed above symbol with descender)
		combining_diaeresis_below, -- Breathy voiced
		-- End of first row.
		combining_caron_below, -- Voiced
		combining_tilde_below, -- Creaky voiced
		combining_inverted_bridge_below, -- Apical
		-- End of second row.
		modifier_letter_small_h, -- Aspirated
		combining_seagul_below, -- Linguolabial
		combining_square_below, -- Laminal
		-- End of third row.
		combining_right_half_ring_below, -- More rounded
		modifier_letter_small_w, -- Labialised
		combining_tilde, -- Nasalised
		-- End of fourth row.
		combining_left_half_ring_below, -- Less rounded
		modifier_letter_small_j, -- Palatalised
		superscript_latin_small_letter_n, -- Pre/post nasalised
		combining_plus_sign_below, -- Advanced
		modifier_letter_small_gamma, -- Velarised
		modifier_letter_small_l, -- Lateral release
		combining_minus_sign_below, -- Retracted
		modifier_letter_small_reversed_glottal_stop, -- Pharyngealised
		combining_left_angle_above, -- No audible release
		combining_diaeresis, -- Centralised
		combining_x_above, -- Mid centralised
		combining_up_tack_below, -- Raised
		combining_vertical_line_below, -- Syllabic
		combining_down_tack_below, -- Lowered
		combining_inverted_breve_below, -- Non-syllabic
		combining_left_tack_below, -- Advanced tongue root
		modifier_letter_rhotic_hook, -- Rhoticity
		combining_right_tack_below, -- Retracted tongue root
		modifier_letter_apostrophe, -- Ejective
		combining_vertical_line_above, -- Syllabic (diacritic placed above)
		combining_bridge_below, -- Dental
		combining_dot_below, -- Closer variety/Fricative
		combining_dot_above -- Palatalization/Centralization
	]

-- To do: find a more suitable name than superscripts.
-- They only look like superscripts if you consider how they
-- look similar to mathematical notation for superscripts.
-- Really, they should be named something different.
superscripts :: [Char]
superscripts = superscriptsBefore <> superscriptsAfter

superscriptsBefore :: [Char]
superscriptsBefore = [superscript_latin_small_letter_n]

superscriptsAfter :: [Char]
superscriptsAfter = diacriticsAndSuprasegmentals <> [modifier_letter_triangular_colon, modifier_letter_half_triangular_colon, combining_breve]

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
	[
		latin_small_letter_b,
		latin_small_letter_t,
		latin_small_letter_d,
		latin_small_letter_k,
		latin_letter_glottal_stop,
		latin_small_letter_f,
		greek_small_letter_theta,
		latin_small_letter_eth,
		latin_small_letter_h_with_stroke,
		latin_letter_pharyngeal_voiced_fricative,
		latin_small_letter_h,
		latin_small_letter_h_with_hook,
		latin_small_letter_l_with_belt,
		latin_small_letter_l,
		latin_small_letter_turned_y,
		latin_letter_bilabial_click,
		latin_small_letter_b_with_hook,
		latin_letter_dental_click,
		latin_small_letter_d_with_hook,
		latin_letter_retroflex_click,
		latin_letter_alveolar_click,
		latin_small_letter_g_with_hook,
		latin_small_letter_dotless_j_with_stroke_and_hook,
		latin_letter_lateral_click,
		latin_letter_small_capital_g_with_hook,
		latin_small_letter_turned_r_with_long_leg,
		latin_letter_reversed_glottal_stop_with_stroke,
		latin_letter_glottal_stop_with_stroke,
		latin_small_letter_rams_horn,
		latin_small_letter_t_with_retroflex_hook,
		latin_small_letter_d_with_tail,
		latin_small_letter_phi,
		greek_small_letter_beta,
		latin_small_letter_esh,
		latin_small_letter_lezh,
		latin_small_letter_l_with_retroflex_hook,
		latin_small_letter_heng_with_hook
	]

descenders :: [Char]
descenders =
	[ latin_small_letter_p,
	latin_small_letter_dotless_j_with_stroke,
	latin_small_letter_g,
	latin_small_letter_q,
	latin_small_letter_m_with_hook,
	latin_small_letter_r_with_tail,
	latin_small_letter_ezh,
	latin_small_letter_s_with_hook,
	latin_small_letter_z_with_retroflex_hook,
	latin_small_letter_c_with_cedilla,
	latin_small_letter_j_with_crossed_tail,
	latin_small_letter_gamma,
	greek_small_letter_chi,
	latin_small_letter_turned_r_with_hook,
	latin_small_letter_j,
	latin_small_letter_turned_m_with_long_leg,
	latin_small_letter_turned_h,
	latin_small_letter_y,
	latin_small_letter_n_with_retroflex_hook,
	latin_small_letter_n_with_left_hook,
	latin_small_letter_eng,
	latin_small_letter_t_with_retroflex_hook,
	latin_small_letter_d_with_tail,
	latin_small_letter_phi,
	greek_small_letter_beta,
	latin_small_letter_esh,
	latin_small_letter_lezh,
	latin_small_letter_heng_with_hook
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
	[ latin_letter_bilabial_click,
	latin_small_letter_b_with_hook, -- Bilabial
	latin_letter_dental_click {- Dental -},
	latin_small_letter_d_with_hook, -- Dental/alveolar
	latin_letter_retroflex_click {-  (Post)alveolar -},
	latin_small_letter_dotless_j_with_stroke,
	latin_letter_alveolar_click,
	latin_small_letter_g_with_hook,
	latin_letter_lateral_click,
	latin_letter_small_capital_g_with_hook
	]

otherSymbols :: [Char]
otherSymbols =
	[ latin_small_letter_turned_w,
	latin_small_letter_c_with_curl,
	latin_small_letter_w,
	latin_small_letter_z_with_curl,
	latin_small_letter_turned_h,
	latin_small_letter_turned_r_with_long_leg,
	latin_letter_small_capital_h,
	latin_small_letter_heng_with_hook,
	latin_letter_reversed_glottal_stop_with_stroke,
	latin_letter_glottal_stop_with_stroke
	]

consonants :: [Char]
consonants = consonantsPulmonic <> consonantsNonPulmonic <> otherSymbols

vowels :: [Char]
vowels =
	[ latin_small_letter_i,
	latin_small_letter_y,
	latin_small_letter_i_with_stroke,
	latin_small_letter_u_bar,
	latin_small_letter_turned_m,
	latin_small_letter_u, -- Close
	latin_letter_small_capital_i,
	latin_letter_small_capital_y,
	latin_small_letter_upsilon,
	latin_small_letter_e,
	latin_small_letter_o_with_stroke,
	latin_small_letter_reversed_e,
	latin_small_letter_barred_o,
	latin_small_letter_rams_horn,
	latin_small_letter_o, -- Close-mid
	latin_small_letter_schwa,
	latin_small_letter_open_e,
	latin_small_ligature_oe,
	latin_small_letter_reversed_open_e,
	latin_small_letter_closed_reversed_open_e,
	latin_small_letter_turned_v,
	latin_small_letter_open_o, -- Open-mid
	latin_small_letter_ae,
	latin_small_letter_turned_a,
	latin_small_letter_a,
	latin_letter_small_capital_oe,
	latin_small_letter_alpha,
	latin_small_letter_turned_alpha -- Open
	]


-- | IPA text that is not a semantic modifier to what is before or after it.
--   This includes vowels, and consonants. It excludes all diacritics.
strictSegmentals :: [Char]
strictSegmentals = consonants <> vowels

suprasegmentals :: [Char]
suprasegmentals =
	[ modifier_letter_vertical_line, -- Primary stress
		modifier_letter_low_vertical_line, -- Secondary stress
		modifier_letter_triangular_colon, -- Long
		modifier_letter_half_triangular_colon, -- Half long
		combining_breve, -- Extra short
		vertical_line, -- Minor (foot) group
		double_vertical_line, -- Major (intonation) group
		full_stop, -- Syllable break
		undertie -- Linking (absence of a break)
	]

toneAndWordAccents :: [Char]
toneAndWordAccents =
	{- Level -}
	[ modifier_letter_extra_high_tone_bar,
	  combining_double_acute_accent, -- Extra high
	  modifier_letter_high_tone_bar,
	  combining_acute_accent, -- High
	  modifier_letter_mid_tone_bar,
	  combining_macron, -- Mid
	  modifier_letter_low_tone_bar,
	  combining_grave_accent, -- Low
	  modifier_letter_extra_low_tone_bar,
	  combining_double_grave_accent, -- Extra low
	  downwards_arrow, -- Downstep
	  upwards_arrow, -- Upstep	
	  {- Contour -}
	  combining_caron, -- Rising
	  combining_circumflex_accent, -- Falling
	  combining_macron_acute, -- High rising
	  combining_grave_macron, -- Low rising
	  combining_grave_acute_grave, -- Rising-falling
	  north_east_arrow, -- Global rise
	  south_east_arrow -- Global fall
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
isDiacriticAbove c = c == combining_ring_above

-- |
-- Whether a diacritic goes below
-- the character which it is placed on.
isDiacriticBelow :: Char -> Bool
isDiacriticBelow c = c == combining_ring_below


-- |
-- When given a diacritic that goes above,
-- replaces it with one that goes below,
-- and has the same meaning.
-- otherwise does nothing.
lowerDiacritic :: Char -> Char
lowerDiacritic c =
	case c of 
		_ | c == combining_ring_above -> combining_ring_below
		other -> other


-- |
-- When given a diacritic that goes below,
-- replaces it with one that goes below, and
-- has the same meaning;
-- otherwise it does nothing.
raiseDiacritic :: Char -> Char
raiseDiacritic c =
	case c of 
		_ | c == combining_ring_below    -> combining_ring_above
		other -> other