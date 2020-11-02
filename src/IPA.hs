module IPA where

import Prelude(Eq, Int, String, (+), (.), (==), (/=), (&&), Maybe(..), (<), (<>), map, otherwise, read, show, unlines, words)
import Data.Maybe (fromMaybe, maybe)
import Numeric.Natural (Natural)
import Data.Text (Text, init, last, null, pack, unpack)

import IPAConstants.IPAUnicodeConstants
import DefaultLanguageText
    ( sorryUnableToCalculate, noEnglishDescriptionFoundMessage )

import Types.Airstream ( Airstream(..))
import Types.Backness ( Backness(..) )
import Types.Height ( Height(..) )
import Types.Manner ( Manner(..) )
import Types.Nasalization ( Nasalization(Oral, Nasalized) )
import Types.Phonet ( Phonet(..) )
import Types.Place ( Place(..) )
import Types.Rounding ( Rounding(..) )
import Types.SecondaryArticulation ( SecondaryArticulation(..) )
import Types.VocalFolds ( VocalFolds(..) )
import Types.VowelLength ( VowelLength(..) )


import qualified SetPhonet (toExtraShort, toHalfLong, toLabialized, toLong,
	toNoSecondaryArticulation,
	toPalatalized, toPharyngealized, toVelarized,
	toVoiced, toVoiceless, withPlace)
import Lib_Functions (aspirate,
	spirantizedPhonet, devoicedPhonet,
	voicedPhonet, decreak, deaspirate,
	retractPhonet, nasalizePhonet)
import ShowFunctions (showPhonet)

import PhoneticFeatures(showFeatures, analyzeFeatures)
import GraphemeGrammar(isDescender)

import UnicodeToIPANumber (unicodeToNumber)
import IPANumberToUnicode (numberToUnicode)

analyzeIPAToSPE :: Text -> Text
analyzeIPAToSPE ipaText =
	maybe sorryUnableToCalculate (showFeatures . analyzeFeatures) (analyzeIPA ipaText)

ipaPhonemeMapList :: [(Text, Phonet)]
ipaPhonemeMapList = 
	[ (pack [latin_small_letter_p], (Consonant Voiceless Bilabial Plosive PulmonicEgressive Normal))
	, (pack [latin_small_letter_b], (Consonant Voiced Bilabial Plosive PulmonicEgressive Normal))
	, (pack [latin_small_letter_t], (Consonant Voiceless Alveolar Plosive PulmonicEgressive Normal))
	, (pack [latin_small_letter_d], (Consonant Voiced Alveolar Plosive PulmonicEgressive Normal))
	, (pack [latin_small_letter_t_with_retroflex_hook], (Consonant Voiceless Retroflex Plosive PulmonicEgressive Normal))
	, (pack [latin_small_letter_d_with_tail], (Consonant Voiced Retroflex Plosive PulmonicEgressive Normal))
	, (pack [latin_small_letter_c], (Consonant Voiceless Palatal Plosive PulmonicEgressive Normal))
	, (pack [latin_small_letter_dotless_j_with_stroke], (Consonant Voiced Palatal Plosive PulmonicEgressive Normal))
	, (pack [latin_small_letter_k], (Consonant Voiceless Velar Plosive PulmonicEgressive Normal))
	, (pack [latin_small_letter_g], (Consonant Voiced Velar Plosive PulmonicEgressive Normal))
	, (pack [latin_small_letter_q], (Consonant Voiceless Uvular Plosive PulmonicEgressive Normal))
	, (pack [latin_letter_small_capital_g], (Consonant Voiced Uvular Plosive PulmonicEgressive Normal))
	, (pack [latin_letter_glottal_stop], (Consonant Voiceless Glottal Plosive PulmonicEgressive Normal))
	-- Nasals:
	, (pack [latin_small_letter_m], (Consonant Voiced Bilabial Nasal PulmonicEgressive Normal))
	, (pack [latin_small_letter_m_with_hook], (Consonant Voiced LabioDental Nasal PulmonicEgressive Normal))
	, (pack [latin_small_letter_n], (Consonant Voiced Alveolar Nasal PulmonicEgressive Normal))
	, (pack [latin_small_letter_n_with_retroflex_hook], (Consonant Voiced Retroflex Nasal PulmonicEgressive Normal))
	, (pack [latin_small_letter_n_with_left_hook], (Consonant Voiced Palatal Nasal PulmonicEgressive Normal))
	, (pack [latin_small_letter_eng], (Consonant Voiced Velar Nasal PulmonicEgressive Normal))
	, (pack [latin_letter_small_capital_n], (Consonant Voiced Uvular Nasal PulmonicEgressive Normal))
	-- Trills:
	, (pack [latin_letter_small_capital_b], (Consonant Voiced Bilabial Trill PulmonicEgressive Normal))
	, (pack [latin_small_letter_r], (Consonant Voiced Alveolar Trill PulmonicEgressive Normal))
	, (pack [latin_letter_small_capital_r], (Consonant Voiced Uvular Trill PulmonicEgressive Normal))
	-- Taps or flaps:
	, (pack [latin_small_letter_v_with_right_hook], (Consonant Voiced LabioDental TapOrFlap PulmonicEgressive Normal))
	, (pack [latin_small_letter_r_with_fishhook], (Consonant Voiced Alveolar TapOrFlap PulmonicEgressive Normal))
	, (pack [latin_small_letter_r_with_tail], (Consonant Voiced Retroflex TapOrFlap PulmonicEgressive Normal))
	-- Fricatives:
	, (pack [latin_small_letter_phi], (Consonant Voiceless Bilabial Fricative PulmonicEgressive Normal))
	, (pack [greek_small_letter_beta], (Consonant Voiced Bilabial Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_f], (Consonant Voiceless LabioDental Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_v], (Consonant Voiced LabioDental Fricative PulmonicEgressive Normal))
	, (pack [greek_small_letter_theta], (Consonant Voiceless Dental Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_eth], (Consonant Voiced Dental Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_s], (Consonant Voiceless Alveolar Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_z], (Consonant Voiced Alveolar Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_esh], (Consonant Voiceless PostAlveolar Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_ezh], (Consonant Voiced PostAlveolar Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_s_with_hook], (Consonant Voiceless Retroflex Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_z_with_retroflex_hook], (Consonant Voiced Retroflex Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_c_with_cedilla], (Consonant Voiceless Palatal Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_j_with_crossed_tail], (Consonant Voiced Palatal Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_x], (Consonant Voiceless Velar Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_gamma], (Consonant Voiced Velar Fricative PulmonicEgressive Normal))
	, (pack [greek_small_letter_chi], (Consonant Voiceless Uvular Fricative PulmonicEgressive Normal))
	, (pack [latin_letter_small_capital_inverted_r], (Consonant Voiced Uvular Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_h_with_stroke], (Consonant Voiceless Pharyngeal Fricative PulmonicEgressive Normal))
	, (pack [latin_letter_pharyngeal_voiced_fricative], (Consonant Voiced Pharyngeal Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_h], (Consonant Voiceless Glottal Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_h_with_hook], (Consonant Voiced Glottal Fricative PulmonicEgressive Normal))
	-- Lateral Fricatives:
	, (pack [latin_small_letter_l_with_belt], (Consonant Voiceless Alveolar LateralFricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_lezh], (Consonant Voiced Alveolar LateralFricative PulmonicEgressive Normal))
	-- Approximants:
	, (pack [latin_small_letter_v_with_hook], (Consonant Voiced LabioDental Approximant PulmonicEgressive Normal))
	, (pack [latin_small_letter_turned_r], (Consonant Voiced Alveolar Approximant PulmonicEgressive Normal))
	, (pack [latin_small_letter_turned_r_with_hook], (Consonant Voiced Retroflex Approximant PulmonicEgressive Normal))
	, (pack [latin_small_letter_j], (Consonant Voiced Palatal Approximant PulmonicEgressive Normal))
	, (pack [latin_small_letter_turned_m_with_long_leg], (Consonant Voiced Velar Approximant PulmonicEgressive Normal))
	-- Lateral Approximants:
	, (pack [latin_small_letter_l], (Consonant Voiced Alveolar LateralApproximant PulmonicEgressive Normal))
	, (pack [latin_small_letter_l_with_retroflex_hook], (Consonant Voiced Retroflex LateralApproximant PulmonicEgressive Normal))
	, (pack [latin_small_letter_turned_y], (Consonant Voiced Palatal LateralApproximant PulmonicEgressive Normal))
	, (pack [latin_letter_small_capital_l], (Consonant Voiced Velar LateralApproximant PulmonicEgressive Normal))
	-- Affricates
	, (pack [latin_small_letter_t, combining_double_breve_below, latin_small_letter_s], (Consonant Voiceless Alveolar Affricate PulmonicEgressive Normal))
	, (pack [latin_small_letter_t, combining_double_inverted_breve, latin_small_letter_s], (Consonant Voiceless Alveolar Affricate PulmonicEgressive Normal))
	, (pack [latin_small_letter_d, combining_double_breve_below, latin_small_letter_z], (Consonant Voiced Alveolar Affricate PulmonicEgressive Normal))
	, (pack [latin_small_letter_d, combining_double_inverted_breve, latin_small_letter_z], (Consonant Voiced Alveolar Affricate PulmonicEgressive Normal))
	, (pack [latin_small_letter_t, combining_double_inverted_breve, latin_small_letter_esh], (Consonant Voiceless PostAlveolar Affricate PulmonicEgressive Normal))
	, (pack [latin_small_letter_t, combining_double_breve_below, latin_small_letter_esh], (Consonant Voiceless PostAlveolar Affricate PulmonicEgressive Normal))
	, (pack [latin_small_letter_d, combining_double_inverted_breve, latin_small_letter_ezh], (Consonant Voiced PostAlveolar Affricate PulmonicEgressive Normal))
	, (pack [latin_small_letter_d, combining_double_breve_below, latin_small_letter_ezh], (Consonant Voiced PostAlveolar Affricate PulmonicEgressive Normal))

	-- Under the Other Symbols part of the IPA chart:
	, (pack [latin_small_letter_w], (Consonant Voiced LabialVelar Approximant PulmonicEgressive Normal))
	, (pack [latin_small_letter_turned_w], (Consonant Voiceless LabialVelar Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_turned_h], (Consonant Voiced LabialPalatal Approximant PulmonicEgressive Normal))
	, (pack [latin_letter_small_capital_h], (Consonant Voiceless Epiglottal Fricative PulmonicEgressive Normal))
	, (pack [latin_letter_reversed_glottal_stop_with_stroke], (Consonant Voiced Epiglottal Fricative PulmonicEgressive Normal))
	, (pack [latin_letter_glottal_stop_with_stroke], (Consonant Voiceless Epiglottal Plosive PulmonicEgressive Normal))
	  -- Is the epiglottal plosive voiceless? The IPA chart does not specify.
	, (pack [latin_small_letter_c_with_curl], (Consonant Voiceless AlveoloPalatal Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_z_with_curl], (Consonant Voiced AlveoloPalatal Fricative PulmonicEgressive Normal))
	, (pack [latin_small_letter_turned_r_with_long_leg], (Consonant Voiced Alveolar LateralFlap PulmonicEgressive Normal))
	, (pack [latin_small_letter_heng_with_hook],
		( Consonant
			Voiceless
			(Places [PostAlveolar, Velar])
			Fricative
			PulmonicEgressive
			Normal
		))

	-- Other Consonants:
	, (pack [latin_letter_bilabial_click], (Consonant Voiceless Bilabial Plosive Click Normal))
	, (pack [latin_letter_dental_click], (Consonant Voiceless Dental Plosive Click Normal))
	, (pack [latin_letter_retroflex_click], (Consonant Voiceless Alveolar Plosive Click Normal))
	-- "ǃ" could also be PostAlveolar.
	, (pack [latin_letter_alveolar_click], (Consonant Voiceless PalatoAlveolar Plosive Click Normal))
	, (pack [latin_letter_lateral_click], (Consonant Voiceless Alveolar Lateral Click Normal))
	, (pack [latin_small_letter_b_with_hook], (Consonant Voiced Bilabial Plosive Implosive Normal))
	, (pack [latin_small_letter_d_with_hook], (Consonant Voiced Dental Plosive Implosive Normal))
	-- "ɗ" could also be Alveolar
	, (pack [latin_small_letter_dotless_j_with_stroke_and_hook], (Consonant Voiced Palatal Plosive Implosive Normal))
	, (pack [latin_small_letter_g_with_hook], (Consonant Voiced Velar Plosive Implosive Normal))
	, (pack [latin_letter_small_capital_g_with_hook], (Consonant Voiced Uvular Plosive Implosive Normal))
	-- Close Vowels:
	, (pack [latin_small_letter_i], (Vowel Close Front Unrounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_y], (Vowel Close Front Rounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_i_with_stroke], (Vowel Close Central Unrounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_u_bar], (Vowel Close Central Rounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_turned_m], (Vowel Close Back Unrounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_u], (Vowel Close Back Rounded Voiced NormalLength Oral))
	-- Near-close Vowels:
	, (pack [latin_letter_small_capital_i], (Vowel NearClose Front Unrounded Voiced NormalLength Oral))
	, (pack [latin_letter_small_capital_y], (Vowel NearClose Front Rounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_upsilon], (Vowel NearClose Back Rounded Voiced NormalLength Oral))
	-- Close-mid Vowels:
	, (pack [latin_small_letter_e], (Vowel CloseMid Front Unrounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_o_with_stroke], (Vowel CloseMid Front Rounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_reversed_e], (Vowel CloseMid Central Unrounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_barred_o], (Vowel CloseMid Central Rounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_rams_horn], (Vowel CloseMid Back Unrounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_o], (Vowel CloseMid Back Rounded Voiced NormalLength Oral))
	-- Mid Vowels:
	, (pack [latin_small_letter_schwa], (Vowel Mid Central Unrounded Voiced NormalLength Oral))
	-- Open-mid Vowels:
	, (pack [latin_small_letter_open_e], (Vowel OpenMid Front Unrounded Voiced NormalLength Oral))
	, (pack [latin_small_ligature_oe], (Vowel OpenMid Front Rounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_reversed_open_e], (Vowel OpenMid Central Unrounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_closed_reversed_open_e], (Vowel OpenMid Central Rounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_turned_v], (Vowel OpenMid Back Unrounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_open_o], (Vowel OpenMid Back Rounded Voiced NormalLength Oral))
	-- Near-open
	, (pack [latin_small_letter_ae], (Vowel NearOpen Front Unrounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_turned_a], (Vowel NearOpen Central Unrounded Voiced NormalLength Oral))
	-- Open Vowels:
	, (pack [latin_small_letter_a], (Vowel Open Front Unrounded Voiced NormalLength Oral))
	, (pack [latin_letter_small_capital_oe], (Vowel Open Front Rounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_alpha], (Vowel Open Back Unrounded Voiced NormalLength Oral))
	, (pack [latin_small_letter_turned_alpha], (Vowel Open Back Rounded Voiced NormalLength Oral))
	]

lookupInList :: Eq a => a -> [(a, b)] -> Maybe b
lookupInList givenKey aList =
	case aList of
		[] -> Nothing
		((key, value) : tailOfList) 
			| key == givenKey -> Just value
			| otherwise       -> lookupInList givenKey tailOfList

lookupInListFromValue :: Eq b => b -> [(a, b)] -> Maybe a
lookupInListFromValue givenKey aList =
	case aList of
		[] -> Nothing
		((value, key) : tailOfList)
			| key == givenKey -> Just value
			| otherwise       -> lookupInListFromValue givenKey tailOfList


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
						_ | last ipaText == combining_bridge_below ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.withPlace Dental x)
								Nothing -> Nothing
						_ | last ipaText == combining_ring_below ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toVoiceless x)
								Nothing -> Nothing
						_ | last ipaText == combining_ring_above ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toVoiceless x)
								Nothing -> Nothing

						_ | last ipaText == combining_caron_below ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toVoiced x)
								Nothing -> Nothing
						_ | last ipaText == modifier_letter_small_w ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toLabialized x)
								Nothing -> Nothing

						_ | last ipaText == modifier_letter_small_j ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toPalatalized x)
								Nothing -> Nothing

						_ | last ipaText == modifier_letter_small_gamma ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toVelarized x)
								Nothing -> Nothing
						_ | last ipaText == modifier_letter_small_reversed_glottal_stop ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toPharyngealized x)
								Nothing -> Nothing
						_ | last ipaText == modifier_letter_triangular_colon ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toLong x)
								Nothing -> Nothing
						_ | last ipaText == modifier_letter_half_triangular_colon ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toHalfLong x)
								Nothing -> Nothing
						_ | last ipaText == combining_breve ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toExtraShort x)
								Nothing -> Nothing

						_ | last ipaText == modifier_letter_small_h ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (aspirate x)
								Nothing -> Nothing
						-- (About the preceding line:) It is strange but we will just
						-- do nothing if they give us an aspirated vowel.
						-- since we have no way to represent it in the type system.
						-- to do: determine
						-- if the idea of an aspirated vowel makes sense
						_ | last ipaText == combining_minus_sign_below ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> retractPhonet x
								Nothing -> Nothing
						_ | last ipaText == combining_tilde ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> nasalizePhonet x
								Nothing -> Nothing

						_ -> Nothing -- not recognized.

constructIPA :: Phonet -> Text
constructIPA phoneme =
	fromMaybe (pack "∅") (constructIPARecursive 9 0 phoneme)

-- | convert a secondary articulation to its IPA text representation
secondaryArticulationIPA :: SecondaryArticulation -> Text
secondaryArticulationIPA articulation =
	case articulation of
		Normal -> pack []
		Palatalized -> pack [modifier_letter_small_j]
		Labialized -> pack [modifier_letter_small_w]
		Velarized -> pack [modifier_letter_small_gamma]
		Pharyngealized -> pack [modifier_letter_small_reversed_glottal_stop]

vowelLengthIPA :: VowelLength -> Text
vowelLengthIPA vowelLength =
	case vowelLength of
		NormalLength -> pack []
		ExtraShort -> pack [combining_breve]
		HalfLong -> pack [modifier_letter_half_triangular_colon]
		Long -> pack [modifier_letter_triangular_colon]

nasalizationIPA :: Nasalization -> Text
nasalizationIPA nasalization =
	case nasalization of
		Oral -> pack []
		Nasalized -> pack [combining_tilde]

addRetractedDiacritic :: Text -> Text
addRetractedDiacritic = (<> pack [combining_minus_sign_below])

addDentalDiacritic :: Text -> Text
addDentalDiacritic = (<> pack [combining_bridge_below])

addVoicedDiacritic :: Text -> Text
addVoicedDiacritic = (<> pack [combining_caron_below])

addCreakyVoicedDiacritic :: Text -> Text
addCreakyVoicedDiacritic = (<> pack [combining_tilde_below])

addAspirationDiacritic :: Text -> Text
addAspirationDiacritic = (<> pack [modifier_letter_small_h])

addVoicelessDiacritic :: Text -> Text
addVoicelessDiacritic x =
	if isDescender (last x)
		then x <> pack [combining_ring_above]
		else x <> pack [combining_ring_below]

-- here here
addPharynealizedDiacritic :: Text -> Text
addPharynealizedDiacritic = (<> (secondaryArticulationIPA Pharyngealized))

addVelarizedDiacritic :: Text -> Text
addVelarizedDiacritic = (<> (secondaryArticulationIPA Velarized))

addPalatalizedDiacritic :: Text -> Text
addPalatalizedDiacritic = (<> (secondaryArticulationIPA Palatalized))

addLabializedDiacritic :: Text -> Text
addLabializedDiacritic = (<> (secondaryArticulationIPA Labialized))

addNasalizedDiacritic :: Text -> Text
addNasalizedDiacritic = (<> (nasalizationIPA Nasalized))


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
				Nothing -> Nothing
				Just regularIPA -> Just (addRetractedDiacritic regularIPA)

	(Consonant _ _ _ PulmonicEgressive Pharyngealized)
		| recursionLevel < recursionLimit ->
			let result =
				constructIPARecursive
				recursionLimit
				(1 + recursionLevel)
				(SetPhonet.toNoSecondaryArticulation p)
			in case result of
				Just regularIPA -> Just (addPharynealizedDiacritic regularIPA)
				Nothing -> Nothing

	(Consonant _ _ _ PulmonicEgressive Velarized)
		| recursionLevel < recursionLimit ->
			let result =
				constructIPARecursive
				recursionLimit
				(1 + recursionLevel)
				(SetPhonet.toNoSecondaryArticulation p)
			in case result of
				Just regularIPA -> Just (addVelarizedDiacritic regularIPA)
				Nothing -> Nothing

	(Consonant _ _ _ PulmonicEgressive Palatalized)
		| recursionLevel < recursionLimit ->
			let result =
				constructIPARecursive
				recursionLimit
				(1 + recursionLevel)
				(SetPhonet.toNoSecondaryArticulation p)
			in case result of
				Just regularIPA -> Just (addPalatalizedDiacritic regularIPA)
				Nothing -> Nothing

	(Consonant _ _ _ PulmonicEgressive Labialized)
		| recursionLevel < recursionLimit ->
			let result =
				constructIPARecursive
				recursionLimit
				(1 + recursionLevel)
				(SetPhonet.toNoSecondaryArticulation p)
			in case result of
				Just regularIPA -> Just (addLabializedDiacritic regularIPA)
				Nothing         -> Nothing

	(Consonant x Dental y z sa)
		| recursionLevel < recursionLimit ->
			case constructIPARecursive
				recursionLimit
				(1 + recursionLevel)
				(Consonant x Alveolar y z sa) of
				Nothing -> Nothing
				Just regularIPA -> Just (addDentalDiacritic regularIPA)

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
				Nothing	-> Nothing
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
			Nothing -> Nothing
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
				Nothing -> Nothing
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
				Nothing	-> Nothing

	(Vowel w x y z vowelLength nasalization)
		| vowelLength /= NormalLength
		&& recursionLevel < recursionLimit ->
			case constructIPARecursive
				recursionLimit
				(1 + recursionLevel)
				(Vowel w x y z NormalLength nasalization) of
						Nothing         -> Nothing
						Just regularIPA -> Just (regularIPA <> vowelLengthIPA vowelLength)

	-- add the nasal diacritic:
	(Vowel x y z v vowelLength Nasalized)
			| recursionLevel < recursionLimit ->
			case constructIPARecursive
				recursionLimit
				(1 + recursionLevel)
				(Vowel x y z v vowelLength Oral) of
					Nothing -> Nothing
					Just regularIPA -> Just (addNasalizedDiacritic regularIPA)

	-- Add the small circle diacritic to vowels to make them voiceless.
	(Vowel x y z Voiceless vowelLength nasalization)
		| recursionLevel < recursionLimit ->
			case constructIPARecursive
				recursionLimit
				(1 + recursionLevel)
				(Vowel x y z Voiced vowelLength nasalization) of
				Nothing         -> Nothing
				Just regularIPA -> Just (addVoicelessDiacritic regularIPA)
	-- If there is no way to express a voiced consonant in a single
	-- grapheme add a diacritic to the grapheme that represents
	-- the voiceless counterpart.
	(Vowel x y z Voiced vowelLength nasalization)
		| recursionLevel < recursionLimit ->
			case constructIPARecursive
				recursionLimit
				(1 + recursionLevel)
				(Vowel x y z Voiceless vowelLength nasalization) of
				Nothing         -> Nothing
				Just regularIPA -> Just (addVoicedDiacritic regularIPA)



	Consonant {} -> Nothing
	Vowel {} -> Nothing

constructDeconstruct :: (Phonet -> Phonet) -> Text -> Text
constructDeconstruct func x =
	let something = analyzeIPA x
	in case something of
		Nothing -> pack "∅"
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


unicodeTextToIPANumbers :: Text -> Text
unicodeTextToIPANumbers text = pack (unlines (map (show . unicodeToNumber) (unpack text)))

ipaNumbersToUnicodeText :: Text -> Text
ipaNumbersToUnicodeText text = pack ((map (numberToUnicode . (read :: String -> Int)) (words (unpack text))))