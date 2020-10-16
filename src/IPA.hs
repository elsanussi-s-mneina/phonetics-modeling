module IPA where

import Prelude(Eq, (+), (.), (==), (/=), (&&), Maybe(..), (<), (<>), otherwise)
import Data.Maybe (fromMaybe, maybe)
import Numeric.Natural (Natural)
import Data.Text (Text, init, last, null, pack)

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
	, (pack "t͜s", (Consonant Voiceless Alveolar Affricate PulmonicEgressive Normal))
	, (pack "t͡s", (Consonant Voiceless Alveolar Affricate PulmonicEgressive Normal))
	, (pack "d͜z", (Consonant Voiced Alveolar Affricate PulmonicEgressive Normal))
	, (pack "d͡z", (Consonant Voiced Alveolar Affricate PulmonicEgressive Normal))
	, (pack "t͡ʃ", (Consonant Voiceless PostAlveolar Affricate PulmonicEgressive Normal))
	, (pack "t͜ʃ", (Consonant Voiceless PostAlveolar Affricate PulmonicEgressive Normal))
	, (pack "d͡ʒ", (Consonant Voiced PostAlveolar Affricate PulmonicEgressive Normal))
	, (pack "d͜ʒ", (Consonant Voiced PostAlveolar Affricate PulmonicEgressive Normal))

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

	-- The following two lines are commented out, because I am unsure
	-- about their place of articulation:
	-- , (pack "k͡p",  (Consonant  Voiceless LabialVelar? Affricate
	--     PulmonicEgressive Normal))
	-- , (pack "c͡ɕ", (Consonant  Voiceless Palatal (or AlveoloPalatal?)
	--     Affricate PulmonicEgressive Normal))

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
	, (pack "i", (Vowel Close Front Unrounded Voiced NormalLength Oral))
	, (pack "y", (Vowel Close Front Rounded Voiced NormalLength Oral))
	, (pack "ɨ", (Vowel Close Central Unrounded Voiced NormalLength Oral))
	, (pack "ʉ", (Vowel Close Central Rounded Voiced NormalLength Oral))
	, (pack "ɯ", (Vowel Close Back Unrounded Voiced NormalLength Oral))
	, (pack "u", (Vowel Close Back Rounded Voiced NormalLength Oral))
	-- Near-close Vowels:
	, (pack "ɪ", (Vowel NearClose Front Unrounded Voiced NormalLength Oral))
	, (pack "ʏ", (Vowel NearClose Front Rounded Voiced NormalLength Oral))
	, (pack "ʊ", (Vowel NearClose Back Rounded Voiced NormalLength Oral))
	-- Close-mid Vowels:
	, (pack "e", (Vowel CloseMid Front Unrounded Voiced NormalLength Oral))
	, (pack "ø", (Vowel CloseMid Front Rounded Voiced NormalLength Oral))
	, (pack "ɘ", (Vowel CloseMid Central Unrounded Voiced NormalLength Oral))
	, (pack "ɵ", (Vowel CloseMid Central Rounded Voiced NormalLength Oral))
	, (pack "ɤ", (Vowel CloseMid Back Unrounded Voiced NormalLength Oral))
	, (pack "o", (Vowel CloseMid Back Rounded Voiced NormalLength Oral))
	-- Mid Vowels:
	, (pack "ə", (Vowel Mid Central Unrounded Voiced NormalLength Oral))
	-- Open-mid Vowels:
	, (pack "ɛ", (Vowel OpenMid Front Unrounded Voiced NormalLength Oral))
	, (pack "œ", (Vowel OpenMid Front Rounded Voiced NormalLength Oral))
	, (pack "ɜ", (Vowel OpenMid Central Unrounded Voiced NormalLength Oral))
	, (pack "ɞ", (Vowel OpenMid Central Rounded Voiced NormalLength Oral))
	, (pack "ʌ", (Vowel OpenMid Back Unrounded Voiced NormalLength Oral))
	, (pack "ɔ", (Vowel OpenMid Back Rounded Voiced NormalLength Oral))
	-- Near-open
	, (pack "æ", (Vowel NearOpen Front Unrounded Voiced NormalLength Oral))
	, (pack "ɐ", (Vowel NearOpen Central Unrounded Voiced NormalLength Oral))
	-- Open Vowels:
	, (pack "a", (Vowel Open Front Unrounded Voiced NormalLength Oral))
	, (pack "ɶ", (Vowel Open Front Rounded Voiced NormalLength Oral))
	, (pack "ɑ", (Vowel Open Back Unrounded Voiced NormalLength Oral))
	, (pack "ɒ", (Vowel Open Back Rounded Voiced NormalLength Oral))
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
						"̪" ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.withPlace Dental x)
								Nothing -> Nothing
						"̥" ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toVoiceless x)
								Nothing -> Nothing
						"̊" ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toVoiceless x)
								Nothing -> Nothing

						"̬" ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toVoiced x)
								Nothing -> Nothing
						"ʷ" ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toLabialized x)
								Nothing -> Nothing

						"ʲ" ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toPalatalized x)
								Nothing -> Nothing

						"ˠ" ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toVelarized x)
								Nothing -> Nothing
						"ˤ" ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toPharyngealized x)
								Nothing -> Nothing
						"ː" ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toLong x)
								Nothing -> Nothing
						"ˑ" ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toHalfLong x)
								Nothing -> Nothing
						"̆" ->
							let fullGrapheme = analyzeIPA (init ipaText)
							in case fullGrapheme of
								Just x -> Just (SetPhonet.toExtraShort x)
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
							in case fullGrapheme of
								Just x -> retractPhonet x
								Nothing -> Nothing
						"̃" ->
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
		Normal -> pack ""
		Palatalized -> pack "ʲ"
		Labialized -> pack "ʷ"
		Velarized -> pack "ˠ"
		Pharyngealized -> pack "ˤ"

vowelLengthIPA :: VowelLength -> Text
vowelLengthIPA vowelLength =
	case vowelLength of
		NormalLength -> pack ""
		ExtraShort -> pack "̆"
		HalfLong -> pack "ˑ"
		Long -> pack "ː"

nasalizationIPA :: Nasalization -> Text
nasalizationIPA nasalization =
	case nasalization of
		Oral -> pack ""
		Nasalized -> pack "̃"

addRetractedDiacritic :: Text -> Text
addRetractedDiacritic = (<> pack "̠")

addDentalDiacritic :: Text -> Text
addDentalDiacritic = (<> pack "̪")

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

