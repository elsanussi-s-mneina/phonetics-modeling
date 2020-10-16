-- | A module for
-- some helper functions for
-- changing the values inside
-- the Phonet type or record.

module SetPhonet where

import Types.Airstream ( Airstream(..))
import Types.Manner ( Manner(..) )
import Types.Nasalization (Nasalization(Nasalized))
import Types.Phonet ( Phonet(..) )
import Types.Place ( Place(..) )
import Types.SecondaryArticulation ( SecondaryArticulation(..) )
import Types.VocalFolds ( VocalFolds(..) )
import Types.VowelLength ( VowelLength(..) )

import Prelude (Maybe(..))

-- | A function for returning
--   a phonete with a possibly different vocal fold configuration of a phonete,
--   but no other difference.
withVocalFolds :: VocalFolds -> Phonet -> Phonet
withVocalFolds vf p =
	case p of
		Consonant _ w x y z -> Consonant vf w x y z
		Vowel x y z _ vl n -> Vowel x y z vf vl n

withPlace :: Place -> Phonet -> Phonet
withPlace x p =  
	case p of 
		Consonant a _ b c d -> Consonant a x b c d
		Vowel {} -> p

withManner :: Manner -> Phonet -> Phonet
withManner x p =
	case p of
		Consonant a b _ c d -> Consonant a b x c d
		Vowel {} -> p

withAirstream :: Airstream -> Phonet -> Maybe Phonet
withAirstream x p = 
	case p of 
		Consonant a b c _ d -> Just (Consonant a b c x d)
		Vowel {} -> Nothing

withVowelLength :: VowelLength -> Phonet -> Phonet
withVowelLength vl p =
	case p of 
		Vowel height backness rounding voicing _ nasalization
			-> Vowel height backness rounding voicing vl nasalization
		Consonant {} 
			-> p -- Ignore phonetes that are not vowels.

toLong :: Phonet -> Phonet
toLong = withVowelLength Long

toHalfLong :: Phonet -> Phonet
toHalfLong = withVowelLength HalfLong

toExtraShort :: Phonet -> Phonet
toExtraShort = withVowelLength ExtraShort

-- | Changes the secondary articulation value
--   for a phonete.
--   Ignore vowels, we don't add any secondary articulation for them
withSecondaryArticulation :: SecondaryArticulation -> Phonet -> Phonet
withSecondaryArticulation x p = 
	case p of 
		Consonant a b c d _ -> Consonant a b c d x
		Vowel {} -> p


-- | Given a phonete returns,
--   a similar phonete with the only possible difference
--   being that the vocal folds are voiced,
--  and not aspirated.
toVoiced :: Phonet -> Phonet
toVoiced = withVocalFolds Voiced

toVoiceless :: Phonet -> Phonet
toVoiceless = withVocalFolds Voiceless

-- | Given a phonete returns,
--   a similar phonete with the only difference
--   being that the result is voiced, and aspirated.
toVoicedAspirated :: Phonet -> Phonet
toVoicedAspirated = withVocalFolds VoicedAspirated

toVoicelessAspirated :: Phonet -> Phonet
toVoicelessAspirated = withVocalFolds VoicelessAspirated

toCreakyVoiced :: Phonet -> Phonet
toCreakyVoiced = withVocalFolds CreakyVoiced

toLabialized :: Phonet -> Phonet
toLabialized = withSecondaryArticulation Labialized

toPalatalized :: Phonet -> Phonet
toPalatalized = withSecondaryArticulation Palatalized

toVelarized :: Phonet -> Phonet
toVelarized = withSecondaryArticulation Velarized

toPharyngealized :: Phonet -> Phonet
toPharyngealized = withSecondaryArticulation Pharyngealized

toNoSecondaryArticulation :: Phonet -> Phonet
toNoSecondaryArticulation = withSecondaryArticulation Normal

toNasalization :: Nasalization -> Phonet -> Maybe Phonet
toNasalization nasalization p =
	case p of
		Consonant {} -> Nothing -- We don't support nasalized consonants yet.
		Vowel x y z vf vl _ -> Just (Vowel x y z vf vl nasalization)

toNasalized :: Phonet -> Maybe Phonet
toNasalized = toNasalization Nasalized