-- | A module for
-- some helper functions for
-- changing the values inside
-- a type or record.

module Lib_PseudoLens where
import Lib_Types

import Prelude (Maybe(Just, Nothing), Bool(True, False))

-- | whether a phonete is a consonant.
isConsonant :: Phonet -> Bool
isConsonant (Consonant {}) = True
isConsonant _              = False

-- | whether a phonete is a vowel.
isVowel :: Phonet -> Bool
isVowel (Vowel {}) = True
isVowel _          = False

-- | The vocal fold configuration of a phoneme.
vocalFolds :: Phonet -> VocalFolds
vocalFolds (Consonant vf _ _ _ _) = vf
vocalFolds (Vowel _ _ _ vf _)     = vf

-- | A function for returning
--   a phonete with a possibly different vocal fold configuration of a phonete,
--   but no other difference.
withVocalFolds :: VocalFolds -> Phonet -> Phonet
withVocalFolds vf (Consonant _ w x y z) = (Consonant vf w x y z)
withVocalFolds vf (Vowel x y z _ vl)    = (Vowel x y z vf vl)

place :: Phonet -> Maybe Place
place (Consonant _ p _ _ _) = Just p
place _                     = Nothing

withPlace :: Place -> Phonet -> Phonet
withPlace x (Consonant a _ b c d) =  Consonant a x b c d
withPlace _ p = p

manner :: Phonet -> Maybe Manner
manner (Consonant _ _ m _ _) = Just m
manner _ = Nothing

withManner :: Manner -> Phonet -> Phonet
withManner x (Consonant a b _ c d) = (Consonant a b x c d)
withManner _ p = p

airstream :: Phonet -> Maybe Airstream
airstream (Consonant _ _ _ a _) = Just a
airstream _ = Nothing

withAirstream :: Airstream -> Phonet -> Maybe Phonet
withAirstream x (Consonant a b c _ d) = Just (Consonant a b c x d)
withAirstream _ _ = Nothing

withVowelLength :: VowelLength -> Phonet -> Phonet
withVowelLength vl (Vowel height backness rounding voicing _ ) =
                   (Vowel height backness rounding voicing vl)
withVowelLength _ p = p -- Ignore phonetes that are not vowels.

toLong :: Phonet -> Phonet
toLong = withVowelLength Long

toHalfLong :: Phonet -> Phonet
toHalfLong = withVowelLength HalfLong

toExtraShort :: Phonet -> Phonet
toExtraShort = withVowelLength ExtraShort

secondaryArticulation :: Phonet -> Maybe SecondaryArticulation
secondaryArticulation (Consonant _ _ _ _ sa) = Just sa
secondaryArticulation _ = Nothing

-- | Changes the secondary articulation value
--   for a phonete.
--   Ignore vowels, we don't add any secondary articulation for them
withSecondaryArticulation :: SecondaryArticulation -> Phonet -> Phonet
withSecondaryArticulation x (Consonant a b c d _) = Consonant a b c d x
withSecondaryArticulation _ x = x


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