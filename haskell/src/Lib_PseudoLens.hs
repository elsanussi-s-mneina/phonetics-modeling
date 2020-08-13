-- | A module for
-- some helper functions for
-- changing the values inside
-- a type or record.

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}
module Lib_PseudoLens where
import Lib_Types

import Relude (Maybe(Just, Nothing))

-- | The vocal fold configuration of a phoneme.
vocalFolds :: Phonet -> VocalFolds
vocalFolds (Consonant vf _ _ _ _) = vf
vocalFolds (Vowel _ _ _ vf _) = vf

-- | A function for returning
--   a phonete with a possibly different vocal fold configuration of a phonete,
--   but no other difference.
withVocalFolds :: VocalFolds -> Phonet -> Phonet
withVocalFolds vf (Consonant _ w x y z) = (Consonant vf w x y z)
withVocalFolds vf (Vowel x y z _ vl)    = (Vowel x y z vf vl)

place :: Phonet -> Maybe Place
place (Consonant _ p _ _ _) = Just p
place _ = Nothing

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

secondaryArticulation :: Phonet -> Maybe SecondaryArticulation
secondaryArticulation (Consonant _ _ _ _ sa) = Just sa
secondaryArticulation _ = Nothing

withSecondaryArticulation :: SecondaryArticulation -> Phonet -> Maybe Phonet
withSecondaryArticulation x (Consonant a b c d _) = Just (Consonant a b c d x)
withSecondaryArticulation _ _ = Nothing

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


