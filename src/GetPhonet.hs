-- | A module for
-- some helper functions for
-- getting the values inside
-- the Phonet type or record.

module GetPhonet where

import Types.Airstream ( Airstream(..))
import Types.Manner ( Manner(..) )
import Types.Nasalization (Nasalization())
import Types.Phonet ( Phonet(..) )
import Types.Place ( Place(..) )
import Types.SecondaryArticulation ( SecondaryArticulation(..) )
import Types.VocalFolds ( VocalFolds(..) )

import Prelude (Maybe(..), Bool(..))

-- | whether a phonete is a consonant.
isConsonant :: Phonet -> Bool
isConsonant p =
	case p of 
		Consonant {} -> True
		Vowel {} -> False

-- | whether a phonete is a vowel.
isVowel :: Phonet -> Bool
isVowel p =
	case p of
		Vowel {} -> True
		Consonant {} -> False

-- | The vocal fold configuration of a phoneme.
vocalFolds :: Phonet -> VocalFolds
vocalFolds p = 
	case p of 
		Consonant vf _ _ _ _ -> vf
		Vowel _ _ _ vf _ _ -> vf

place :: Phonet -> Maybe Place
place p = 
	case p of 
		Consonant _ pl _ _ _ -> Just pl
		Vowel {} -> Nothing

manner :: Phonet -> Maybe Manner
manner p =
	case p of
		Consonant _ _ m _ _ -> Just m
		Vowel {} -> Nothing

airstream :: Phonet -> Maybe Airstream
airstream p =
	case p of
		Consonant _ _ _ a _ -> Just a
		Vowel {} -> Nothing

secondaryArticulation :: Phonet -> Maybe SecondaryArticulation
secondaryArticulation (Consonant _ _ _ _ sa) = Just sa
secondaryArticulation _ = Nothing

nasalization :: Phonet -> Maybe Nasalization
nasalization p =
	case p of
		Consonant {} -> Nothing -- for now until we implement nasal consonants
		Vowel _ _ _ _ _ n -> Just n
