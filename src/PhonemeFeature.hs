module PhonemeFeature where

import Lib

-- Part for features:
-- Go to Section 12.2 of the textbook.

data BinaryFeatures = SyllabicFeature | ConsonantalFeature | SonorantFeature | ContinuantFeature | VoiceFeature
data UnaryFeatures = NasalFeature | LateralFeature | DelayedReleaseFeature | SpreadGlottisFeature | ConstrictedGlottisFeature
                   | LabialFeature | CoronalFeature | DorsalFeature | PharyngealFeature

syllabic :: Phonet -> Bool
syllabic (Consonant _ _ _ _) = False
syllabic (Vowel _ _ _ _ ) = True

isGlide :: Phonet -> Bool
isGlide candidate =
  let firstPart = head (constructIPA candidate)  -- So that we ignore any diacritics that come after.
  in firstPart == 'j' ||
     firstPart == 'w' ||
     firstPart == 'ɥ' ||
     firstPart == 'ɰ'


consonantal :: Phonet -> Bool
consonantal consonant@(Consonant v p m a) = not (isGlide consonant)
consonantal (Vowel _ _ _ _) = False

sonorant :: Phonet -> Bool
-- Vowels are sonorants.
sonorant (Vowel _ _ _ _) = True
-- Nasals are sonorants.
sonorant (Consonant _ _ Nasal _) = True
-- Approximants are sonorants.
sonorant (Consonant _ _ Approximant _) = True
-- Laterals are sonorants.
sonorant (Consonant _ _ LateralApproximant _ ) = True
-- Are Lateral flaps, and Laterals that are not fricatives approximants. Let us just
-- guess that they are:
-- sonorant (Consonants _ _ Lateral _ ) = True -- unsure

-- sonorant (Consonants _ _ LateralFlap _ ) = Flap -- unsure whether this is true
-- Fricatives are not sonorants.
sonorant (Consonant _ _ Fricative _) = False
-- Lateral fricatives are not sonorants.
sonorant (Consonant _ _ LateralFricative _) = False
-- Affricates are not sonorants.
sonorant (Consonant _ _ Affricate _) = False
