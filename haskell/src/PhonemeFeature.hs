module PhonemeFeature where

import InternationalPhoneticAlphabet
import Lib

-- Part for features:
-- Go to Section 12.2 of the textbook.
data BinaryFeature = SyllabicFeature | ConsonantalFeature | SonorantFeature | ContinuantFeature | VoiceFeature
data UnaryFeature = NasalFeature | LateralFeature | DelayedReleaseFeature | SpreadGlottisFeature | ConstrictedGlottisFeature
                   | LabialFeature | CoronalFeature | DorsalFeature | PharyngealFeature | LaryngealFeature

syllabic :: Phonet -> Bool
syllabic (Vowel _ _ _ _ ) = True
syllabic (Consonant _ _ _ _) = False

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


continuant (Consonant _ _ Fricative _) = True
continuant (Consonant _ _ Approximant _ ) = True
continuant (Consonant _ _ Lateral _) = True
continuant (Consonant _ _ LateralFricative _) = True
continuant (Consonant _ _ LateralApproximant _) = True
continuant c@(Consonant _ _ _ _) = isGlide c
continuant (Vowel _ _ _ _) = True



nasal (Consonant _ _ Nasal _) = True
nasal _ = False

lateral (Consonant _ _ manner _) = manner == Lateral || manner == LateralApproximant || manner == LateralFricative
lateral _ = False


delayedRelease (Consonant _ _ Affricate _) = True
delayedRelease _ = False

labial (Consonant _ place _ _) = place == Bilabial || place == LabioDental
labial _ = False

coronal (Consonant _ place _ _) = place `elem` [Dental, Alveolar, PostAlveolar, Retroflex, Palatal, AlveoloPalatal]
coronal _ = False

dorsal (Consonant _ place _ _) = place `elem` [Palatal, AlveoloPalatal, Velar, Uvular] -- Palatal is actually in parentheses in the textbook
dorsal _ = False

pharyngeal (Consonant _ Pharyngeal _ _) = True
pharyngeal _ = False

laryngeal (Consonant _ Glottal _ _ ) = True
laryngeal _ = False


voice (Consonant v _ _ _) = v == Voiced
voice (Vowel _ _ _ v) = v == Voiced


anterior (Consonant _ Dental _ _) = Just True
anterior (Consonant _ Alveolar _ _) = Just True
anterior (Consonant _ PostAlveolar _ _) = Just False
anterior (Consonant _ Retroflex _ _) = Just False
anterior (Consonant _ Palatal _ _) = Just False
anterior (Consonant _ AlveoloPalatal _ _) = Just False
anterior _ = Nothing

distributed (Consonant _ Dental _ _) = Just True
distributed (Consonant _ Alveolar _ _) = Just False
distributed (Consonant _ PostAlveolar _ _) = Just True
distributed (Consonant _ Retroflex _ _) = Just False
distributed (Consonant _ Palatal _ _) = Just True
distributed (Consonant _ AlveoloPalatal _ _) = Just True
distributed _ = Nothing


strident (Consonant _ Bilabial _ _) = Just False
strident (Consonant _ LabioDental _ _) = Just True
strident (Consonant _ Dental _ _) = Just False
strident (Consonant _ Alveolar _ _) = Just True
strident (Consonant _ PostAlveolar _ _) = Just True
strident (Consonant _ Retroflex _ _) = Just False
strident (Consonant _ Palatal _ _) = Just False
strident (Consonant _ AlveoloPalatal _ _) = Just False
strident (Consonant _ Velar _ _) = Just False
strident (Consonant _ Uvular _ _) = Just True
strident (Consonant _ Pharyngeal _ _) = Just False
strident (Consonant _ Glottal _ _) = Just False
strident _ = Nothing

high (Consonant _ Palatal _ _) = Just True
high (Consonant _ AlveoloPalatal _ _) = Just True
high (Consonant _ Velar _ _) = Just True
high (Consonant _ Uvular _ _) = Just False
high (Consonant _ _ _ _) = Nothing
high (Vowel height _ _ _ ) = Just (height == Close || height == NearClose)
high _ = Nothing

low (Consonant _ Uvular _ _) = Just True
low (Consonant _ Pharyngeal _ _) = Just True
low (Consonant _ Glottal _ _) = Just True
low (Consonant _ _ _ _) = Nothing
low (Vowel height _ _ _ ) = Just (height == Open || height == NearOpen)
low _ = Nothing


back (Vowel _ Back _ _) = Just True
back (Vowel _ Central _ _) = Just True
back (Vowel _ Front _ _) = Just False
back _ = Nothing

round (Vowel _ _ rounding _) = Just (rounding == Rounded)
round _ = Just False


atr vowel@(Vowel _ _ _ _) = Just (vowel == analyzeIPA("i") || vowel == analyzeIPA("e") ||
            vowel == analyzeIPA("u") || vowel == analyzeIPA("ø") || 
            vowel == analyzeIPA("o") || vowel == analyzeIPA("y"))
atr (Vowel NearClose Front Unrounded Voiced) = Just False
atr (Vowel NearClose Back  Rounded   Voiced) = Just False
atr (Vowel  OpenMid Front   Unrounded Voiced) = Just False
atr (Vowel  OpenMid Back    Rounded   Voiced) = Just False
atr _ = Nothing

toTextFeatures :: Phonet -> String
toTextFeatures phonete =
  let allString = mapf [toTextAnteriorFeature, toTextDistributedFeature,
                        toTextStridentFeature, toTextHighFeature, toTextLowFeature, toTextNasalFeature, toTextLabialFeature, toTextCoronalFeature, toTextDorsalFeature, 
                        toTextPharyngealFeature, toTextLaryngealFeature,
                        toTextBackFeature, toTextRoundFeature,
                        toTextATRFeature] phonete
  in "[" ++ concatIgnoringNothing "; " allString ++ "]"

mapf :: [Phonet -> Maybe String] -> Phonet -> [Maybe String]
mapf functions phonete = map (\f -> f phonete) functions

concatIgnoringNothing :: String -> [Maybe String] -> String
concatIgnoringNothing _ [] = ""
concatIgnoringNothing joiner (Nothing:xs) = concatIgnoringNothing joiner xs
concatIgnoringNothing joiner ((Just x):xs) = x ++ joiner ++ concatIgnoringNothing joiner xs


toTextBackFeature :: Phonet -> Maybe String
toTextBackFeature phonete = 
  toTextGenericFeature back "back" phonete


toTextRoundFeature :: Phonet -> Maybe String
toTextRoundFeature phonete = 
  toTextGenericFeature PhonemeFeature.round "round" phonete

toTextATRFeature :: Phonet -> Maybe String
toTextATRFeature phonete = 
  toTextGenericFeature atr "ATR" phonete




toTextAnteriorFeature :: Phonet -> Maybe String
toTextAnteriorFeature phonete = 
  toTextGenericFeature anterior "anterior" phonete

toTextDistributedFeature :: Phonet -> Maybe String
toTextDistributedFeature phonete = 
  toTextGenericFeature distributed "distributed" phonete


toTextStridentFeature :: Phonet -> Maybe String
toTextStridentFeature phonete = 
  toTextGenericFeature strident "strident" phonete
 

toTextHighFeature :: Phonet -> Maybe String
toTextHighFeature phonete = 
  toTextGenericFeature high "high" phonete

toTextLowFeature :: Phonet -> Maybe String
toTextLowFeature phonete = 
  toTextGenericFeature low "low" phonete


toTextNasalFeature :: Phonet -> Maybe String
toTextNasalFeature phonete =
  toTextUnaryFeature nasal "nasal" phonete

toTextLateralFeature :: Phonet -> Maybe String
toTextLateralFeature phonete =
  toTextUnaryFeature lateral "lateral" phonete


toTextLabialFeature :: Phonet -> Maybe String
toTextLabialFeature phonete =
  toTextUnaryFeature labial "labial" phonete

toTextCoronalFeature :: Phonet -> Maybe String
toTextCoronalFeature phonete =
  toTextUnaryFeature coronal "coronal" phonete

toTextDorsalFeature :: Phonet -> Maybe String
toTextDorsalFeature phonete =
  toTextUnaryFeature dorsal "dorsal" phonete

toTextPharyngealFeature :: Phonet -> Maybe String
toTextPharyngealFeature phonete =
  toTextUnaryFeature pharyngeal "pharyngeal" phonete

toTextLaryngealFeature :: Phonet -> Maybe String
toTextLaryngealFeature phonete =
  toTextUnaryFeature laryngeal "laryngeal" phonete



toTextUnaryFeature :: (Phonet -> Bool) -> String -> Phonet -> Maybe String
toTextUnaryFeature featureFunction featureName phonete =
  let featureValue = featureFunction phonete
  in if featureValue
       then Just featureName
       else Nothing

toTextGenericFeature :: (Phonet -> Maybe Bool) -> String -> Phonet -> Maybe String
toTextGenericFeature featureFunction featureName phonete =
  let featureValue = featureFunction phonete
  in if featureValue == Just True
     then Just ("+ " ++ featureName)
     else if featureValue == Just False
        then Just ("- " ++ featureName)
        else Nothing
