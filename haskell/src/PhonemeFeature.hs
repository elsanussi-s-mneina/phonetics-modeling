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


data MajorSoundClass = PlosiveSC | FricativeSC | AffricateSC | NasalSC | ApproximantSC |LateralSC | GlideSC | VowelSC

-- Table 12.2
table12_2_column_labels = [PlosiveSC , FricativeSC, AffricateSC, NasalSC, ApproximantSC, LateralSC, GlideSC, VowelSC]
table12_2_binary_row_features = [SyllabicFeature, ConsonantalFeature, SonorantFeature, ContinuantFeature]

table_12_2_binary_features :: [[String]]
table_12_2_binary_features =
  [[ "-" ,  "-" ,  "-" ,  "-" ,  "-" ,  "-" ,  "-" ,  "+" ]
  ,[ "+" ,  "+" ,  "+" ,  "+" ,  "+" ,  "+" ,  "-" ,  "-" ]
  ,[ "-" ,  "-" ,  "-" ,  "+" ,  "+" ,  "+" ,  "+" ,  "+" ]
  ,[ "-" ,  "+" ,  "-" ,  "-" ,  "+" , "(+)",  "+" ,  "+" ]
  ]

table12_2_unary_row_features = [NasalFeature, LateralFeature, DelayedReleaseFeature]

table_12_2_unaryFeatures :: [[String]]
table_12_2_unaryFeatures =
  [[ ""  ,  ""  ,  ""  ,  "N" ,  ""  , ""   ,  ""  ,  ""  ]
  ,[ ""  ,  ""  ,  ""  ,  "" ,   ""  , "L"  ,  ""  ,  ""  ]
  ,[ ""  ,  ""  ,  "DR",  "" ,   ""  , ""   ,  ""  ,  ""  ]
  ]

nasal (Consonant _ _ Nasal _) = True
nasal _ = False

lateral (Consonant _ _ manner _) = manner == Lateral || manner == LateralApproximant || manner == LateralFricative
lateral _ = False


delayedRelease (Consonant _ _ Affricate _) = True
delayedRelease _ = False

labial (Consonant _ place _ _) = place == Bilabial || place == LabioDental
labial _ = False

coronal (Consonant _ place _ _) = place `elem` [Dental, Alveolar, PostAlveolar, Retroflex, Palatal]
coronal _ = False

dorsal (Consonant _ place _ _) = place `elem` [Palatal, Velar, Uvular] -- Palatal is actually in parentheses in the textbook
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
anterior _ = Nothing

distributed (Consonant _ Dental _ _) = Just True
distributed (Consonant _ Alveolar _ _) = Just False
distributed (Consonant _ PostAlveolar _ _) = Just True
distributed (Consonant _ Retroflex _ _) = Just False
distributed (Consonant _ Palatal _ _) = Just True
distributed _ = Nothing


strident (Consonant _ Bilabial _ _) = Just False
strident (Consonant _ LabioDental _ _) = Just True
strident (Consonant _ Dental _ _) = Just False
strident (Consonant _ Alveolar _ _) = Just True
strident (Consonant _ PostAlveolar _ _) = Just True
strident (Consonant _ Retroflex _ _) = Just False
strident (Consonant _ Palatal _ _) = Just False
strident (Consonant _ Velar _ _) = Just False
strident (Consonant _ Uvular _ _) = Just True
strident (Consonant _ Pharyngeal _ _) = Just False
strident (Consonant _ Glottal _ _) = Just False
strident _ = Nothing

high (Consonant _ Palatal _ _) = Just True
high (Consonant _ Velar _ _) = Just True
high (Consonant _ Uvular _ _) = Just False
high (Consonant _ _ _ _) = Nothing
high _ = Nothing

low (Consonant _ Uvular _ _) = Just True
low (Consonant _ Pharyngeal _ _) = Just True
low (Consonant _ Glottal _ _) = Just True
low (Consonant _ _ _ _) = Nothing
low _ = Nothing



toTextFeatures :: Phonet -> String
toTextFeatures phonete =
  let anteriorString = toTextAnteriorFeature phonete
      distributedString = toTextDistributedFeature phonete
      stridentString = toTextStridentFeature phonete
      highString = toTextHighFeature phonete
      lowString = toTextLowFeature phonete
  in concatIgnoringNothing "; " [anteriorString, distributedString, stridentString, highString, lowString]

concatIgnoringNothing :: String -> [Maybe String] -> String
concatIgnoringNothing _ [] = ""
concatIgnoringNothing joiner (Nothing:xs) = concatIgnoringNothing joiner xs
concatIgnoringNothing joiner ((Just x):xs) = x ++ joiner ++ concatIgnoringNothing joiner xs


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



toTextGenericFeature :: (Phonet -> Maybe Bool) -> String -> Phonet -> Maybe String
toTextGenericFeature featureFunction featureName phonete =
  let featureValue = featureFunction phonete
  in if featureValue == Just True
     then Just ("+ " ++ featureName)
     else if featureValue == Just False
        then Just ("- " ++ featureName)
        else Nothing