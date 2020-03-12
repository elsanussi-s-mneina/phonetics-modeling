module PhonemeFeature where

-- Design decisions:
-- This module should not depend on any module
-- that is specifically for representing graphemes.
-- For this reason there should be no import of
-- a module that represents the International Phonetic
-- Alphabet. The reason for this design decision, is
-- that we want to be able to easily add other
-- alphabets or modifications to the IPA
-- without editing this module.

import Lib

import Data.List (intercalate)
-- Part for features:
-- Go to Section 12.2 of the textbook.

-- TODO: Determine if the "phoneme feature"
-- is the term used by linguists for the concept
-- being modeled in this module.

{-|
 Represents the '+' (plus) or '-' (minus)
 of a binary feature. e.g. [+sonorant],
 [-sonorant]
|-}
data Polarity = Plus | Minus
                deriving Eq
{-|
 According to Linguistics, phonemes can be
 analyzed as a set of features. One phoneme
 will have one set of features, and a different
 phoneme will have a different set of features.
 
 These features are well known in phonology, and
 are limited in number. There are two kinds of
 features, unary features, and binary features. The
 difference is obvious in how they are represented in
 the notation that linguists use. Unary features,
 are either present or absent. Binary features
 can be positve or negative. For example, Nasal
 is a unary feature. A phoneme is either nasal,
 or it isn't. i.e. [nasal] or not. For example,
 Voice is a binary feature, a phoneme can be
 [+voice] (can be pronounced: "plus voice")
 or [-voice] (can be pronounced: "minus voice").
 
 Because linguists represent phonemic features in these
 two different ways. We represent these as two
 different kinds of types.
 
 So [nasal] which is a unary feature would be
 represented by a value `NasalFeature` of type `PhonemeFeature`.
 And [+voice] which is a binary feature would
 be represented by a value `VoiceFeature Plus` of type
 `PhonemeFeature`.
 
 We represent the plus or minus symbol by
 the type Polarity.
 
 Notice that: Linguists write a set of features
 as a 2D matrix with one column, roughly like this:
 [ +voice     ]
 [ +sonorant  ]
 [  nasal     ]

Note that certain sets of features cannot coexist,
syntactically. For example a phoneme cannot be
[+voice] and [-voice].

TODO: implement checking whether a set of phonemes
contains non-existant pairs (+ and - for the same
name of feature).

 Note that some analyses
are language specific, so for some phonemes (not
the usual case) whether it has feature X (say 'coronal')
depends on the language (theoretical example: e.g. Swahili,
vs French). This is not implemented here.

TODO: model the ability to decide whether certain phonemes
have certain features based on a language, or let the user
decide.
|-}
data PhonemeFeature = SyllabicFeature Polarity
                    | ConsonantalFeature Polarity
                    | SonorantFeature Polarity
                    | ContinuantFeature Polarity
                    | VoiceFeature Polarity
                    | AdvancedTongueRootFeature Polarity
                    | NasalFeature
                    | LateralFeature
                    | DelayedReleaseFeature
                    | SpreadGlottisFeature
                    | ConstrictedGlottisFeature
                    | LabialFeature
                    | CoronalFeature
                    | DorsalFeature
                    | PharyngealFeature
                    | LaryngealFeature
                    | RoundFeature Polarity
                    | AnteriorFeature Polarity
                    | DistributedFeature Polarity
                    | StridentFeature Polarity
                    | HighFeature Polarity
                    | LowFeature Polarity
                    | BackFeature Polarity
                    deriving Eq

instance Show Polarity where
  show Plus = "+"
  show Minus = "-"

instance Show PhonemeFeature where
  show (SyllabicFeature polarity)           = show polarity ++ "syllabic"
  show (ConsonantalFeature polarity)        = show polarity ++ "consonantal"
  show (SonorantFeature polarity)           = show polarity ++ "sonorant"
  show (ContinuantFeature polarity)         = show polarity ++ "continuant"
  show (VoiceFeature polarity)              = show polarity ++ "voice"
  show (AdvancedTongueRootFeature polarity) = show polarity ++ "ATR"
  show NasalFeature                         = "nasal"
  show LateralFeature                       = "lateral"
  show DelayedReleaseFeature                = "DR"
  show SpreadGlottisFeature                 = "SG"
  show ConstrictedGlottisFeature            = "CG"
  show LabialFeature                        = "labial"
  show CoronalFeature                       = "coronal"
  show DorsalFeature                        = "dorsal"
  show PharyngealFeature                    = "pharyngeal"
  show LaryngealFeature                     = "laryngeal"
  show (RoundFeature polarity)              = show polarity ++ "round"
  show (AnteriorFeature polarity)           = show polarity ++ "anterior"
  show (DistributedFeature polarity)        = show polarity ++ "distributed"
  show (StridentFeature polarity)           = show polarity ++ "strident"
  show (HighFeature polarity)               = show polarity ++ "high"
  show (LowFeature polarity)                = show polarity ++ "low"
  show (BackFeature polarity)               = show polarity ++ "back"

-- | A function for determining if a feature is unary or not.
isUnary :: PhonemeFeature -> Bool
isUnary NasalFeature = True
isUnary LateralFeature = True
isUnary DelayedReleaseFeature = True
isUnary SpreadGlottisFeature = True
isUnary ConstrictedGlottisFeature = True
isUnary LabialFeature = True
isUnary CoronalFeature = True
isUnary DorsalFeature = True
isUnary PharyngealFeature = True
isUnary LaryngealFeature = True
isUnary _ = False

-- | A function that takes data representing
-- how a phoneme is pronounced, and returns
-- a list of phonemic features.
analyzeFeatures :: Phonet -> [PhonemeFeature]
analyzeFeatures phonete =
  dejustifyList [ syllabic phonete
                , consonantal phonete
                , sonorant phonete
                , continuant phonete
                , voice phonete
                , atr phonete
                , nasal phonete
                , lateral phonete
                , delayedRelease phonete
                , spreadGlottis phonete
                , constrictedGlottis phonete
                , labial phonete
                , coronal phonete
                , dorsal phonete
                , pharyngeal phonete
                , laryngeal phonete
                , PhonemeFeature.round phonete
                , anterior phonete
                , distributed phonete
                , strident phonete
                , high phonete
                , low phonete
                , back phonete
                ]


dejustifyList :: [Maybe a] -> [a]
dejustifyList [] = []
dejustifyList (Just x: xs) = x:(dejustifyList xs)
dejustifyList (Nothing:xs) = dejustifyList xs

-- | This function takes two lists of phoneme features
-- and returns how they differ. Any phonemic
-- feature present in one list, and absent in the other
-- will be represented; and any phonemic
-- feature that is positive in one list but absent
-- in the other will be represented.
difference :: [PhonemeFeature] 
           -> [PhonemeFeature] 
           -> [(Maybe PhonemeFeature, Maybe PhonemeFeature)]
difference list1 list2 = 
  [ differenceOfBinaryFeature SyllabicFeature list1 list2
  , differenceOfBinaryFeature ConsonantalFeature list1 list2
  , differenceOfBinaryFeature SonorantFeature list1 list2
  , differenceOfBinaryFeature ContinuantFeature list1 list2
  , differenceOfBinaryFeature VoiceFeature list1 list2
  , differenceOfBinaryFeature AdvancedTongueRootFeature list1 list2
  , differenceOfUnaryFeature NasalFeature list1 list2
  , differenceOfUnaryFeature LateralFeature list1 list2
  , differenceOfUnaryFeature DelayedReleaseFeature list1 list2
  , differenceOfUnaryFeature SpreadGlottisFeature list1 list2
  , differenceOfUnaryFeature ConstrictedGlottisFeature list1 list2
  , differenceOfUnaryFeature LabialFeature list1 list2
  , differenceOfUnaryFeature CoronalFeature list1 list2
  , differenceOfUnaryFeature DorsalFeature list1 list2
  , differenceOfUnaryFeature PharyngealFeature list1 list2
  , differenceOfUnaryFeature LaryngealFeature list1 list2
  , differenceOfBinaryFeature RoundFeature list1 list2
  , differenceOfBinaryFeature AnteriorFeature list1 list2
  , differenceOfBinaryFeature DistributedFeature list1 list2
  , differenceOfBinaryFeature StridentFeature list1 list2
  , differenceOfBinaryFeature HighFeature list1 list2
  , differenceOfBinaryFeature LowFeature list1 list2
  , differenceOfBinaryFeature BackFeature list1 list2
  ]

differenceOfBinaryFeature :: 
          (Polarity -> PhonemeFeature) 
                    -> [PhonemeFeature] 
                    -> [PhonemeFeature] 
                    -> (Maybe PhonemeFeature, Maybe PhonemeFeature)
differenceOfBinaryFeature feature list1 list2 =
   let
   relevant x = x == feature Plus || x == feature Minus
   list1Relevant = filter relevant list1
   list2Relevant = filter relevant list2
   in
     if  null list1Relevant && null list2Relevant
       || (not (null list1Relevant) && not (null list2Relevant) &&
           head list1Relevant == head list2Relevant)
       then (Nothing, Nothing)
       else if not (null list1Relevant) && not (null list2Relevant)
         then (Just (head list1Relevant), Just (head list2Relevant))
         else if length list1Relevant > length list2Relevant
         then (Just (head list1Relevant), Nothing)
           else (Nothing, Just (head list2Relevant))
           

differenceOfUnaryFeature :: PhonemeFeature 
                         -> [PhonemeFeature] 
                         -> [PhonemeFeature] 
                         -> (Maybe PhonemeFeature, Maybe PhonemeFeature)
differenceOfUnaryFeature feature list1 list2 =
  if elem feature list1 == elem feature list2
     then (Nothing, Nothing)
     else if elem feature list1 && not (elem feature list2)
            then (Just feature, Nothing)
            else (Nothing, Just feature)

syllabic :: Phonet -> Maybe PhonemeFeature
syllabic (Vowel _ _ _ _ ) = Just (SyllabicFeature Plus)
syllabic (Consonant _ _ _ _) = Just (SyllabicFeature Minus)


isGlide :: Phonet -> Bool
isGlide (Consonant _ Palatal Approximant PulmonicEgressive) = True
isGlide (Consonant _ LabialVelar Approximant PulmonicEgressive) = True
isGlide (Consonant _ LabialPalatal Approximant PulmonicEgressive) = True
isGlide (Consonant _ Velar Approximant PulmonicEgressive) = True
isGlide _ = False


consonantal :: Phonet -> Maybe PhonemeFeature
consonantal consonant@(Consonant _ _ _ _)
  | isGlide consonant = Just (ConsonantalFeature Minus)
  | otherwise         = Just (ConsonantalFeature Plus)
consonantal (Vowel _ _ _ _) = Nothing



sonorant :: Phonet -> Maybe PhonemeFeature
-- Vowels are sonorants.
sonorant (Vowel _ _ _ _) = Just (SonorantFeature Plus)
-- Nasals are sonorants.
sonorant (Consonant _ _ Nasal _) = Just (SonorantFeature Plus)
-- Approximants are sonorants.
sonorant (Consonant _ _ Approximant _) = Just (SonorantFeature Plus)
-- Laterals are sonorants.
sonorant (Consonant _ _ LateralApproximant _ ) = Just (SonorantFeature Plus)
-- Are Lateral flaps, and Laterals that are not fricatives approximants.
-- Let us just guess that they are:
-- sonorant (Consonants _ _ Lateral _ ) = Just (SonorantFeature Plus) -- unsure

-- sonorant (Consonants _ _ LateralFlap _ ) = Flap
-- I am unsure whether lateral flaps are sonorants. That is
-- why the previous line of code is commented out.


-- Fricatives are not sonorants.
sonorant (Consonant _ _ Fricative _) = Just (SonorantFeature Minus)
-- Lateral fricatives are not sonorants.
sonorant (Consonant _ _ LateralFricative _) = Just  (SonorantFeature Minus)
-- Affricates are not sonorants.
sonorant (Consonant _ _ Affricate _) = Just  (SonorantFeature Minus)
sonorant _ = Just  (SonorantFeature Minus) -- Add more



continuant :: Phonet -> Maybe PhonemeFeature
continuant (Consonant _ _ Fricative _) = Just (ContinuantFeature Plus)
continuant (Consonant _ _ Approximant _ ) = Just (ContinuantFeature Plus)
continuant (Consonant _ _ Lateral _) = Just (ContinuantFeature Plus)
continuant (Consonant _ _ LateralFricative _) = Just (ContinuantFeature Plus)
continuant (Consonant _ _ LateralApproximant _) = Just (ContinuantFeature Plus)
continuant consonant@(Consonant _ _ _ _)
  | isGlide consonant = Just (ContinuantFeature Plus)
  | otherwise         = Just (ContinuantFeature Minus)
continuant (Vowel _ _ _ _) = Just (ContinuantFeature Plus)


nasal :: Phonet -> Maybe PhonemeFeature
nasal (Consonant _ _ Nasal _) = Just NasalFeature
nasal _ = Nothing


lateral :: Phonet -> Maybe PhonemeFeature
lateral (Consonant _ _ Lateral _) = Just LateralFeature
lateral (Consonant _ _ LateralApproximant _) = Just LateralFeature
lateral (Consonant _ _ LateralFricative _) = Just LateralFeature
lateral _ = Nothing

{-|
Affricates are [+delayed release].
All other segments are [-delayed release].

(Source: page 260)
|-}
delayedRelease :: Phonet -> Maybe PhonemeFeature
delayedRelease (Consonant _ _ Affricate _) = Just DelayedReleaseFeature
delayedRelease _ = Nothing


{-|
Bilabial consonants are [labial].
Labio-dental consonants are [labial].
All other sounds are undefined for [labial].

(Source: page 264)
|-}
labial :: Phonet -> Maybe PhonemeFeature
labial (Consonant _ Bilabial _ _) = Just LabialFeature
labial (Consonant _ LabioDental _ _) = Just LabialFeature
labial _ = Nothing


{-|
Dentals are [coronal].
Alveolars are [coronal] also.
Alveolopalatals are [coronal] also.
Retroflexes are [coronal] also.
Palatals are [coronal] also.

Post-alveolars are [coronal] also.

All other sounds are undefined for [coronal].

(Source: page 264)
(The fact that Post-alveolar consonants are coronal is indicated by
 Table 12. on page 265.)
|-}
coronal :: Phonet -> Maybe PhonemeFeature
coronal (Consonant _ Dental         _ _) = Just CoronalFeature
coronal (Consonant _ Alveolar       _ _) = Just CoronalFeature
coronal (Consonant _ AlveoloPalatal _ _) = Just CoronalFeature
coronal (Consonant _ Retroflex      _ _) = Just CoronalFeature
coronal (Consonant _ Palatal        _ _) = Just CoronalFeature

coronal (Consonant _ PostAlveolar   _ _) = Just CoronalFeature

coronal _                                = Nothing



dorsal :: Phonet -> Maybe PhonemeFeature
dorsal (Consonant _ Palatal        _ _) = Just DorsalFeature  -- Aside: Palatal is actually in parentheses in the textbook
dorsal (Consonant _ AlveoloPalatal _ _) = Just DorsalFeature
dorsal (Consonant _ Velar          _ _) = Just DorsalFeature
dorsal (Consonant _ Uvular         _ _) = Just DorsalFeature
dorsal _                                = Nothing


{-|
Pharyngeal fricatives are [pharyngeal].
All other sounds are undefined for [pharyngeal].

(Source: page 264)
|-}
pharyngeal :: Phonet -> Maybe PhonemeFeature
pharyngeal (Consonant _ Pharyngeal Fricative _) = Just PharyngealFeature
pharyngeal _ = Nothing


laryngeal :: Phonet -> Maybe PhonemeFeature
laryngeal (Consonant _ Glottal _ _ ) = Just LaryngealFeature
laryngeal _ = Nothing



voice :: Phonet -> Maybe PhonemeFeature
voice (Consonant Voiceless Glottal Plosive PulmonicEgressive) = Just (VoiceFeature Minus)
-- The voiceless glottal plosive is [-voice]
voice (Consonant Voiced                                _ _ _) = Just (VoiceFeature Plus)
voice (Consonant VoicedAspirated                       _ _ _) = Just (VoiceFeature Plus)
voice (Vowel _ _ _                                    Voiced) = Just (VoiceFeature Plus)
voice _                                                       = Just (VoiceFeature Minus)


spreadGlottis :: Phonet -> Maybe PhonemeFeature
spreadGlottis (Consonant VoicelessAspirated _ _ _) = Just SpreadGlottisFeature
spreadGlottis (Consonant VoicedAspirated    _ _ _) = Just SpreadGlottisFeature
spreadGlottis _ = Nothing



constrictedGlottis :: Phonet -> Maybe PhonemeFeature
constrictedGlottis (Consonant
                      Voiceless
                      Glottal
                      Plosive
                      PulmonicEgressive) =
  Just ConstrictedGlottisFeature
constrictedGlottis _  = Nothing
-- TODO: add CreakyVoice here = Just True


anterior :: Phonet -> Maybe PhonemeFeature
anterior (Consonant _ Dental            _ _) = Just (AnteriorFeature Plus)
anterior (Consonant _ Alveolar          _ _) = Just (AnteriorFeature Plus)
anterior (Consonant _ PostAlveolar      _ _) = Just (AnteriorFeature Minus)
anterior (Consonant _ Retroflex         _ _) = Just (AnteriorFeature Minus)
anterior (Consonant _ Palatal           _ _) = Just (AnteriorFeature Minus)
anterior (Consonant _ AlveoloPalatal    _ _) = Just (AnteriorFeature Minus)
anterior _ = Nothing

distributed :: Phonet -> Maybe PhonemeFeature
distributed (Consonant _ Dental         _ _) = Just (DistributedFeature Plus)
distributed (Consonant _ Alveolar       _ _) = Just (DistributedFeature Minus)
distributed (Consonant _ PostAlveolar   _ _) = Just (DistributedFeature Plus)
distributed (Consonant _ Retroflex      _ _) = Just (DistributedFeature Minus)
distributed (Consonant _ Palatal        _ _) = Just (DistributedFeature Plus)
distributed (Consonant _ AlveoloPalatal _ _) = Just (DistributedFeature Plus)
distributed _ = Nothing


{-|
Alveolar fricatives are [+strident].
Alveolar affricates are [+strident], also.
Post-alveolar fricatives are [+strident], also.
Post-alveolar affricates are [+strident], also.
Labio-dental fricatives are [+strident] , also.
Labio-dental affricates are [+strident] , also.
Uvular fricatives are [+strident], also.
Uvular affricates are [+strident], also.

All other fricatives are [-strident].
All other affricates are [-strident], also.

All other segments are undefined for [+/-strident].

(For source of facts
 see page 266, under [+/-strident] heading, under the subsection "Natural classes".)
|-}
strident :: Phonet -> Maybe PhonemeFeature
strident (Consonant _ Alveolar       Fricative _) = Just (StridentFeature Plus)
strident (Consonant _ Alveolar       Affricate _) = Just (StridentFeature Plus)
strident (Consonant _ PostAlveolar   Fricative _) = Just (StridentFeature Plus)
strident (Consonant _ PostAlveolar   Affricate _) = Just (StridentFeature Plus)
strident (Consonant _ LabioDental    Fricative _) = Just (StridentFeature Plus)
strident (Consonant _ LabioDental    Affricate _) = Just (StridentFeature Plus)
strident (Consonant _ Uvular         Fricative _) = Just (StridentFeature Plus)
strident (Consonant _ Uvular         Affricate _) = Just (StridentFeature Plus)

strident (Consonant _ _              Fricative _) = Just (StridentFeature Minus)
strident (Consonant _ _              Affricate _) = Just (StridentFeature Minus)

strident _                                        = Nothing


{-|
Meaning:

Palatal consonants are [+high].
Alveolo-palatal consonants are [+high].
Velar consonants are [+high].

Uvular consonants are [-high].
All other consonants are undefined for [+/-high].
Close vowels are [+high].
Near-close vowels are [+high].
All other vowels are [-high].
|-}
high :: Phonet -> Maybe PhonemeFeature
high (Consonant _ Palatal _ _) = Just (HighFeature Plus)
high (Consonant _ AlveoloPalatal _ _) = Just (HighFeature Plus)
high (Consonant _ Velar _ _) = Just (HighFeature Plus)
high (Consonant _ Uvular _ _) = Just (HighFeature Minus)
high (Consonant _ _ _ _) = Nothing
high (Vowel Close _ _ _ ) = Just (HighFeature Plus)
high (Vowel NearClose _ _ _ ) = Just (HighFeature Plus)
high (Vowel _ _ _ _) = Just (HighFeature Minus)


{-|
Meaning:

Uvular consonants are [+low].
Pharyngeal consonants are [+low].
Glottal consonants are [+low].
All other consonants are undefined for [+/-low].
Open vowels are [+low].
Near open vowels are [+low].
All other vowels are [-low].
|-}
low :: Phonet -> Maybe PhonemeFeature
low (Consonant _ Uvular _ _) = Just (LowFeature Plus)
low (Consonant _ Pharyngeal _ _) = Just (LowFeature Plus)
low (Consonant _ Glottal _ _) = Just (LowFeature Plus)
low (Consonant _ _ _ _) = Nothing
low (Vowel Open _ _ _ ) = Just (LowFeature Plus)
low (Vowel NearOpen _ _ _ ) = Just (LowFeature Plus)
low (Vowel _ _ _ _ ) = Just (LowFeature Minus)


{-|

Meaning:
Back vowels are [+back].
Central vowels are [+back].
Front vowels are [-back].
All other segments are undefined for [+/-back].
|-}
back :: Phonet -> Maybe PhonemeFeature
back (Vowel _ Back _ _) = Just (BackFeature Plus)
back (Vowel _ Central _ _) = Just (BackFeature Plus)
back (Vowel _ Front _ _) = Just (BackFeature Minus)
back _ = Nothing


{-|

Meaning of source code:
Rounded vowels are [+round].
All other vowels are [-round].
All other segments are [-round].
|-}
round :: Phonet -> Maybe PhonemeFeature
round (Vowel _ _ Rounded _) = Just (RoundFeature Plus)
round (Vowel _ _ _ _) = Just (RoundFeature Minus)
round _ = Just (RoundFeature Minus)

atr :: Phonet -> Maybe PhonemeFeature
atr (Vowel  Close     Front   Unrounded Voiced) = Just (AdvancedTongueRootFeature Plus)
atr (Vowel  CloseMid  Front   Unrounded Voiced) = Just (AdvancedTongueRootFeature Plus)
atr (Vowel  Close     Back    Rounded   Voiced) = Just (AdvancedTongueRootFeature Plus)
atr (Vowel  CloseMid  Front   Rounded   Voiced) = Just (AdvancedTongueRootFeature Plus)
atr (Vowel  CloseMid  Back    Rounded   Voiced) = Just (AdvancedTongueRootFeature Plus)
atr (Vowel  Close     Front   Rounded   Voiced) = Just (AdvancedTongueRootFeature Plus)
atr (Vowel  NearOpen  Front   Unrounded Voiced) = Just (AdvancedTongueRootFeature Minus)
atr (Vowel  Open      Back    Unrounded Voiced) = Just (AdvancedTongueRootFeature Minus)
atr (Vowel  Close     Central Unrounded Voiced) = Just (AdvancedTongueRootFeature Minus)
atr (Vowel  OpenMid   Back    Unrounded Voiced) = Just (AdvancedTongueRootFeature Minus)
atr (Vowel  NearClose Front   Unrounded Voiced) = Just (AdvancedTongueRootFeature Minus)
atr (Vowel  NearClose Back    Rounded   Voiced) = Just (AdvancedTongueRootFeature Minus)
atr (Vowel  OpenMid   Front   Unrounded Voiced) = Just (AdvancedTongueRootFeature Minus)
atr (Vowel  OpenMid   Back    Rounded   Voiced) = Just (AdvancedTongueRootFeature Minus)
atr _ = Nothing


toTextFeatures :: Phonet -> String
toTextFeatures phonete =
  let  features :: [Maybe PhonemeFeature]
       features = [ consonantal phonete
                  , syllabic phonete
                  , continuant phonete
                  , sonorant phonete
                  , delayedRelease phonete
                  , anterior phonete
                  , distributed phonete
                  , strident phonete
                  , high phonete
                  , low phonete
                  , nasal phonete
                  , labial phonete
                  , coronal phonete
                  , dorsal phonete
                  , pharyngeal phonete
                  , laryngeal phonete
                  , back phonete
                  , PhonemeFeature.round phonete
                  , atr phonete
                  , spreadGlottis phonete
                  , constrictedGlottis phonete
                  ]
  in "[" ++ intercalate "; " (map show (dejustifyList features)) ++ "]"
