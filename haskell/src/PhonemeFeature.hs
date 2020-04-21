{-# LANGUAGE UnicodeSyntax #-}

module PhonemeFeature (toTextFeatures, analyzeFeatures, showFeatures, difference,


                       isGlide, low, anterior,
                       distributed,
                       strident, high,
                       nasal,
                       labial, coronal,
                       dorsal,
                       pharyngeal, laryngeal,
                       round, atr, back,
                       PhonemeFeature(NasalFeature, LateralFeature, 
                       DelayedReleaseFeature,
                       SpreadGlottisFeature, ConstrictedGlottisFeature,
                       LabialFeature, CoronalFeature, DorsalFeature,
                       PharyngealFeature, LaryngealFeature,
                       RoundFeature,
                       AdvancedTongueRootFeature, HighFeature,
                       LowFeature, BackFeature,
                       StridentFeature, DistributedFeature,
                       AnteriorFeature),
                       Polarity(Minus, Plus)
                      ) where


-- Design decisions:
-- This module should not depend on any module
-- that is specifically for representing graphemes.
-- For this reason there should be no import of
-- a module that represents the International Phonetic
-- Alphabet. The reason for this design decision, is
-- that we want to be able to easily add other
-- alphabets or modifications to the IPA
-- without editing this module.


-- Go to Section 12.2 of the textbook to understand
-- the concept of phonological features.


import Prelude
  (
    Bool(False, True), Maybe(Just, Nothing), Show, String, Eq,
    elem, filter, head, length, map, null, not, notElem, otherwise, show,
    (>)
  )

import Prelude.Unicode
  ( (≡), (⧺), (∧), (∨)
  )

import Lib_Types


import Data.List (intercalate)
import Data.Maybe (catMaybes)


{-|
 Represents the '+' (plus) or '-' (minus)
 of a binary feature. e.g. [+sonorant],
 [-sonorant]
|-}
data Polarity = Plus | Minus
                deriving Eq


instance Show Polarity where
  show Plus = "+"
  show Minus = "-"



-- TODO: Determine if the "phoneme feature"
-- is the term used by linguists for the concept
-- being modeled in this module.


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
 ⎡ +voice    ⎤
 ⎢ +sonorant ⎥
 ⎣  nasal    ⎦

Note that certain sets of features cannot coexist,
syntactically. For example a phoneme cannot be
[+voice] and [-voice].

TODO: implement checking whether a set of phonemes
contains non-existant pairs (+ and − for the same
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


instance Show PhonemeFeature where
  show (SyllabicFeature polarity)           = show polarity ⧺ "syllabic"
  show (ConsonantalFeature polarity)        = show polarity ⧺ "consonantal"
  show (SonorantFeature polarity)           = show polarity ⧺ "sonorant"
  show (ContinuantFeature polarity)         = show polarity ⧺ "continuant"
  show (VoiceFeature polarity)              = show polarity ⧺ "voice"
  show (AdvancedTongueRootFeature polarity) = show polarity ⧺ "ATR"
  show NasalFeature                         =                  "nasal"
  show LateralFeature                       =                  "lateral"
  show DelayedReleaseFeature                =                  "delayed release"
  show SpreadGlottisFeature                 =                  "spread glottis"
  show ConstrictedGlottisFeature            =                  "constricted glottis"
  show LabialFeature                        =                  "labial"
  show CoronalFeature                       =                  "coronal"
  show DorsalFeature                        =                  "dorsal"
  show PharyngealFeature                    =                  "pharyngeal"
  show LaryngealFeature                     =                  "laryngeal"
  show (RoundFeature polarity)              = show polarity ⧺ "round"
  show (AnteriorFeature polarity)           = show polarity ⧺ "anterior"
  show (DistributedFeature polarity)        = show polarity ⧺ "distributed"
  show (StridentFeature polarity)           = show polarity ⧺ "strident"
  show (HighFeature polarity)               = show polarity ⧺ "high"
  show (LowFeature polarity)                = show polarity ⧺ "low"
  show (BackFeature polarity)               = show polarity ⧺ "back"


-- Given a binary feature, and another feature.
-- returns whether they are the same kind of feature.
-- They don't have to be the same polarity.
-- For example, [+voice] and [−voice] are mutually relevant features.
--   As are [+sonorant] and [+sonorant].
--   But [+sonorant] and [+voice] are not relevant because 
-- "voice" and "sonorant" are different.
relevantBinary ∷ (Polarity → PhonemeFeature) → PhonemeFeature → Bool
relevantBinary feature otherFeature = 
  otherFeature ≡ feature Plus ∨ otherFeature ≡ feature Minus

binaryDifference ::
          (Polarity → PhonemeFeature) 
                    → [PhonemeFeature] 
                    → [PhonemeFeature] 
                    → (Maybe PhonemeFeature, Maybe PhonemeFeature)
binaryDifference feature list1 list2
  | null list1Relevant ∧ null list2Relevant
       ∨ (not (null list1Relevant) ∧ not (null list2Relevant) 
       ∧ head list1Relevant ≡ head list2Relevant)
  = (Nothing, Nothing)
  | not (null list1Relevant) ∧ not (null list2Relevant)
  = (Just (head list1Relevant), Just (head list2Relevant))
  | length list1Relevant > length list2Relevant
  = (Just (head list1Relevant), Nothing)
  | otherwise
  = (Nothing, Just (head list2Relevant))
   where
   list1Relevant = filter (relevantBinary feature) list1
   list2Relevant = filter (relevantBinary feature) list2


unaryDifference ∷ PhonemeFeature
                         → [PhonemeFeature] 
                         → [PhonemeFeature] 
                         → (Maybe PhonemeFeature, Maybe PhonemeFeature)
unaryDifference feature list1 list2
  | elem feature list1 ≡ elem feature list2    = (Nothing, Nothing)
  | elem feature list1 ∧ notElem feature list2 = (Just feature, Nothing)
  | otherwise                                   = (Nothing, Just feature)



-- | This function takes two lists of phoneme features
-- and returns how they differ. Any phonemic
-- feature present in one list, and absent in the other
-- will be represented; and any phonemic
-- feature that is positive in one list but absent
-- in the other will be represented.
difference ∷ [PhonemeFeature]
           → [PhonemeFeature]
           → [(Maybe PhonemeFeature, Maybe PhonemeFeature)]
difference list1 list2 =
  [ binaryDifference SyllabicFeature           list1 list2
  , binaryDifference ConsonantalFeature        list1 list2
  , binaryDifference SonorantFeature           list1 list2
  , binaryDifference ContinuantFeature         list1 list2
  , binaryDifference VoiceFeature              list1 list2
  , binaryDifference AdvancedTongueRootFeature list1 list2
  , unaryDifference  NasalFeature              list1 list2
  , unaryDifference  LateralFeature            list1 list2
  , unaryDifference  DelayedReleaseFeature     list1 list2
  , unaryDifference  SpreadGlottisFeature      list1 list2
  , unaryDifference  ConstrictedGlottisFeature list1 list2
  , unaryDifference  LabialFeature             list1 list2
  , unaryDifference  CoronalFeature            list1 list2
  , unaryDifference  DorsalFeature             list1 list2
  , unaryDifference  PharyngealFeature         list1 list2
  , unaryDifference  LaryngealFeature          list1 list2
  , binaryDifference RoundFeature              list1 list2
  , binaryDifference AnteriorFeature           list1 list2
  , binaryDifference DistributedFeature        list1 list2
  , binaryDifference StridentFeature           list1 list2
  , binaryDifference HighFeature               list1 list2
  , binaryDifference LowFeature                list1 list2
  , binaryDifference BackFeature               list1 list2
  ]

{-|
Vowels are [+syllabic]
Consonants (glides included) are [-syllabic].

(Source: page 258)
|-}
syllabic ∷ Phonet → Maybe PhonemeFeature
syllabic (Vowel     _ _ _ _) = Just (SyllabicFeature Plus)
syllabic (Consonant _ _ _ _) = Just (SyllabicFeature Minus)

{-|
Whether a segment is a glide.
|-}
isGlide ∷ Phonet → Bool
isGlide (Consonant _ Palatal       Approximant PulmonicEgressive) = True
isGlide (Consonant _ LabialVelar   Approximant PulmonicEgressive) = True
isGlide (Consonant _ LabialPalatal Approximant PulmonicEgressive) = True
isGlide (Consonant _ Velar         Approximant PulmonicEgressive) = True
isGlide _                                                         = False

{-|
Vowels are [-consonantal].
Glides are [-consonantal].
Consonants (that are not glides) are [+consonantal].

(Source: page 258)
|-}
consonantal ∷ Phonet → Maybe PhonemeFeature
consonantal (Vowel _ _ _ _) = Just (ConsonantalFeature Minus)
consonantal consonant@(Consonant _ _ _ _)
  | isGlide consonant = Just (ConsonantalFeature Minus)
  | otherwise         = Just (ConsonantalFeature Plus)


{-|
Oral stops are [-sonorant].
Affricates are [-sonorant].
Fricatives are [-sonorant].
Nasals are [+sonorant].
Approximants are [+sonorant].
Laterals are [+sonorant].
Vowels are [+sonorant].
Glides are [+sonorant].

(Source: page 258)
|-}
sonorant ∷ Phonet → Maybe PhonemeFeature
sonorant (Consonant _ _ Plosive     _) = Just (SonorantFeature Minus)
sonorant (Consonant _ _ Affricate   _) = Just (SonorantFeature Minus)
sonorant (Consonant _ _ Fricative   _) = Just (SonorantFeature Minus)
sonorant (Consonant _ _ Nasal       _) = Just (SonorantFeature Plus)
sonorant (Consonant _ _ Approximant _) = Just (SonorantFeature Plus)
sonorant (Consonant _ _ Lateral     _) = Just (SonorantFeature Plus)
sonorant (Vowel               _ _ _ _) = Just (SonorantFeature Plus)
sonorant consonant@(Consonant _ _ _ _)
  | isGlide consonant = Just (SonorantFeature Plus)
  | otherwise         = Just (SonorantFeature Minus)

{-|
Oral stops are [-continuant].
Nasals stops are [-continuant].
Affricates are [-continuant].
Fricatives are [+continuant].
Approximants are [+continuant].
Vowels are [+continuant].
Glides are [+continuant].

(Source: page 258)

  Aside: we do not define lateral approximants for [+/-continuant] because the
  textbook puts it in parentheses. Usually this means, it depends on the language
  under study or
  it depends on the linguist.
  Lateral approximants may be considered [+continuant]. (arguable) (see chart on page 259))

|-}
continuant ∷ Phonet → Maybe PhonemeFeature
continuant (Consonant _ _ Plosive            _) = Just (ContinuantFeature Minus)
continuant (Consonant _ _ Nasal              _) = Just (ContinuantFeature Minus)
continuant (Consonant _ _ Affricate          _) = Just (ContinuantFeature Minus)
continuant (Consonant _ _ Approximant        _) = Just (ContinuantFeature Plus)
continuant (Vowel     _ _ _                  _) = Just (ContinuantFeature Plus)
continuant consonant@(Consonant _ _ _ _)
  | isGlide consonant = Just (ContinuantFeature Plus)
  | otherwise         = Nothing

{-|
Nasal consonants are [nasal].
-- to do: add support for nasal vowels.
All other segments are not defined for [nasal].
|-}
nasal ∷ Phonet → Maybe PhonemeFeature
nasal (Consonant _ _ Nasal _) = Just NasalFeature
nasal _                       = Nothing


{-|
Lateral consonants are [lateral].
Lateral Approximant consonants are [lateral].
Lateral fricative consonants are [lateral].
Lateral flap consonants are [lateral].
All other segments are not defined for [lateral].
|-}
lateral ∷ Phonet → Maybe PhonemeFeature
lateral (Consonant _ _ Lateral            _) = Just LateralFeature
lateral (Consonant _ _ LateralApproximant _) = Just LateralFeature
lateral (Consonant _ _ LateralFricative   _) = Just LateralFeature
lateral (Consonant _ _ LateralFlap        _) = Just LateralFeature
lateral _                                    = Nothing

{-|
Affricates are [+delayed release].
All other segments are [-delayed release].

(Source: page 260)
|-}
delayedRelease ∷ Phonet → Maybe PhonemeFeature
delayedRelease (Consonant _ _ Affricate _) = Just DelayedReleaseFeature
delayedRelease _                           = Nothing


{-|
Bilabial consonants are [labial].
Labio-dental consonants are [labial].
All other segments are undefined for [labial].

(Source: page 264)
|-}
labial ∷ Phonet → Maybe PhonemeFeature
labial (Consonant _ Bilabial    _ _) = Just LabialFeature
labial (Consonant _ LabioDental _ _) = Just LabialFeature
labial _                             = Nothing


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
coronal ∷ Phonet → Maybe PhonemeFeature
coronal (Consonant _ Dental         _ _) = Just CoronalFeature
coronal (Consonant _ Alveolar       _ _) = Just CoronalFeature
coronal (Consonant _ AlveoloPalatal _ _) = Just CoronalFeature
coronal (Consonant _ Retroflex      _ _) = Just CoronalFeature
coronal (Consonant _ Palatal        _ _) = Just CoronalFeature

coronal (Consonant _ PostAlveolar   _ _) = Just CoronalFeature

coronal _                                = Nothing


{-|
Palatals are [dorsal].

  Aside: alveolo-palatals do not seem to be dorsals,
  although the table 12.4 is confusing
  because it uses the IPA symbol for one.
  TODO: find more information on whether
  alveolo-palatals are [dorsal].

Velars are [dorsal].
Uvulars are [dorsal].
All other segments are undefined for [dorsal].
|-}
dorsal ∷ Phonet → Maybe PhonemeFeature
dorsal (Consonant _ Palatal        _ _) = Just DorsalFeature
dorsal (Consonant _ Velar          _ _) = Just DorsalFeature
dorsal (Consonant _ Uvular         _ _) = Just DorsalFeature
dorsal _                                = Nothing


{-|
Pharyngeal fricatives are [pharyngeal].
All other segments are undefined for [pharyngeal].

(Source: page 264)
|-}
pharyngeal ∷ Phonet → Maybe PhonemeFeature
pharyngeal (Consonant _ Pharyngeal Fricative _) = Just PharyngealFeature
pharyngeal _                                    = Nothing

{-|
Glottal consonants are [laryngeal].
All other segments are undefined for [laryngeal].

(Source: page 265)
|-}
laryngeal ∷ Phonet → Maybe PhonemeFeature
laryngeal (Consonant _ Glottal _ _ ) = Just LaryngealFeature
laryngeal _                          = Nothing


{-|
Voiced Aspirated consonants are [+voice].
Voiced consonants are [+voice].
Voiced vowels are [+voice].
All other segments are [-voice].
|-}
voice ∷ Phonet → Maybe PhonemeFeature
voice (Consonant Voiceless Glottal Plosive PulmonicEgressive) = Just (VoiceFeature Minus)
-- The voiceless glottal plosive is [-voice]
voice (Consonant VoicedAspirated _ _ _)  = Just (VoiceFeature Plus)
voice (Consonant Voiced          _ _ _)  = Just (VoiceFeature Plus)
voice (Vowel _ _ _               Voiced) = Just (VoiceFeature Plus)
voice _                                  = Just (VoiceFeature Minus)

{-|
Voiceless aspirated plosives are [spread glottis].
Voiced aspirated plosives are [spread glottis].
All other segments are not defined for [spread glottis].
(Source: page 262)
|-}
spreadGlottis ∷ Phonet → Maybe PhonemeFeature
spreadGlottis (Consonant VoicelessAspirated _ Plosive _) = Just SpreadGlottisFeature
spreadGlottis (Consonant VoicedAspirated    _ Plosive _) = Just SpreadGlottisFeature
spreadGlottis _                                          = Nothing


{-|
Ejectives have the feature [constricted glottis].
Glottal stop have the feature [constricted glottis].
Creaky voiced sonorants have the feature [constricted glottis].

(Source: page 262)
|-}
constrictedGlottis ∷ Phonet → Maybe PhonemeFeature
constrictedGlottis (Consonant _ Glottal Plosive _) =
  Just ConstrictedGlottisFeature
constrictedGlottis consonant@(Consonant CreakyVoiced _ _ _) =
  if sonorant consonant ≡ Just (SonorantFeature Plus)
    then Just ConstrictedGlottisFeature
    else Nothing
constrictedGlottis vowel@(Vowel _ _ _ CreakyVoiced) =
  if sonorant vowel ≡ Just (SonorantFeature Plus)
    then Just ConstrictedGlottisFeature
    else Nothing
constrictedGlottis _  = Nothing

{-|
Dentals are [+anterior].
Alveolars are [+anterior].
Post-alveolars are [-anterior].
Retroflexes are [-anterior].
Palatals are [-anterior].

(Source: page 265)

TODO: answer the question:
Question: Are Alveolo-palatals [+anterior], or [-anterior]?
Alveolo-palatals are [-anterior].
(SOURCE: not found)

|-}
anterior ∷ Phonet → Maybe PhonemeFeature
anterior (Consonant _ Dental            _ _) = Just (AnteriorFeature Plus)
anterior (Consonant _ Alveolar          _ _) = Just (AnteriorFeature Plus)
anterior (Consonant _ PostAlveolar      _ _) = Just (AnteriorFeature Minus)
anterior (Consonant _ Retroflex         _ _) = Just (AnteriorFeature Minus)
anterior (Consonant _ Palatal           _ _) = Just (AnteriorFeature Minus)
anterior (Consonant _ AlveoloPalatal    _ _) = Just (AnteriorFeature Minus)
anterior _                                   = Nothing

distributed ∷ Phonet → Maybe PhonemeFeature
distributed (Consonant _ Dental         _ _) = Just (DistributedFeature Plus)
distributed (Consonant _ Alveolar       _ _) = Just (DistributedFeature Minus)
distributed (Consonant _ PostAlveolar   _ _) = Just (DistributedFeature Plus)
distributed (Consonant _ Retroflex      _ _) = Just (DistributedFeature Minus)
distributed (Consonant _ Palatal        _ _) = Just (DistributedFeature Plus)
distributed (Consonant _ AlveoloPalatal _ _) = Just (DistributedFeature Plus)
distributed _                                = Nothing


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

(Source: page 266, under [+/-strident] heading, under the subsection "Natural classes".)
|-}
strident ∷ Phonet → Maybe PhonemeFeature
strident (Consonant _ Alveolar     Fricative _) = Just (StridentFeature Plus)
strident (Consonant _ Alveolar     Affricate _) = Just (StridentFeature Plus)
strident (Consonant _ PostAlveolar Fricative _) = Just (StridentFeature Plus)
strident (Consonant _ PostAlveolar Affricate _) = Just (StridentFeature Plus)
strident (Consonant _ LabioDental  Fricative _) = Just (StridentFeature Plus)
strident (Consonant _ LabioDental  Affricate _) = Just (StridentFeature Plus)
strident (Consonant _ Uvular       Fricative _) = Just (StridentFeature Plus)
strident (Consonant _ Uvular       Affricate _) = Just (StridentFeature Plus)

strident (Consonant _ _            Fricative _) = Just (StridentFeature Minus)
strident (Consonant _ _            Affricate _) = Just (StridentFeature Minus)

strident _                                      = Nothing


{-|
Palatal consonants are [+high].
Alveolo-palatal consonants are [+high].
Velar consonants are [+high].

Uvular consonants are [-high].
All other consonants are undefined for [+/-high].
Close vowels are [+high].
Near-close vowels are [+high].
All other vowels are [-high].
|-}
high ∷ Phonet → Maybe PhonemeFeature
high (Consonant _ Palatal        _ _) = Just (HighFeature Plus)
high (Consonant _ AlveoloPalatal _ _) = Just (HighFeature Plus)
high (Consonant _ Velar          _ _) = Just (HighFeature Plus)
high (Consonant _ Uvular         _ _) = Just (HighFeature Minus)
high (Consonant _ _              _ _) = Nothing
high (Vowel Close              _ _ _) = Just (HighFeature Plus)
high (Vowel NearClose          _ _ _) = Just (HighFeature Plus)
high (Vowel _                  _ _ _) = Just (HighFeature Minus)


{-|
Uvular consonants are [+low].
Pharyngeal consonants are [+low].
Glottal consonants are [+low].
All other consonants are undefined for [+/-low].
Open vowels are [+low].
Near open vowels are [+low].
All other vowels are [-low].
|-}
low ∷ Phonet → Maybe PhonemeFeature
low (Consonant _ Uvular     _ _) = Just (LowFeature Plus)
low (Consonant _ Pharyngeal _ _) = Just (LowFeature Plus)
low (Consonant _ Glottal    _ _) = Just (LowFeature Plus)
low (Consonant _ _          _ _) = Nothing
low (Vowel Open _           _ _) = Just (LowFeature Plus)
low (Vowel NearOpen _       _ _) = Just (LowFeature Plus)
low (Vowel _ _              _ _) = Just (LowFeature Minus)


{-|
Back vowels are [+back].
Central vowels are [+back].
Front vowels are [-back].
All other segments are undefined for [+/-back].
|-}
back ∷ Phonet → Maybe PhonemeFeature
back (Vowel _ Back    _ _) = Just (BackFeature Plus)
back (Vowel _ Central _ _) = Just (BackFeature Plus)
back (Vowel _ Front   _ _) = Just (BackFeature Minus)
back _                     = Nothing


{-|
Rounded vowels are [+round].
All other vowels are [-round].
All other segments are [-round].
|-}
round ∷ Phonet → Maybe PhonemeFeature
round (Vowel _ _ Rounded _) = Just (RoundFeature Plus)
round (Vowel _ _ _       _) = Just (RoundFeature Minus)
round _                     = Just (RoundFeature Minus)

{-|
Advanced tongue root
|-}
atr ∷ Phonet → Maybe PhonemeFeature
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
atr _                                           = Nothing


{-|
Given a phoneme (representation)
Gives a feature matrix.

Note: to non-linguists, feature matrices
are 1-dimensional, always displayed
as a single column.

For example:
/p/

  |-}
featureMatrix ∷ Phonet → [Maybe PhonemeFeature]
featureMatrix phonete
  = [ consonantal          phonete
    , syllabic             phonete
    , continuant           phonete
    , sonorant             phonete
    , delayedRelease       phonete
    , anterior             phonete
    , distributed          phonete
    , strident             phonete
    , high                 phonete
    , low                  phonete
    , nasal                phonete
    , lateral              phonete
    , labial               phonete
    , coronal              phonete
    , dorsal               phonete
    , pharyngeal           phonete
    , laryngeal            phonete
    , back                 phonete
    , round                phonete
    , voice                phonete
    , atr                  phonete
    , spreadGlottis        phonete
    , constrictedGlottis   phonete
    ]


-- | A function that takes data representing
-- how a phoneme is pronounced, and returns
-- a list of phonemic features.
analyzeFeatures ∷ Phonet → [PhonemeFeature]
analyzeFeatures phonete =
  catMaybes (featureMatrix phonete)

showFeatures ∷ [PhonemeFeature] → String
showFeatures features =
  let featuresStrings = map show features
  in "[" ⧺ intercalate "; " featuresStrings ⧺ "]"

toTextFeatures ∷ Phonet → String
toTextFeatures phonete =
  let features = analyzeFeatures phonete
  in showFeatures features

