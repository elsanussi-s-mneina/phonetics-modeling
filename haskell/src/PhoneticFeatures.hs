
{-# LANGUAGE OverloadedStrings #-}

module PhoneticFeatures where
import           Prelude ()
import           Relude (Bool(False, True), Text, Maybe(Just, Nothing), catMaybes, elem, filter, map, notElem, otherwise, (||), (==), (<>), (&&), (!!?))


import qualified Data.Text     as T
import PhoneticFeaturesTypes

import           Lib_Types

import PhoneticFeaturesTypes()
import EnglishUSText

showPolarity :: Polarity -> Text
showPolarity Plus  = "+"
showPolarity Minus = "-"

showPhonemeFeature :: PhonemeFeature -> Text
showPhonemeFeature pf =
  case pf of
    (SyllabicFeature p) -> showPolarity p <> syllabicPhonemeFeatureUIText
    (ConsonantalFeature p) -> showPolarity p <> consonantalPhonemeFeatureUIText
    (SonorantFeature p) -> showPolarity p <> sonorantPhonemeFeatureUIText
    (ContinuantFeature p) -> showPolarity p <> continuantPhonemeFeatureUIText
    (VoiceFeature p) -> showPolarity p <> voicePhonemeFeatureUIText
    (AdvancedTongueRootFeature p) -> showPolarity p <> atrPhonemeFeatureUIText
    NasalFeature -> nasalPhonemeFeatureUIText
    LateralFeature -> lateralPhonemeFeatureUIText
    DelayedReleaseFeature -> delayedReleasePhonemeFeatureUIText
    SpreadGlottisFeature -> spreadGlottisPhonemeFeatureUIText
    ConstrictedGlottisFeature -> constrictedGlottisPhonemeFeatureUIText
    LabialFeature -> labialPhonemeFeatureUIText
    CoronalFeature -> coronalPhonemeFeatureUIText
    DorsalFeature -> dorsalPhonemeFeatureUIText
    PharyngealFeature -> pharyngealPhonemeFeatureUIText
    LaryngealFeature -> laryngealPhonemeFeatureUIText
    (RoundFeature p) -> showPolarity p <> roundPhonemeFeatureUIText
    (AnteriorFeature p) -> showPolarity p <> anteriorPhonemeFeatureUIText
    (DistributedFeature p) -> showPolarity p <> distributedPhonemeFeatureUIText
    (StridentFeature p) -> showPolarity p <> stridentPhonemeFeatureUIText
    (HighFeature p) -> showPolarity p <> highPhonemeFeatureUIText
    (LowFeature p) -> showPolarity p <> lowPhonemeFeatureUIText
    (BackFeature p) -> showPolarity p <> backPhonemeFeatureUIText


-- |
-- Vowels are [-consonantal].
-- Glides are [-consonantal].
-- Consonants (that are not glides) are [+consonantal].
--
-- (Source: page 258)
consonantal :: Phonet -> Maybe PhonemeFeature
consonantal p = case p of
  Vowel {} -> Just (ConsonantalFeature Minus)
  Consonant {}
    | isGlide p -> Just (ConsonantalFeature Minus)
    | otherwise -> Just (ConsonantalFeature Plus)

-- |
-- Oral stops are [-sonorant].
-- Affricates are [-sonorant].
-- Fricatives are [-sonorant].
-- Nasals are [+sonorant].
-- Approximants are [+sonorant].
-- Laterals are [+sonorant].
-- Vowels are [+sonorant].
-- Glides are [+sonorant].
--
-- (Source: page 258)
sonorant :: Phonet -> Maybe PhonemeFeature
sonorant p = case p of
  (Consonant _ _ Plosive _) -> Just (SonorantFeature Minus)
  (Consonant _ _ Affricate _) -> Just (SonorantFeature Minus)
  (Consonant _ _ Fricative _) -> Just (SonorantFeature Minus)
  (Consonant _ _ Nasal _) -> Just (SonorantFeature Plus)
  (Consonant _ _ Approximant _) -> Just (SonorantFeature Plus)
  (Consonant _ _ Lateral _) -> Just (SonorantFeature Plus)
  Vowel {} -> Just (SonorantFeature Plus)
  Consonant {}
    | isGlide p -> Just (SonorantFeature Plus)
    | otherwise -> Just (SonorantFeature Minus)

-- |
-- Oral stops are [-continuant].
-- Nasals stops are [-continuant].
-- Affricates are [-continuant].
-- Fricatives are [+continuant].
-- Approximants are [+continuant].
-- Vowels are [+continuant].
-- Glides are [+continuant].
--
-- (Source: page 258)
--
--   Aside: we do not define lateral approximants for [+/-continuant] because the
--   textbook puts it in parentheses. Usually this means, it depends on
--   the language under study or
--   it depends on the linguist.
--   Lateral approximants may be considered [+continuant]. (arguable)
--   (see chart on page 259))
--
continuant :: Phonet -> Maybe PhonemeFeature
continuant p = case p of
  (Consonant _ _ Plosive _) -> Just (ContinuantFeature Minus)
  (Consonant _ _ Nasal _) -> Just (ContinuantFeature Minus)
  (Consonant _ _ Affricate _) -> Just (ContinuantFeature Minus)
  (Consonant _ _ Approximant _) -> Just (ContinuantFeature Plus)
  Vowel {} -> Just (ContinuantFeature Plus)
  Consonant {}
    | isGlide p -> Just (ContinuantFeature Plus)
    | otherwise -> Nothing

-- |
-- Nasal consonants are [nasal].
-- -- to do: add support for nasal vowels.
-- All other segments are not defined for [nasal].
nasal :: Phonet -> Maybe PhonemeFeature
nasal (Consonant _ _ Nasal _) = Just NasalFeature
nasal _                       = Nothing

-- |
-- Lateral consonants are [lateral].
-- Lateral approximant consonants are [lateral].
-- Lateral fricative consonants are [lateral].
-- Lateral flap consonants are [lateral].
-- All other segments are not defined for [lateral].
lateral :: Phonet -> Maybe PhonemeFeature
lateral p = case p of
  (Consonant _ _ Lateral _)            -> Just LateralFeature
  (Consonant _ _ LateralApproximant _) -> Just LateralFeature
  (Consonant _ _ LateralFricative _)   -> Just LateralFeature
  (Consonant _ _ LateralFlap _)        -> Just LateralFeature
  _                                    -> Nothing

-- |
-- Affricates are [+delayed release].
-- All other segments are [-delayed release].
--
-- (Source: page 260)
delayedRelease :: Phonet -> Maybe PhonemeFeature
delayedRelease (Consonant _ _ Affricate _) = Just DelayedReleaseFeature
delayedRelease _                           = Nothing

-- |
-- Bilabial consonants are [labial].
-- Labio-dental consonants are [labial].
-- All other segments are undefined for [labial].
--
-- (Source: page 264)
labial :: Phonet -> Maybe PhonemeFeature
labial p = case p of
  (Consonant _ Bilabial _ _)    -> Just LabialFeature
  (Consonant _ LabioDental _ _) -> Just LabialFeature
  _                             -> Nothing

-- |
-- Dentals are [coronal].
-- Alveolars are [coronal] also.
-- Alveolopalatals are [coronal] also.
-- Retroflexes are [coronal] also.
-- Palatals are [coronal] also.
--
-- Post-alveolars are [coronal] also.
--
-- All other sounds are undefined for [coronal].
--
-- (Source: page 264)
-- (The fact that Post-alveolar consonants are coronal is indicated by
--  Table 12. on page 265.)
coronal :: Phonet -> Maybe PhonemeFeature
coronal p = case p of
  (Consonant _ Dental _ _)         -> Just CoronalFeature
  (Consonant _ Alveolar _ _)       -> Just CoronalFeature
  (Consonant _ AlveoloPalatal _ _) -> Just CoronalFeature
  (Consonant _ Retroflex _ _)      -> Just CoronalFeature
  (Consonant _ Palatal _ _)        -> Just CoronalFeature
  (Consonant _ PostAlveolar _ _)   -> Just CoronalFeature
  _                                -> Nothing

-- |
-- Palatals are [dorsal].
--
--   Aside: alveolo-palatals do not seem to be dorsals,
--   although the table 12.4 is confusing
--   because it uses the IPA symbol for one.
--
-- Velars are [dorsal].
-- Uvulars are [dorsal].
-- All other segments are undefined for [dorsal].
dorsal :: Phonet -> Maybe PhonemeFeature
dorsal p = case p of
  (Consonant _ Palatal _ _) -> Just DorsalFeature
  (Consonant _ Velar _ _)   -> Just DorsalFeature
  (Consonant _ Uvular _ _)  -> Just DorsalFeature
  _                         -> Nothing

-- |
-- Pharyngeal fricatives are [pharyngeal].
-- All other segments are undefined for [pharyngeal].
--
-- (Source: page 264)
pharyngeal :: Phonet -> Maybe PhonemeFeature
pharyngeal (Consonant _ Pharyngeal Fricative _) = Just PharyngealFeature
pharyngeal _                                    = Nothing

-- |
-- Glottal consonants are [laryngeal].
-- All other segments are undefined for [laryngeal].
--
-- (Source: page 265)
laryngeal :: Phonet -> Maybe PhonemeFeature
laryngeal (Consonant _ Glottal _ _) = Just LaryngealFeature
laryngeal _                         = Nothing

-- |
-- Voiced Aspirated consonants are [+voice].
-- Voiced consonants are [+voice].
-- Voiced vowels are [+voice].
-- All other segments are [-voice].
voice :: Phonet -> Maybe PhonemeFeature
voice p = case p of
  (Consonant Voiceless Glottal Plosive PulmonicEgressive) ->
    Just (VoiceFeature Minus) -- The voiceless glottal plosive is [-voice]
  (Consonant VoicedAspirated _ _ _) ->
    Just (VoiceFeature Plus)
  (Consonant Voiced _ _ _) ->
    Just (VoiceFeature Plus)
  (Vowel _ _ _ Voiced) ->
    Just (VoiceFeature Plus)
  _ ->
    Just (VoiceFeature Minus)

-- |
-- Voiceless aspirated plosives are [spread glottis].
-- Voiced aspirated plosives are [spread glottis].
-- All other segments are not defined for [spread glottis].
-- (Source: page 262)
spreadGlottis :: Phonet -> Maybe PhonemeFeature
spreadGlottis p = case p of
  (Consonant VoicelessAspirated _ Plosive _) -> Just SpreadGlottisFeature
  (Consonant VoicedAspirated _ Plosive _)    -> Just SpreadGlottisFeature
  _                                          -> Nothing

-- |
-- Ejectives have the feature [constricted glottis].
-- Glottal stop have the feature [constricted glottis].
-- Creaky voiced sonorants have the feature [constricted glottis].
--
-- (Source: page 262)
constrictedGlottis :: Phonet -> Maybe PhonemeFeature
constrictedGlottis p = case p of
  (Consonant _ Glottal Plosive _) ->
    Just ConstrictedGlottisFeature
  (Consonant CreakyVoiced _ _ _) ->
    if sonorant p == Just (SonorantFeature Plus)
      then Just ConstrictedGlottisFeature
      else Nothing
  (Vowel _ _ _ CreakyVoiced) ->
    if sonorant p == Just (SonorantFeature Plus)
      then Just ConstrictedGlottisFeature
      else Nothing
  _ ->
    Nothing

-- |
-- Dentals are [+anterior].
-- Alveolars are [+anterior].
-- Post-alveolars are [-anterior].
-- Retroflexes are [-anterior].
-- Palatals are [-anterior].
--
-- (Source: page 265)
--
-- TODO: answer the question:
-- Question: Are Alveolo-palatals [+anterior], or [-anterior]?
-- Alveolo-palatals are [-anterior].
-- (SOURCE: not found)
anterior :: Phonet -> Maybe PhonemeFeature
anterior p = case p of
  (Consonant _ Dental _ _)         -> Just (AnteriorFeature Plus)
  (Consonant _ Alveolar _ _)       -> Just (AnteriorFeature Plus)
  (Consonant _ PostAlveolar _ _)   -> Just (AnteriorFeature Minus)
  (Consonant _ Retroflex _ _)      -> Just (AnteriorFeature Minus)
  (Consonant _ Palatal _ _)        -> Just (AnteriorFeature Minus)
  (Consonant _ AlveoloPalatal _ _) -> Just (AnteriorFeature Minus)
  _                                -> Nothing

distributed :: Phonet -> Maybe PhonemeFeature
distributed p = case p of
  (Consonant _ Dental _ _)         -> Just (DistributedFeature Plus)
  (Consonant _ Alveolar _ _)       -> Just (DistributedFeature Minus)
  (Consonant _ PostAlveolar _ _)   -> Just (DistributedFeature Plus)
  (Consonant _ Retroflex _ _)      -> Just (DistributedFeature Minus)
  (Consonant _ Palatal _ _)        -> Just (DistributedFeature Plus)
  (Consonant _ AlveoloPalatal _ _) -> Just (DistributedFeature Plus)
  _                                -> Nothing

-- |
-- Alveolar fricatives are [+strident].
-- Alveolar affricates are [+strident], also.
-- Post-alveolar fricatives are [+strident], also.
-- Post-alveolar affricates are [+strident], also.
-- Labio-dental fricatives are [+strident] , also.
-- Labio-dental affricates are [+strident] , also.
-- Uvular fricatives are [+strident], also.
-- Uvular affricates are [+strident], also.
--
-- All other fricatives are [-strident].
-- All other affricates are [-strident], also.
--
-- All other segments are undefined for [+/-strident].
--
-- (Source: page 266, under [+/-strident] heading, under the subsection
-- "Natural classes".)
strident :: Phonet -> Maybe PhonemeFeature
strident p = case p of
  (Consonant _ Alveolar Fricative _)     -> Just (StridentFeature Plus)
  (Consonant _ Alveolar Affricate _)     -> Just (StridentFeature Plus)
  (Consonant _ PostAlveolar Fricative _) -> Just (StridentFeature Plus)
  (Consonant _ PostAlveolar Affricate _) -> Just (StridentFeature Plus)
  (Consonant _ LabioDental Fricative _)  -> Just (StridentFeature Plus)
  (Consonant _ LabioDental Affricate _)  -> Just (StridentFeature Plus)
  (Consonant _ Uvular Fricative _)       -> Just (StridentFeature Plus)
  (Consonant _ Uvular Affricate _)       -> Just (StridentFeature Plus)
  (Consonant _ _ Fricative _)            -> Just (StridentFeature Minus)
  (Consonant _ _ Affricate _)            -> Just (StridentFeature Minus)
  _                                      -> Nothing

-- |
-- Palatal consonants are [+high].
-- Alveolo-palatal consonants are [+high].
-- Velar consonants are [+high].
--
-- Uvular consonants are [-high].
-- All other consonants are undefined for [+/-high].
-- Close vowels are [+high].
-- Near-close vowels are [+high].
-- All other vowels are [-high].
high :: Phonet -> Maybe PhonemeFeature
high p = case p of
  (Consonant _ Palatal _ _)        -> Just (HighFeature Plus)
  (Consonant _ AlveoloPalatal _ _) -> Just (HighFeature Plus)
  (Consonant _ Velar _ _)          -> Just (HighFeature Plus)
  (Consonant _ Uvular _ _)         -> Just (HighFeature Minus)
  Consonant {}                     -> Nothing
  (Vowel Close _ _ _)              -> Just (HighFeature Plus)
  (Vowel NearClose _ _ _)          -> Just (HighFeature Plus)
  Vowel {}                         -> Just (HighFeature Minus)

-- |
-- Uvular consonants are [+low].
-- Pharyngeal consonants are [+low].
-- Glottal consonants are [+low].
-- All other consonants are undefined for [+/-low].
-- Open vowels are [+low].
-- Near open vowels are [+low].
-- All other vowels are [-low].
low :: Phonet -> Maybe PhonemeFeature
low p = case p of
  (Consonant _ Uvular _ _)     -> Just (LowFeature Plus)
  (Consonant _ Pharyngeal _ _) -> Just (LowFeature Plus)
  (Consonant _ Glottal _ _)    -> Just (LowFeature Plus)
  Consonant {}                 -> Nothing
  (Vowel Open _ _ _)           -> Just (LowFeature Plus)
  (Vowel NearOpen _ _ _)       -> Just (LowFeature Plus)
  Vowel {}                     -> Just (LowFeature Minus)

-- |
-- Back vowels are [+back].
-- Central vowels are [+back].
-- Front vowels are [-back].
-- All other segments are undefined for [+/-back].
back :: Phonet -> Maybe PhonemeFeature
back p = case p of
  (Vowel _ Back _ _)    -> Just (BackFeature Plus)
  (Vowel _ Central _ _) -> Just (BackFeature Plus)
  (Vowel _ Front _ _)   -> Just (BackFeature Minus)
  _                     -> Nothing

-- |
-- Rounded vowels are [+round].
-- All other vowels are [-round].
-- All other segments are [-round].
lipRound :: Phonet -> Maybe PhonemeFeature
lipRound p = case p of
  (Vowel _ _ Rounded _) -> Just (RoundFeature Plus)
  Vowel {}              -> Just (RoundFeature Minus)
  _                     -> Just (RoundFeature Minus)

-- |
-- Advanced tongue root
atr :: Phonet -> Maybe PhonemeFeature
atr p = case p of
  (Vowel Close Front Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Plus)
  (Vowel CloseMid Front Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Plus)
  (Vowel Close Back Rounded Voiced) ->
    Just (AdvancedTongueRootFeature Plus)
  (Vowel CloseMid Front Rounded Voiced) ->
    Just (AdvancedTongueRootFeature Plus)
  (Vowel CloseMid Back Rounded Voiced) ->
    Just (AdvancedTongueRootFeature Plus)
  (Vowel Close Front Rounded Voiced) ->
    Just (AdvancedTongueRootFeature Plus)
  (Vowel NearOpen Front Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  (Vowel Open Back Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  (Vowel Close Central Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  (Vowel OpenMid Back Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  (Vowel NearClose Front Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  (Vowel NearClose Back Rounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  (Vowel OpenMid Front Unrounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  (Vowel OpenMid Back Rounded Voiced) ->
    Just (AdvancedTongueRootFeature Minus)
  _ ->
    Nothing


-- |
-- Given a phoneme (representation)
-- Gives a feature matrix.
--
-- Note: to non-linguists, feature matrices
-- are 1-dimensional, always displayed
-- as a single column.
--
-- For example:
-- /p/
featureMatrix :: Phonet -> [Maybe PhonemeFeature]
featureMatrix phonete =
  [ consonantal phonete,
    syllabic phonete,
    continuant phonete,
    sonorant phonete,
    delayedRelease phonete,
    anterior phonete,
    distributed phonete,
    strident phonete,
    high phonete,
    low phonete,
    nasal phonete,
    lateral phonete,
    labial phonete,
    coronal phonete,
    dorsal phonete,
    pharyngeal phonete,
    laryngeal phonete,
    back phonete,
    lipRound phonete,
    voice phonete,
    atr phonete,
    spreadGlottis phonete,
    constrictedGlottis phonete
  ]

-- | A function that takes data representing
-- how a phoneme is pronounced, and returns
-- a list of phonemic features.
analyzeFeatures :: Phonet -> [PhonemeFeature]
analyzeFeatures phonete =
  catMaybes (featureMatrix phonete)

showFeatures :: [PhonemeFeature] -> Text
showFeatures features =
  let featuresStrings :: [Text]
      featuresStrings = map showPhonemeFeature features
   in "[" <> T.intercalate "; " featuresStrings <> "]"

toTextFeatures :: Phonet -> Text
toTextFeatures phonete =
  let features = analyzeFeatures phonete
   in showFeatures features



-- |
-- Vowels are [+syllabic]
-- Consonants (glides included) are [-syllabic].
--
-- (Source: page 258)
--
syllabic :: Phonet -> Maybe PhonemeFeature
syllabic Vowel {}     = Just (SyllabicFeature Plus)
syllabic Consonant {} = Just (SyllabicFeature Minus)

-- |
-- Whether a segment is a glide.
isGlide :: Phonet -> Bool
isGlide p = case p of
  (Consonant _ Palatal Approximant PulmonicEgressive)       -> True
  (Consonant _ LabialVelar Approximant PulmonicEgressive)   -> True
  (Consonant _ LabialPalatal Approximant PulmonicEgressive) -> True
  (Consonant _ Velar Approximant PulmonicEgressive)         -> True
  _                                                         -> False


-- Go to Section 12.2 of the textbook to understand
-- the concept of phonological features.

-- Given a binary feature, and another feature.
-- returns whether they are the same kind of feature.
-- They don't have to be the same polarity.
-- For example, [+voice] and [−voice] are mutually relevant features.
--   As are [+sonorant] and [+sonorant].
--   But [+sonorant] and [+voice] are not relevant because
-- "voice" and "sonorant" are different.
relevantBinary :: (Polarity -> PhonemeFeature) -> PhonemeFeature -> Bool
relevantBinary feature otherFeature =
  otherFeature == feature Plus || otherFeature == feature Minus

binaryDifference ::
  (Polarity -> PhonemeFeature) ->
  [PhonemeFeature] ->
  [PhonemeFeature] ->
  (Maybe PhonemeFeature, Maybe PhonemeFeature)
binaryDifference feature list_1 list_2
  | relevantList_1 == relevantList_2 =
    (Nothing, Nothing)
  | otherwise =
    (relevantList_1 !!? 0, relevantList_2 !!? 0)
  where
    relevantList_1 = filter (relevantBinary feature) list_1
    relevantList_2 = filter (relevantBinary feature) list_2

unaryDifference ::
  PhonemeFeature ->
  [PhonemeFeature] ->
  [PhonemeFeature] ->
  (Maybe PhonemeFeature, Maybe PhonemeFeature)
unaryDifference feature list_1 list_2
  | (feature `elem` list_1) == (feature `elem` list_2) = (Nothing, Nothing)
  | feature `elem` list_1 && feature `notElem` list_2 = (Just feature, Nothing)
  | otherwise = (Nothing, Just feature)

-- | This function takes two lists of phoneme features
-- and returns how they differ. Any phonemic
-- feature present in one list, and absent in the other
-- will be represented; and any phonemic
-- feature that is positive in one list but absent
-- in the other will be represented.
difference ::
  [PhonemeFeature] ->
  [PhonemeFeature] ->
  [(Maybe PhonemeFeature, Maybe PhonemeFeature)]
difference list_1 list_2 =
  [ binaryDifference SyllabicFeature list_1 list_2,
    binaryDifference ConsonantalFeature list_1 list_2,
    binaryDifference SonorantFeature list_1 list_2,
    binaryDifference ContinuantFeature list_1 list_2,
    binaryDifference VoiceFeature list_1 list_2,
    binaryDifference AdvancedTongueRootFeature list_1 list_2,
    unaryDifference NasalFeature list_1 list_2,
    unaryDifference LateralFeature list_1 list_2,
    unaryDifference DelayedReleaseFeature list_1 list_2,
    unaryDifference SpreadGlottisFeature list_1 list_2,
    unaryDifference ConstrictedGlottisFeature list_1 list_2,
    unaryDifference LabialFeature list_1 list_2,
    unaryDifference CoronalFeature list_1 list_2,
    unaryDifference DorsalFeature list_1 list_2,
    unaryDifference PharyngealFeature list_1 list_2,
    unaryDifference LaryngealFeature list_1 list_2,
    binaryDifference RoundFeature list_1 list_2,
    binaryDifference AnteriorFeature list_1 list_2,
    binaryDifference DistributedFeature list_1 list_2,
    binaryDifference StridentFeature list_1 list_2,
    binaryDifference HighFeature list_1 list_2,
    binaryDifference LowFeature list_1 list_2,
    binaryDifference BackFeature list_1 list_2
  ]
