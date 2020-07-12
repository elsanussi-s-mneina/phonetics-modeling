
{-# LANGUAGE DerivingStrategies #-}

module Lib_Types where
import           Prelude ()
import           Relude  (Eq, NonEmpty, fromList)

-- | The data type Phonet, represents a linguistics
-- phoneme or phonete.
-- It can be a consonant, or a vowel.
-- A consonant is specified by
--    the configuration of the vocal folds,
--    the place of articulation,
--    the manner of articulation, and
--    an airstream mechanism.
-- A vowel is specified by
--    the height   (height of the tongue),
--    the backness (how far back in the mouth),
--    the rounding (rounding of lips), and
--    the configuration of the vocal folds.
-- Note: The Phonet datatype does not represent
-- any marked/unmarked distinction.
data Phonet = Consonant VocalFolds
                        Place   -- ^ Place of articulation
                        Manner  -- ^ Manner of articulation
                        Airstream

            | Vowel Height
                    Backness
                    Rounding
                    VocalFolds
                    deriving stock Eq

-- | The data type UnmarkablePhonet was originally intended
-- to represent a phoneme, or a phonete but with the additional
-- ability to have unmarked properties, such as unmarked voicing.
-- So far, though it has not been used in a way consistent with
-- linguistics, instead it has been used to represent all values of a property.
-- So for example, we would use unmarked voicing to represent all
-- possible vocal fold configurations. We would use unmarked place to
-- to represent all possible places of articulation.
data UnmarkablePhonet
  = UnmarkableConsonant
      UnmarkableVocalFolds
      UnmarkablePlace
      UnmarkableManner
      UnmarkableAirstream
  | UnmarkableVowel
      UnmarkableHeight
      UnmarkableBackness
      UnmarkableRounding
      UnmarkableVocalFolds



data Backness = Front
              | Central
              | Back
                deriving stock Eq


backnessStates :: NonEmpty Backness
backnessStates = fromList [Front, Central, Back]


data UnmarkableBackness
  = UnmarkedBackness
  | MarkedBackness Backness



data Height = Close
            | NearClose
            | CloseMid
            | Mid
            | OpenMid
            | NearOpen
            | Open
              deriving stock Eq

data UnmarkableHeight
  = UnmarkedHeight | MarkedHeight Height



heightStates :: NonEmpty Height
heightStates = fromList
             [ Close
             , NearClose
             , CloseMid
             , Mid
             , OpenMid
             , NearOpen
             , Open
             ]


data Rounding = Rounded
              | Unrounded
                deriving stock Eq

data UnmarkableRounding
  = UnmarkedRounding
  | MarkedRounding Rounding



roundingStates :: NonEmpty Rounding
roundingStates = fromList [Rounded, Unrounded]

data Place = Bilabial
           | LabioDental
           | Dental
           | Alveolar
           | PostAlveolar
           | Retroflex
           | Palatal
           | Velar
           | Uvular
           | Pharyngeal
           | Glottal
           | Epiglottal
           -- I am unsure if the following three should be counted
           -- as 6 different places of articulation, or just 3
           | LabialVelar
           | LabialPalatal
           | AlveoloPalatal
           | PalatoAlveolar  -- To do: investigate what the difference
           -- is between alveolopalatal, and palatoalveolar
           | Places (NonEmpty Place)
           deriving stock Eq

data UnmarkablePlace
  = UnmarkedPlace
  | MarkedPlace Place



placeStates :: NonEmpty Place
placeStates = fromList
              [ Bilabial
              , LabioDental
              , Dental
              , Alveolar
              , PostAlveolar
              , Retroflex
              , Palatal
              , Velar
              , Uvular
              , Pharyngeal
              , Glottal
              , Epiglottal
              , LabialVelar
              , LabialPalatal
              , AlveoloPalatal
              , PalatoAlveolar
              ]



data Manner = Plosive
            | Nasal
            | Trill
            | TapOrFlap
            | Approximant
            | Fricative
            | Affricate
            | LateralFricative
            | LateralApproximant
            | LateralFlap  -- ^ There are very few IPA symbols for lateral flaps
            | Lateral      -- ^ We need this one for the lateral click.
              deriving stock Eq

data UnmarkableManner
  = UnmarkedManner
  | MarkedManner Manner



mannerStates :: NonEmpty Manner
mannerStates = fromList
               [ Plosive
               , Nasal
               , Trill
               , TapOrFlap
               , Approximant
               , Fricative
               , Affricate
               , LateralFricative
               , LateralApproximant
               , LateralFlap
               , Lateral
               ]

data Airstream = PulmonicEgressive
               | Click
               | Implosive
                 deriving stock Eq

data UnmarkableAirstream
  = UnmarkedAirstream
  | MarkedAirstream Airstream



airstreamStates :: NonEmpty Airstream
airstreamStates = fromList
                  [ PulmonicEgressive
                  , Click
                  , Implosive
                  ]

data VocalFolds = Voiced
                | Voiceless
                | VoicedAspirated
                | VoicelessAspirated
                | CreakyVoiced
                  deriving stock Eq

data UnmarkableVocalFolds
  = UnmarkedVocalFolds | MarkedVocalFolds VocalFolds


vocalFoldStates :: NonEmpty VocalFolds
vocalFoldStates
  = fromList
    [ Voiceless
    , Voiced
    , VoicedAspirated
    , VoicelessAspirated
    , CreakyVoiced
    ]

newtype PhonetInventory = PhonetInventory (NonEmpty Phonet)


{-|
 Represents the '+' (plus) or '-' (minus)
 of a binary feature. e.g. [+sonorant],
 [-sonorant]
-}
data Polarity = Plus | Minus
                deriving stock Eq



{-|
 In Linguistics, phonemes can be
 analyzed as a set of features. One phoneme
 will have one set of features, and a different
 phoneme will have a different set of features.

 These features are well known in phonology, and
 are limited in number. There are two kinds of
 features, unary features, and binary features. The
 difference is obvious in how they are represented in
 the notation that linguists use. Unary features,
 are either present or absent. Binary features
 can be positive or negative. For example, Nasal
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

 Note that some analyses
are language specific, so for some phonemes (not
the usual case) whether it has feature X (say 'coronal')
depends on the language (theoretical example: e.g. Swahili,
vs French). This is not implemented here.
-}
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
                    deriving stock Eq
