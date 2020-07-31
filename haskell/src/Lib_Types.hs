{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies #-}

module Lib_Types where
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
                        SecondaryArticulation
            | Vowel Height
                    Backness
                    Rounding
                    VocalFolds
                    VowelLength
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
      UnmarkableSecondaryArticulation
  | UnmarkableVowel
      UnmarkableHeight
      UnmarkableBackness
      UnmarkableRounding
      UnmarkableVocalFolds
      UnmarkableVowelLength



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

data UnmarkableVowelLength
  = UnmarkedVowelLength
  | MarkedVowelLength VowelLength

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

data UnmarkableSecondaryArticulation
  = UnmarkedSecondaryArticulation | MarkedSecondaryArticulation SecondaryArticulation

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

data SecondaryArticulation
  = Normal | Labialized | Palatalized | Velarized | Pharyngealized
    deriving stock Eq

secondaryArticulationStates :: NonEmpty SecondaryArticulation
secondaryArticulationStates
  = fromList
    [ Labialized
    , Palatalized
    , Velarized
    , Pharyngealized
    ]

data VowelLength
  = NormalLength | Long | HalfLong | ExtraShort
    deriving stock Eq

vowelLengthStates :: NonEmpty VowelLength
vowelLengthStates
  = fromList
  [ NormalLength
  , Long
  , HalfLong
  , ExtraShort
  ]