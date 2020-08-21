module Lib_Types where

import Prelude (Eq)
import Data.List.NonEmpty (NonEmpty, fromList)

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
                    deriving Eq

data Backness = Front
              | Central
              | Back
                deriving Eq

backnessStates :: NonEmpty Backness
backnessStates = fromList [Front, Central, Back]


data Height = Close
            | NearClose
            | CloseMid
            | Mid
            | OpenMid
            | NearOpen
            | Open
              deriving Eq

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
                deriving Eq

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
           deriving Eq


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
              deriving Eq


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
                 deriving Eq


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
                  deriving Eq

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
    deriving Eq

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
    deriving Eq

vowelLengthStates :: NonEmpty VowelLength
vowelLengthStates
  = fromList
  [ NormalLength
  , Long
  , HalfLong
  , ExtraShort
  ]