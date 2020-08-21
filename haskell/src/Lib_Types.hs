{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}
module Lib_Types where
import           Relude  (Eq, Int, Hashable, hashWithSalt, NonEmpty, fromList)

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


instance Hashable Phonet where
    hashWithSalt salt (Consonant vf _ m _ _) = hashWithSalt (hashWithSalt salt m) vf
    hashWithSalt salt (Vowel _ b _ vf _)     = hashWithSalt (hashWithSalt salt b) vf


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

instance Hashable Backness where
  hashWithSalt s Front   = s `hashWithSalt` (0 :: Int)
  hashWithSalt s Central = s `hashWithSalt` (1 :: Int)
  hashWithSalt s Back    = s `hashWithSalt` (2 :: Int)


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

instance Hashable Height where
  hashWithSalt s Close     = s `hashWithSalt` (1 :: Int)
  hashWithSalt s NearClose = s `hashWithSalt` (2 :: Int)
  hashWithSalt s CloseMid  = s `hashWithSalt` (3 :: Int)
  hashWithSalt s Mid       = s `hashWithSalt` (4 :: Int)
  hashWithSalt s OpenMid   = s `hashWithSalt` (5 :: Int)
  hashWithSalt s NearOpen  = s `hashWithSalt` (6 :: Int)
  hashWithSalt s Open      = s `hashWithSalt` (7 :: Int)

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

instance Hashable Rounding where
  hashWithSalt s Rounded   = s `hashWithSalt` (1 :: Int)
  hashWithSalt s Unrounded = s `hashWithSalt` (2 :: Int)

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

instance Hashable Manner where
  hashWithSalt s Plosive            = s `hashWithSalt` (0  :: Int)
  hashWithSalt s Nasal              = s `hashWithSalt` (1  :: Int)
  hashWithSalt s Trill              = s `hashWithSalt` (2  :: Int)
  hashWithSalt s TapOrFlap          = s `hashWithSalt` (3  :: Int)
  hashWithSalt s Approximant        = s `hashWithSalt` (4  :: Int)
  hashWithSalt s Fricative          = s `hashWithSalt` (5  :: Int)
  hashWithSalt s Affricate          = s `hashWithSalt` (6  :: Int)
  hashWithSalt s LateralFricative   = s `hashWithSalt` (7  :: Int)
  hashWithSalt s LateralApproximant = s `hashWithSalt` (8  :: Int)
  hashWithSalt s LateralFlap        = s `hashWithSalt` (9  :: Int)
  hashWithSalt s Lateral            = s `hashWithSalt` (10 :: Int)




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

instance Hashable VocalFolds where
  hashWithSalt s vf =
    case vf of
      Voiced             -> hashWithSalt s (1 :: Int)
      Voiceless          -> hashWithSalt s (2 :: Int)
      VoicedAspirated    -> hashWithSalt s (3 :: Int)
      VoicelessAspirated -> hashWithSalt s (4 :: Int)
      CreakyVoiced       -> hashWithSalt s (5 :: Int)




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