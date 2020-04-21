{-# LANGUAGE UnicodeSyntax #-}

module Lib_Types where

import Prelude (Bool(False, True), Eq((==)), Show(show),
  concatMap, map, unwords)

import Prelude.Unicode ((⧺), (∈))

data Phonet = Consonant VocalFolds
                        Place   -- | Place of articulation
                        Manner -- | Manner of articulation
                        Airstream
                        
            | Vowel Height
                    Backness
                    Rounding
                    VocalFolds
                    deriving Eq

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
                deriving Eq


backnessStates ∷ [Backness]
backnessStates = [Front, Central, Back]


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
              deriving Eq

data UnmarkableHeight
  = UnmarkedHeight | MarkedHeight Height



heightStates ∷ [Height]
heightStates =
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

data UnmarkableRounding 
  = UnmarkedRounding
  | MarkedRounding Rounding



roundingStates ∷ [Rounding]
roundingStates = [Rounded, Unrounded]

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
           | Places [Place]

data UnmarkablePlace 
  = UnmarkedPlace
  | MarkedPlace Place



placeStates ∷ [Place]
placeStates = [ Bilabial
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
            | LateralFlap  -- There are very few IPA symbols for lateral flaps
            | Lateral -- we need this one for the lateral click.
              deriving Eq

data UnmarkableManner 
  = UnmarkedManner 
  | MarkedManner Manner



mannerStates ∷ [Manner]
mannerStates = [ Plosive
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

data UnmarkableAirstream 
  = UnmarkedAirstream
  | MarkedAirstream Airstream



airstreamStates ∷ [Airstream]
airstreamStates = [ PulmonicEgressive
                  , Click
                  , Implosive
                  ]

data VocalFolds = Voiced
                | Voiceless
                | VoicedAspirated
                | VoicelessAspirated
                | CreakyVoiced
                  deriving Eq

data UnmarkableVocalFolds 
  = UnmarkedVocalFolds | MarkedVocalFolds VocalFolds


vocalFoldStates ∷ [VocalFolds]
vocalFoldStates = [Voiceless, Voiced, VoicedAspirated, VoicelessAspirated, CreakyVoiced]

data PhonetInventory = PhonetInventory [Phonet]




instance Show Phonet where
  show phonet =
    case phonet of
      Consonant v p m a → show v ⧺ " " ⧺ show p ⧺ " " ⧺ show m ⧺ " " ⧺ show a ⧺ " consonant"
      Vowel h b r v   → show v ⧺ " " ⧺ show r ⧺ " " ⧺ show h ⧺ " " ⧺ show b ⧺ " vowel"



instance Show Backness where
  show Front            = "front"
  show Central          = "central"
  show Back             = "back"



instance Show Height where
  show Close          = "close"
  show NearClose      = "near-close"
  show CloseMid       = "close-mid"
  show Mid            = "mid"
  show OpenMid        = "open-mid"
  show NearOpen       = "near-open"
  show Open           = "open"



instance Show Rounding where
  show Rounded          = "rounded"
  show Unrounded        = "unrounded"



instance Eq Place where
  Bilabial     == Bilabial            = True
  LabioDental  == LabioDental         = True
  Dental       == Dental              = True
  Alveolar     == Alveolar            = True
  PostAlveolar == PostAlveolar        = True
  Retroflex    == Retroflex           = True
  Palatal      == Palatal             = True
  Velar        == Velar               = True
  Uvular       == Uvular              = True
  Pharyngeal   == Pharyngeal          = True
  Glottal      == Glottal             = True
  Epiglottal   == Epiglottal          = True
  x            == Places pList        = x ∈ pList
  Places x     == y                   = y == Places x
  _            == _                   = False


instance Show Place where
  show place1 =
    case place1 of
      Bilabial       → "bilabial"
      LabioDental    → "labio-dental"
      Dental         → "dental"
      Alveolar       → "alveolar"
      PostAlveolar   → "post-alveolar"
      Retroflex      → "retroflex"
      Palatal        → "palatal"
      Velar          → "velar"
      Uvular         → "uvular"
      Pharyngeal     → "pharyngeal"
      Glottal        → "glottal"
      Epiglottal     → "epiglottal"
      LabialVelar    → "labial-velar"
      LabialPalatal  → "labial-palatal"
      AlveoloPalatal → "alveolo-palatal"
      PalatoAlveolar → "palato-alveolar"
      Places ps      → unwords (map show ps)


instance Show Manner where
  show manner1 =
    case manner1 of
      Plosive            → "plosive"
      Nasal              → "nasal"
      Trill              → "trill"
      TapOrFlap          → "tap or flap"
      Approximant        → "approximant"
      Fricative          → "fricative"
      Affricate          → "affricate"
      LateralFricative   → "lateral fricative"
      LateralApproximant → "lateral approximant"
      LateralFlap        → "lateral flap"
      Lateral            → "lateral"



instance Show Airstream where
  show airstream1 =
    case airstream1 of
      PulmonicEgressive → "pulmonic egressive"
      Click             → "click"
      Implosive         → "implosive"



instance Show VocalFolds where
  show vocalFolds1 =
    case vocalFolds1 of
      Voiced             → "voiced"
      Voiceless          → "voiceless"
      VoicedAspirated    → "voiced aspirated"
      VoicelessAspirated → "voiceless aspirated"
      CreakyVoiced       → "creaky voiced"


instance Show PhonetInventory where
    show (PhonetInventory phonetes) = concatMap show phonetes
