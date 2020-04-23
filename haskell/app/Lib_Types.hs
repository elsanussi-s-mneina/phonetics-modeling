{-# LANGUAGE UnicodeSyntax #-}

module Lib_Types where

import Prelude 
  ( String
  , Bool(False, True)
  , Eq((==))
  , Show(show)
  , concatMap, map
  , unwords
  )

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



type IPAText = String
-- For storing text meant to be interpreted as International phonetic alphabet


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

