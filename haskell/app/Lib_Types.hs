{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib_Types where
import Relude (Text, unwords)
import Prelude
  ( Bool(False, True)
  , Eq((==))
  , map
  )
import Prelude.Unicode ((∈))
import Data.Text (append, concat)
-- The data type Phonet, represents a linguistics
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
                        Place   -- | Place of articulation
                        Manner  -- | Manner of articulation
                        Airstream

            | Vowel Height
                    Backness
                    Rounding
                    VocalFolds
                    deriving Eq

-- The data type UnmarkablePhonet was originally intended
-- to represent a phoneme, or a phonete but with the additional
-- ability to have unmarked properties, such as unmarked voicing.
-- So far, though it has not been used in a way consistent with
-- linguistics, instead it has been used to represent all values of a property.
-- So for example, we would use unmarked voicing to represent all
-- possible vocal fold configurations. We would use unmarked place to
-- to reprsent all possible places of articulation.
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
            | Lateral      -- we need this one for the lateral click.
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
vocalFoldStates
  = [ Voiceless
    , Voiced
    , VoicedAspirated
    , VoicelessAspirated
    , CreakyVoiced
    ]

data PhonetInventory = PhonetInventory [Phonet]



showPhonet ∷ Phonet → Text
showPhonet phonet =
  case phonet of
    Consonant v p m a → showVocalFolds v `append` " " `append` showPlace p `append` " " `append` showManner m `append` " " `append` showAirstream a
                               `append` " consonant"
    Vowel h b r v     → showVocalFolds v `append` " " `append` showRounding r `append` " " `append` showHeight h `append` " " `append` showBackness b
                               `append` " vowel"


showBackness ∷ Backness → Text
showBackness Front            = "front"
showBackness Central          = "central"
showBackness Back             = "back"


showHeight ∷ Height → Text
showHeight Close          = "close"
showHeight NearClose      = "near-close"
showHeight CloseMid       = "close-mid"
showHeight Mid            = "mid"
showHeight OpenMid        = "open-mid"
showHeight NearOpen       = "near-open"
showHeight Open           = "open"


showRounding ∷ Rounding → Text
showRounding Rounded          = "rounded"
showRounding Unrounded        = "unrounded"



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

showPlace ∷ Place → Text
showPlace place1 =
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
    Places ps      → unwords (map showPlace ps)

showManner ∷ Manner → Text
showManner manner1 =
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


showAirstream ∷ Airstream → Text
showAirstream airstream1 =
  case airstream1 of
    PulmonicEgressive → "pulmonic egressive"
    Click             → "click"
    Implosive         → "implosive"


showVocalFolds ∷ VocalFolds → Text
showVocalFolds vocalFolds1 =
  case vocalFolds1 of
    Voiced             → "voiced"
    Voiceless          → "voiceless"
    VoicedAspirated    → "voiced aspirated"
    VoicelessAspirated → "voiceless aspirated"
    CreakyVoiced       → "creaky voiced"

showPhonetInventory ∷ PhonetInventory → Text
showPhonetInventory (PhonetInventory phonetes) = concat (map showPhonet phonetes)



type IPAText = Text
-- For storing text meant to be interpreted as International phonetic alphabet


{-|
 Represents the '+' (plus) or '-' (minus)
 of a binary feature. e.g. [+sonorant],
 [-sonorant]
|-}
data Polarity = Plus | Minus
                deriving Eq

showPolarity ∷ Polarity → Text
showPolarity Plus = "+"
showPolarity Minus = "-"



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

showPhonemeFeature ∷ PhonemeFeature → Text
showPhonemeFeature (SyllabicFeature polarity)           = showPolarity polarity `append` "syllabic"
showPhonemeFeature (ConsonantalFeature polarity)        = showPolarity polarity `append` "consonantal"
showPhonemeFeature (SonorantFeature polarity)           = showPolarity polarity `append` "sonorant"
showPhonemeFeature (ContinuantFeature polarity)         = showPolarity polarity `append` "continuant"
showPhonemeFeature (VoiceFeature polarity)              = showPolarity polarity `append` "voice"
showPhonemeFeature (AdvancedTongueRootFeature polarity) = showPolarity polarity `append` "ATR"
showPhonemeFeature NasalFeature                         =                                "nasal"
showPhonemeFeature LateralFeature                       =                                "lateral"
showPhonemeFeature DelayedReleaseFeature                =                                "delayed release"
showPhonemeFeature SpreadGlottisFeature                 =                                "spread glottis"
showPhonemeFeature ConstrictedGlottisFeature            =                                "constricted glottis"
showPhonemeFeature LabialFeature                        =                                "labial"
showPhonemeFeature CoronalFeature                       =                                "coronal"
showPhonemeFeature DorsalFeature                        =                                "dorsal"
showPhonemeFeature PharyngealFeature                    =                                "pharyngeal"
showPhonemeFeature LaryngealFeature                     =                                "laryngeal"
showPhonemeFeature (RoundFeature polarity)              = showPolarity polarity `append` "round"
showPhonemeFeature (AnteriorFeature polarity)           = showPolarity polarity `append` "anterior"
showPhonemeFeature (DistributedFeature polarity)        = showPolarity polarity `append` "distributed"
showPhonemeFeature (StridentFeature polarity)           = showPolarity polarity `append` "strident"
showPhonemeFeature (HighFeature polarity)               = showPolarity polarity `append` "high"
showPhonemeFeature (LowFeature polarity)                = showPolarity polarity `append` "low"
showPhonemeFeature (BackFeature polarity)               = showPolarity polarity `append` "back"
