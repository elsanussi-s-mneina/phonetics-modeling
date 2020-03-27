module Lib where


import Prelude
  (
    Bool(False, True), Eq, Show, String,
    concatMap, elem, show,
    (==), (++)
  )

data Phonet = Consonant { vocalFolds :: VocalFolds
                        , place :: Place   -- | Place of articulation
                        , manner :: Manner -- | Manner of articulation
                        , airstream :: Airstream
                        }
            | Vowel { height :: Height
                    , backness :: Backness
                    , rounding :: Rounding
                    , vocalFolds :: VocalFolds
                    }
                    deriving Eq

instance Show Phonet where
  show phonet =
    case phonet of
      Consonant v p m a -> show v ++ " " ++ show p ++ " " ++ show m ++ " " ++ show a ++ " consonant"
      Vowel h b r v   -> show v ++ " " ++ show r ++ " " ++ show h ++ " " ++ show b ++ " vowel"

data Backness = Front
              | Central
              | Back
              | UnmarkedBackness
                deriving Eq

instance Show Backness where
  show Front            = "front"
  show Central          = "central"
  show Back             = "back"
  show UnmarkedBackness = ""

backnessStates :: [Backness]
backnessStates = [Front, Central, Back]

data Height = Close
            | NearClose
            | CloseMid
            | Mid
            | OpenMid
            | NearOpen
            | Open
            | UnmarkedHeight
              deriving Eq

instance Show Height where
  show Close          = "close"
  show NearClose      = "near-close"
  show CloseMid       = "close-mid"
  show Mid            = "mid"
  show OpenMid        = "open-mid"
  show NearOpen       = "near-open"
  show Open           = "open"
  show UnmarkedHeight = ""

heightStates :: [Height]
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
              | UnmarkedRounding
                deriving Eq

instance Show Rounding where
  show Rounded          = "rounded"
  show Unrounded        = "unrounded"
  show UnmarkedRounding = ""

roundingStates :: [Rounding]
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
           | UnmarkedPlace

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
  x            == Places pList        = x `elem` pList
  Places x     == y                   = y == Places x
  _            == _                   = False


instance Show Place where
  show place1 =
    case place1 of
      Bilabial       -> "bilabial"
      LabioDental    -> "labio-dental"
      Dental         -> "dental"
      Alveolar       -> "alveolar"
      PostAlveolar   -> "post-alveolar"
      Retroflex      -> "retroflex"
      Palatal        -> "palatal"
      Velar          -> "velar"
      Uvular         -> "uvular"
      Pharyngeal     -> "pharyngeal"
      Glottal        -> "glottal"
      Epiglottal     -> "epiglottal"
      LabialVelar    -> "labial-velar"
      LabialPalatal  -> "labial-palatal"
      AlveoloPalatal -> "alveolo-palatal"
      PalatoAlveolar -> "palato-alveolar"
      UnmarkedPlace  -> ""

placeStates :: [Place]
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

retractedPlace :: Place -> Place
retractedPlace Bilabial     = LabioDental
retractedPlace LabioDental  = Dental
retractedPlace Dental       = Alveolar
retractedPlace Alveolar     = PostAlveolar
retractedPlace PostAlveolar = Retroflex
retractedPlace Retroflex    = Palatal
retractedPlace Palatal      = Velar
retractedPlace Velar        = Uvular
retractedPlace Uvular       = Pharyngeal
retractedPlace Pharyngeal   = Glottal
retractedPlace Glottal      = Epiglottal
retractedPlace same         = same


data Manner = Plosive
            | Nasal
            | Trill
            | TapOrFlap
            | Approximant
            | Fricative
            | Affricate
            | LateralFricative
            | LateralApproximant
            | LateralFlap
            | Lateral -- we need this one for the lateral click.
            | UnmarkedManner -- There are very few IPA symbols for lateral flaps
              deriving Eq

instance Show Manner where
  show manner1 =
    case manner1 of
      Plosive            -> "plosive"
      Nasal              -> "nasal"
      Trill              -> "trill"
      TapOrFlap          -> "tap or flap"
      Approximant        -> "approximant"
      Fricative          -> "fricative"
      Affricate          -> "affricate"
      LateralFricative   -> "lateral fricative"
      LateralApproximant -> "lateral approximant"
      LateralFlap        -> "lateral flap"
      Lateral            -> "lateral"
      UnmarkedManner     -> ""

mannerStates :: [Manner]
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
               | UnmarkedAirstream
                 deriving Eq

instance Show Airstream where
  show airstream1 =
    case airstream1 of
      PulmonicEgressive -> "pulmonic egressive"
      Click             -> "click"
      Implosive         -> "implosive"
      UnmarkedAirstream -> ""

airstreamStates :: [Airstream]
airstreamStates = [ PulmonicEgressive
                  , Click
                  , Implosive
                  ]

data VocalFolds = Voiced
                | Voiceless
                | VoicedAspirated
                | VoicelessAspirated
                | CreakyVoiced
                | UnmarkedVocalFolds
                  deriving Eq


instance Show VocalFolds where
  show vocalFolds1 =
    case vocalFolds1 of
      Voiced             -> "voiced"
      Voiceless          -> "voiceless"
      VoicedAspirated    -> "voiced aspirated"
      VoicelessAspirated -> "voiceless aspirated"
      CreakyVoiced       -> "creaky voiced"
      UnmarkedVocalFolds -> ""

vocalFoldStates :: [VocalFolds]
vocalFoldStates = [Voiceless, Voiced, VoicedAspirated, VoicelessAspirated, CreakyVoiced]

newtype PhonetInventory = PhonetInventory [Phonet]


instance Show PhonetInventory where
    show (PhonetInventory phonetes) = concatMap englishDescription phonetes


englishDescription :: Phonet -> String
englishDescription x = show x


-- | A function that given an IPA symbol will convert it to the voiced equivalent.
voicedPhonet :: Phonet -> Phonet
voicedPhonet (Consonant   VoicelessAspirated x y z) = Consonant   VoicedAspirated x y z
voicedPhonet (Consonant   Voiceless          x y z) = Consonant   Voiced x y z
voicedPhonet (Consonant   Voiced             x y z) = Consonant   Voiced x y z
voicedPhonet (Consonant   VoicedAspirated    x y z) = Consonant   VoicedAspirated x y z
voicedPhonet (Consonant   _                  x y z) = Consonant   Voiced x y z
voicedPhonet (Vowel x y z _                       ) = Vowel x y z Voiced

-- | A function that given an IPA symbol will convert it to the voiceless equivalent.
devoicedPhonet :: Phonet -> Phonet
devoicedPhonet (Consonant   Voiced             x y z) = Consonant   Voiceless          x y z
devoicedPhonet (Consonant   CreakyVoiced       x y z) = Consonant   Voiceless          x y z
devoicedPhonet (Consonant   Voiceless          x y z) = Consonant   Voiceless          x y z
devoicedPhonet (Consonant   VoicedAspirated    x y z) = Consonant   VoicelessAspirated x y z
devoicedPhonet (Consonant   VoicelessAspirated x y z) = Consonant   VoicelessAspirated x y z
devoicedPhonet (Consonant   UnmarkedVocalFolds x y z) = Consonant   UnmarkedVocalFolds x y z
devoicedPhonet (Vowel x y z _                       ) = Vowel x y z Voiceless



spirantizedPhonet :: Phonet -> Phonet

-- The following is inelegant, but there is no other way in the system,
-- right now. The part that is inelegant is that,
-- a [t] which is considered alveolar, when spirantized becomes [θ] which is dental.
-- So the following line implements this
-- change in place of articulation.
spirantizedPhonet (Consonant x Alveolar Plosive z) =
  Consonant x Dental Fricative z

spirantizedPhonet (Consonant x place1 Plosive z)
  = Consonant x place1 Fricative z
spirantizedPhonet other = other



unmarkDifferences :: Phonet -> Phonet -> Phonet
unmarkDifferences (Consonant voice1 place1 manner1 airstream1) (Consonant voice2 place2 manner2 airstream2)=
  let voice'     = if voice1     == voice2     then voice1     else UnmarkedVocalFolds
      place'     = if place1     == place2     then place1     else UnmarkedPlace
      manner'    = if manner1    == manner2    then manner1    else UnmarkedManner
      airstream' = if airstream1 == airstream2 then airstream1 else UnmarkedAirstream
  in Consonant voice' place' manner' airstream'

unmarkDifferences (Vowel height1 backness1 rounding1 voice1) (Vowel height2 backness2 rounding2 voice2) =
  let voice'    = if voice1    == voice2    then voice1    else UnmarkedVocalFolds
      height'   = if height1   == height2   then height1   else UnmarkedHeight
      backness' = if backness1 == backness2 then backness1 else UnmarkedBackness
      rounding' = if rounding1 == rounding2 then rounding1 else UnmarkedRounding
  in Vowel height' backness' rounding' voice'

unmarkDifferences (Vowel _ _ _ voice1) (Consonant voice2 _ _ _) =
  let voice' = if voice1 == voice2 then voice1 else UnmarkedVocalFolds
  in Vowel UnmarkedHeight UnmarkedBackness UnmarkedRounding voice'


unmarkDifferences c@(Consonant _ _ _ _) v@(Vowel _ _ _ _) =
  unmarkDifferences v c -- Change the order of arguments



-- This function (I realize it is poorly named)
-- takes any unmarked attributes in the phoneme definition,
-- and returns a list with all possibilities for that attribute.
generateFromUnmarked :: Phonet -> [Phonet]
generateFromUnmarked (Consonant voice1 place1 manner1 airstream1) =
  let voice'     = if voice1     == UnmarkedVocalFolds     then vocalFoldStates else [voice1]
      place'     = if place1     == UnmarkedPlace          then placeStates     else [place1]
      manner'    = if manner1    == UnmarkedManner         then mannerStates    else [manner1]
      airstream' = if airstream1 == UnmarkedAirstream      then airstreamStates else [airstream1]
  in [Consonant v p m a | p <- place', v <- voice',  m <- manner', a <- airstream']

generateFromUnmarked (Vowel height1 backness1 rounding1 voice1) =
  let voice'    = if voice1    == UnmarkedVocalFolds then vocalFoldStates else [voice1]
      height'   = if height1   == UnmarkedHeight     then heightStates    else [height1]
      backness' = if backness1 == UnmarkedBackness   then backnessStates  else [backness1]
      rounding' = if rounding1 == UnmarkedRounding   then roundingStates  else [rounding1]
  in [Vowel h b r v | h <- height', b <- backness', r <- rounding', v <- voice']


-- The following function returns whether an articulation is
-- considered impossible according to the IPA (pulmonic) consonants chart.
-- Does not work for other values.
impossible :: Phonet -> Bool
impossible (Consonant Voiced          Pharyngeal  Plosive            PulmonicEgressive) = True
impossible (Consonant VoicedAspirated Pharyngeal  Plosive            PulmonicEgressive) = True
impossible (Consonant Voiceless       Glottal     Plosive            PulmonicEgressive) = False  -- [ʔ] is not impossible.
impossible (Consonant _               Glottal     Fricative          PulmonicEgressive) = False  -- [h] and [ɦ] are not impossible.
impossible (Consonant _               Glottal     _                  PulmonicEgressive) = True   -- all other pulmonary egressive consonants are impossible..
impossible (Consonant _               Pharyngeal  Nasal              PulmonicEgressive) = True
impossible (Consonant _               Pharyngeal  LateralFricative   PulmonicEgressive) = True
impossible (Consonant _               Pharyngeal  LateralApproximant PulmonicEgressive) = True
impossible (Consonant _               Velar       Trill              PulmonicEgressive) = True
impossible (Consonant _               Velar       TapOrFlap          PulmonicEgressive) = True
impossible (Consonant _               Bilabial    LateralFricative   PulmonicEgressive) = True
impossible (Consonant _               Bilabial    LateralApproximant PulmonicEgressive) = True
impossible (Consonant _               LabioDental LateralFricative   PulmonicEgressive) = True
impossible (Consonant _               LabioDental LateralApproximant PulmonicEgressive) = True
impossible _ = False -- Everything else is assumed to be possible.
