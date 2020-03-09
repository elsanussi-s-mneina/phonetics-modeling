module Lib where

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
                    deriving (Eq, Show)

data Backness = Front | Central | Back | UnmarkedBackness
                deriving (Eq, Show)

backnessStates :: [Backness]
backnessStates = [Front, Central, Back]

data Height = Close | NearClose | CloseMid | Mid | OpenMid | NearOpen | Open | UnmarkedHeight
              deriving (Eq, Show)

heightStates :: [Height]
heightStates = [Close, NearClose, CloseMid, Mid, OpenMid, NearOpen, Open]

data TenseLax = Tense | Lax | UnmarkedTenseness
                deriving (Eq, Show)

tenseStates :: [TenseLax]
tenseStates = [Tense, Lax]

data Rounding = Rounded | Unrounded | UnmarkedRounding
                deriving (Eq, Show)

roundingStates :: [Rounding]
roundingStates = [Rounded, Unrounded]

data Place = Bilabial | LabioDental | Dental | Alveolar | PostAlveolar
           | Retroflex
           | Palatal  | Velar  | Uvular | Pharyngeal | Glottal | Epiglottal
           -- I am unsure if the following three should be counted
           -- as 6 different places of articulation, or just 3
           | LabialVelar | LabialPalatal | AlveoloPalatal
           | PalatoAlveolar  -- To do: investigate what the difference
           -- is between alveolopalatal, and palatoalveolar
           | UnmarkedPlace
           deriving (Eq, Show)

placeStates :: [Place]
placeStates = [ Bilabial, LabioDental, Dental, Alveolar, PostAlveolar
              , Retroflex
              , Palatal  , Velar  , Uvular , Pharyngeal , Glottal , Epiglottal
              , LabialVelar , LabialPalatal , AlveoloPalatal
              , PalatoAlveolar
              ]

retractedPlace Bilabial = LabioDental
retractedPlace LabioDental = Dental
retractedPlace Dental = Alveolar
retractedPlace Alveolar = PostAlveolar
retractedPlace PostAlveolar = Retroflex
retractedPlace Retroflex = Palatal
retractedPlace Palatal = Velar
retractedPlace Velar = Uvular
retractedPlace Uvular = Pharyngeal
retractedPlace Pharyngeal = Glottal
retractedPlace Glottal = Epiglottal

data Manner = Plosive | Nasal | Trill | TapOrFlap | Approximant | Fricative
              | Affricate 
              | LateralFricative
              | LateralApproximant
              | LateralFlap
              | Lateral -- we need this one for the lateral click.
              | UnmarkedManner -- There are very few IPA symbols for lateral flaps
              deriving (Eq, Show)

mannerStates :: [Manner]
mannerStates = [ Plosive, Nasal, Trill, TapOrFlap, Approximant, Fricative
               , Affricate
               , LateralFricative
               , LateralApproximant
               , LateralFlap
               , Lateral
               ]

data Airstream = PulmonicEgressive | Click | Implosive | UnmarkedAirstream
                 deriving (Eq, Show)

airstreamStates :: [Airstream]
airstreamStates = [PulmonicEgressive, Click, Implosive]

data VocalFolds = Voiced | Voiceless | VoicedAspirated | VoicelessAspirated | UnmarkedVocalFolds
                  deriving (Eq, Show)


vocalFoldStates :: [VocalFolds]
vocalFoldStates = [Voiceless, Voiced]

data PhonetInventory = PhonetInventory [Phonet]


instance Show PhonetInventory where
    show (PhonetInventory phonetes) = concatMap englishDescription phonetes


englishDescription :: Phonet -> String
englishDescription x = show x


voicedPhonet :: Phonet -> Phonet
-- | A function that given an IPA symbol will convert it to the voiced equivalent.
voicedPhonet (Consonant VoicelessAspirated x  y z) = Consonant VoicedAspirated x y z
voicedPhonet (Consonant Voiceless x  y z) = Consonant Voiced x y z
voicedPhonet (Consonant Voiced x  y z) = Consonant Voiced x y z
voicedPhonet (Consonant VoicedAspirated x  y z) = Consonant VoicedAspirated x y z
voicedPhonet (Vowel x y z _) = Vowel x y z     Voiced


devoicedPhonet :: Phonet -> Phonet
-- | A function that given an IPA symbol will convert it to the voiceless equivalent.
devoicedPhonet (Consonant Voiced x  y z) = Consonant Voiceless x  y z
devoicedPhonet (Consonant Voiceless x  y z) = Consonant Voiceless x  y z
devoicedPhonet (Consonant VoicedAspirated x  y z) = Consonant VoicelessAspirated x  y z
devoicedPhonet (Consonant VoicelessAspirated x  y z) = Consonant VoicelessAspirated x  y z
devoicedPhonet (Vowel x y z _) = Vowel x y z        Voiceless



spirantizedPhonet :: Phonet -> Phonet
spirantizedPhonet (Consonant x place Plosive z) | place /= Alveolar
  = Consonant x place Fricative z

-- The following is inelegant, but there is no other way in the system,
-- right now. The part that is inelegant is that,
-- a t which is considered alveolar, becomes θ which is dental
-- when spirantized. So the following line implements this
-- change in place of articulation.
spirantizedPhonet (Consonant x Alveolar Plosive z) =
  Consonant x Dental Fricative z



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

unmarkDifferences (Vowel height1 backness1 rounding1 voice1) (Consonant voice2 place2 manner2 airstream2) =
  let voice' = if voice1 == voice2 then voice1 else UnmarkedVocalFolds
  in Vowel UnmarkedHeight UnmarkedBackness UnmarkedRounding voice'


unmarkDifferences (Consonant voice2 place2 manner2 airstream2) (Vowel height1 backness1 rounding1 voice1) =
  unmarkDifferences (Vowel height1 backness1 rounding1 voice1) (Consonant voice2 place2 manner2 airstream2)



-- This function (I realize it is poorly named)
-- takes any unmarked attributes in the phoneme definition,
-- and returns a list with all possibilities for that attribute.
generateFromUnmarked :: Phonet -> [Phonet]
generateFromUnmarked (Consonant voice place manner airstream) =
  let voice'     = if voice     == UnmarkedVocalFolds     then vocalFoldStates else [voice]
      place'     = if place     == UnmarkedPlace          then placeStates     else [place]
      manner'    = if manner    == UnmarkedManner         then mannerStates    else [manner]
      airstream' = if airstream == UnmarkedAirstream      then airstreamStates else [airstream]
  in [Consonant v p m a | p <- place', v <- voice',  m <- manner', a <- airstream']

generateFromUnmarked (Vowel height backness rounding voice) =
  let voice'    = if voice    == UnmarkedVocalFolds    then vocalFoldStates else [voice]
      height'   = if height   == UnmarkedHeight        then heightStates   else [height]
      backness' = if backness == UnmarkedBackness      then backnessStates else [backness]
      rounding' = if rounding == UnmarkedRounding      then roundingStates else [rounding]
  in [Vowel h b r v | h <- height', b <- backness', r <- rounding', v <- voice']


-- The following function returns whether an articulation is
-- considered impossible according to the IPA (pulmonic) consonants chart.
-- Does not work for other values.
impossible :: Phonet -> Bool
impossible (Consonant Voiced Pharyngeal Plosive PulmonicEgressive) = True
impossible (Consonant VoicedAspirated Pharyngeal Plosive PulmonicEgressive) = True
impossible (Consonant Voiceless Glottal Plosive PulmonicEgressive) = False -- [ʔ] is not impossible.
impossible (Consonant _ Glottal Fricative PulmonicEgressive) = False  -- [h] and [ɦ] are not impossible.
impossible (Consonant _ Glottal _ PulmonicEgressive) = True -- all other pulmonary egressive consonants are impossible..
impossible (Consonant _ Pharyngeal Nasal PulmonicEgressive) = True
impossible (Consonant _ Pharyngeal LateralFricative PulmonicEgressive) = True
impossible (Consonant _ Pharyngeal LateralApproximant PulmonicEgressive) = True
impossible (Consonant _ Velar Trill PulmonicEgressive) = True
impossible (Consonant _ Velar TapOrFlap PulmonicEgressive) = True
impossible (Consonant _ Bilabial LateralFricative PulmonicEgressive) = True
impossible (Consonant _ Bilabial LateralApproximant PulmonicEgressive) = True
impossible (Consonant _ LabioDental LateralFricative PulmonicEgressive) = True
impossible (Consonant _ LabioDental LateralApproximant PulmonicEgressive) = True
impossible _ = False -- Everything else is assumed to be true.

-- | Whether a phonet is in an intervocalic environment.
-- | This means that there is a vowel directly before it,
-- | and one after it.
interVocalic :: Phonet  -- Before
             -> Phonet  -- After
             -> Bool
interVocalic (Vowel _ _ _ _) (Vowel _ _ _ _) = True
interVocalic _ _ = False

