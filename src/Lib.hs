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

data VocalFolds = Voiced | Voiceless | UnmarkedVocalFolds
                  deriving (Eq, Show)


vocalFoldStates :: [VocalFolds]
vocalFoldStates = [Voiceless, Voiced]

data PhonetInventory = PhonetInventory [Phonet]


instance Show PhonetInventory where
    show (PhonetInventory phonetes) = concatMap constructIPA phonetes


type IPAText = String
  -- For storing text meant to be interpreted as International phonetic alphabet

-- | This function will allow us to convert an IPA symbol
-- | to its analyzed form (its phonetic features)
-- Currently, only the consonants (pulmonic) in the 2005 IPA chart are included.
analyzeIPA  :: IPAText -> Phonet
-- Plosives:
analyzeIPA "p"  = Consonant  Voiceless Bilabial Plosive PulmonicEgressive
analyzeIPA "b"  = Consonant  Voiced Bilabial Plosive PulmonicEgressive
analyzeIPA "t"  = Consonant  Voiceless Alveolar Plosive PulmonicEgressive
analyzeIPA "d"  = Consonant  Voiced Alveolar Plosive PulmonicEgressive
analyzeIPA "ʈ"  = Consonant  Voiceless Retroflex Plosive PulmonicEgressive
analyzeIPA "ɖ"  = Consonant  Voiced Retroflex Plosive PulmonicEgressive
analyzeIPA "c"  = Consonant  Voiceless Palatal Plosive PulmonicEgressive
analyzeIPA "ɟ"  = Consonant  Voiced Palatal Plosive PulmonicEgressive
analyzeIPA "k"  = Consonant  Voiceless Velar Plosive PulmonicEgressive
analyzeIPA "g"  = Consonant  Voiced Velar Plosive PulmonicEgressive
analyzeIPA "q"  = Consonant  Voiceless Uvular Plosive PulmonicEgressive
analyzeIPA "ɢ"  = Consonant  Voiced Uvular Plosive PulmonicEgressive
analyzeIPA "ʔ"  = Consonant  Voiceless Glottal Plosive PulmonicEgressive

-- Nasals:
analyzeIPA "m"  = Consonant  Voiced Bilabial Nasal PulmonicEgressive
analyzeIPA "ɱ"  = Consonant  Voiced LabioDental Nasal PulmonicEgressive
analyzeIPA "n"  = Consonant  Voiced Alveolar Nasal PulmonicEgressive
analyzeIPA "ɳ"  = Consonant  Voiced Retroflex Nasal PulmonicEgressive
analyzeIPA "ɲ"  = Consonant  Voiced Palatal Nasal PulmonicEgressive
analyzeIPA "ŋ"  = Consonant  Voiced Velar Nasal PulmonicEgressive
analyzeIPA "ɴ"  = Consonant  Voiced Uvular Nasal PulmonicEgressive

-- Trills:
analyzeIPA "ʙ"  = Consonant  Voiced Bilabial Trill PulmonicEgressive
analyzeIPA "r"  = Consonant  Voiced Alveolar Trill PulmonicEgressive
analyzeIPA "ʀ"  = Consonant  Voiced Uvular Trill PulmonicEgressive

-- Taps or flaps:
analyzeIPA "ⱱ"  = Consonant  Voiced LabioDental TapOrFlap PulmonicEgressive
analyzeIPA "ɾ"  = Consonant  Voiced Alveolar TapOrFlap PulmonicEgressive
analyzeIPA "ɽ"  = Consonant  Voiced Retroflex TapOrFlap PulmonicEgressive

-- Fricatives:
analyzeIPA "ɸ"  = Consonant  Voiceless Bilabial Fricative PulmonicEgressive
analyzeIPA "β"  = Consonant  Voiced Bilabial Fricative PulmonicEgressive
analyzeIPA "f"  = Consonant  Voiceless LabioDental Fricative PulmonicEgressive
analyzeIPA "v"  = Consonant  Voiced LabioDental Fricative PulmonicEgressive
analyzeIPA "θ"  = Consonant  Voiceless Dental Fricative PulmonicEgressive
analyzeIPA "ð"  = Consonant  Voiced Dental Fricative PulmonicEgressive
analyzeIPA "s"  = Consonant  Voiceless Alveolar Fricative PulmonicEgressive
analyzeIPA "z"  = Consonant  Voiced Alveolar Fricative PulmonicEgressive
analyzeIPA "ʃ"  = Consonant  Voiceless PostAlveolar Fricative PulmonicEgressive
analyzeIPA "ʒ"  = Consonant  Voiced PostAlveolar Fricative PulmonicEgressive
analyzeIPA "ʂ"  = Consonant  Voiceless Retroflex Fricative PulmonicEgressive
analyzeIPA "ʐ"  = Consonant  Voiced Retroflex Fricative PulmonicEgressive
analyzeIPA "ç"  = Consonant  Voiceless Palatal Fricative PulmonicEgressive
analyzeIPA "ʝ"  = Consonant  Voiced Palatal Fricative PulmonicEgressive
analyzeIPA "x"  = Consonant  Voiceless Velar Fricative PulmonicEgressive
analyzeIPA "ɣ"  = Consonant  Voiced Velar Fricative PulmonicEgressive
analyzeIPA "χ"  = Consonant  Voiceless Uvular Fricative PulmonicEgressive
analyzeIPA "ʁ"  = Consonant  Voiced Uvular Fricative PulmonicEgressive
analyzeIPA "ħ"  = Consonant  Voiceless Pharyngeal Fricative PulmonicEgressive
analyzeIPA "ʕ"  = Consonant  Voiced Pharyngeal Fricative PulmonicEgressive
analyzeIPA "h"  = Consonant  Voiceless Glottal Fricative PulmonicEgressive
analyzeIPA "ɦ"  = Consonant  Voiced Glottal Fricative PulmonicEgressive

-- Affricates
analyzeIPA "t͡ʃ" = Consonant  Voiceless PostAlveolar Affricate PulmonicEgressive
analyzeIPA "d͡ʒ" = Consonant  Voiced PostAlveolar Affricate PulmonicEgressive
-- We should probably enforce use of the tie-bar underneath, otherwise
-- it would not be deterministic to determine whether two graphemes here
-- represent affricates or a plosive followed by a fricative.

-- Lateral Fricatives:
analyzeIPA "ɬ" = Consonant Voiceless Alveolar LateralFricative PulmonicEgressive
analyzeIPA "ɮ" = Consonant  Voiced Alveolar LateralFricative PulmonicEgressive


-- Approximants:
analyzeIPA "ʋ"  = Consonant  Voiced LabioDental Approximant PulmonicEgressive
analyzeIPA "ɹ"  = Consonant  Voiced PostAlveolar Approximant PulmonicEgressive
analyzeIPA "ɻ"  = Consonant  Voiced Retroflex Approximant PulmonicEgressive
analyzeIPA "j"  = Consonant  Voiced Palatal Approximant PulmonicEgressive
analyzeIPA "ɰ"  = Consonant  Voiced Velar Approximant PulmonicEgressive

-- Lateral Approximants:
analyzeIPA "l"  = Consonant  Voiced Alveolar LateralApproximant PulmonicEgressive
analyzeIPA "ɭ"  = Consonant  Voiced Retroflex LateralApproximant PulmonicEgressive
analyzeIPA "ʎ"  = Consonant  Voiced Palatal LateralApproximant PulmonicEgressive
analyzeIPA "ʟ"  = Consonant  Voiced Velar LateralApproximant PulmonicEgressive


-- Under the Other Symbols part of the IPA chart:

analyzeIPA "w"  = Consonant  Voiced LabialVelar Approximant PulmonicEgressive

analyzeIPA "ʍ" = Consonant Voiceless LabialVelar Fricative PulmonicEgressive
analyzeIPA "ɥ" = Consonant Voiced LabialPalatal Approximant PulmonicEgressive
analyzeIPA "ʜ" = Consonant Voiceless Epiglottal Fricative PulmonicEgressive
analyzeIPA "ʢ" = Consonant Voiced Epiglottal Fricative PulmonicEgressive
analyzeIPA "ʡ" = Consonant Voiceless Epiglottal Plosive PulmonicEgressive
-- Is the epiglottal plosive voiceless? The IPA chart does not specify.

analyzeIPA "ɕ" = Consonant Voiceless AlveoloPalatal Fricative PulmonicEgressive
analyzeIPA "ʑ" = Consonant Voiced AlveoloPalatal Fricative PulmonicEgressive
analyzeIPA "ɺ" = Consonant Voiced Alveolar LateralFlap PulmonicEgressive

-- We cannot handle the ɧ (simultaneous ʃ and x) because
-- we did not define our data types to handle it yet.
-- In any case, here is some pseudocode for it:
-- analyzeIPA "ɧ" = simultaneous (analyzeIPA "ʃ") (analyzeIPA "x")

-- Other Consonants:
analyzeIPA "ʘ" = Consonant UnmarkedVocalFolds Bilabial UnmarkedManner Click
analyzeIPA "ǀ" = Consonant UnmarkedVocalFolds Dental UnmarkedManner  Click
analyzeIPA "ǃ" = Consonant UnmarkedVocalFolds Alveolar UnmarkedManner Click -- Or it could be PostAlveolar.
analyzeIPA "ǂ" = Consonant UnmarkedVocalFolds PalatoAlveolar UnmarkedManner Click
analyzeIPA "ǁ" = Consonant UnmarkedVocalFolds Alveolar Lateral Click
analyzeIPA "ɓ" = Consonant Voiced Bilabial UnmarkedManner Implosive
analyzeIPA "ɗ" = Consonant Voiced Dental UnmarkedManner Implosive  -- Or Alveolar
analyzeIPA "ʄ" = Consonant Voiced Palatal UnmarkedManner Implosive
analyzeIPA "ɠ" = Consonant Voiced Velar UnmarkedManner Implosive
analyzeIPA "ʛ" = Consonant Voiced Uvular UnmarkedManner Implosive

-- Close Vowels:
analyzeIPA "i"  = Vowel  Close Front   Unrounded Voiced
analyzeIPA "y"  = Vowel  Close Front   Rounded   Voiced
analyzeIPA "ɨ"  = Vowel  Close Central Unrounded Voiced
analyzeIPA "ʉ"  = Vowel  Close Central Rounded   Voiced
analyzeIPA "ɯ"  = Vowel  Close Back    Unrounded Voiced
analyzeIPA "u"  = Vowel  Close Back    Rounded   Voiced

-- Near-close Vowels:
analyzeIPA "ɪ"  = Vowel NearClose Front Unrounded Voiced
analyzeIPA "ʏ"  = Vowel NearClose Front Rounded   Voiced
analyzeIPA "ʊ"  = Vowel NearClose Back  Rounded   Voiced

-- Close-mid Vowels:
analyzeIPA "e"  = Vowel  CloseMid Front   Unrounded Voiced
analyzeIPA "ø"  = Vowel  CloseMid Front   Rounded   Voiced
analyzeIPA "ɘ"  = Vowel  CloseMid Central Unrounded Voiced
analyzeIPA "ɵ"  = Vowel  CloseMid Central Rounded   Voiced
analyzeIPA "ɤ"  = Vowel  CloseMid Back    Unrounded Voiced
analyzeIPA "o"  = Vowel  CloseMid Back    Rounded   Voiced

-- Mid Vowels:
analyzeIPA "ə"  = Vowel Mid Central UnmarkedRounding Voiced


-- Open-mid Vowels:
analyzeIPA "ɛ"  = Vowel  OpenMid Front   Unrounded Voiced
analyzeIPA "œ"  = Vowel  OpenMid Front   Rounded   Voiced
analyzeIPA "ɜ"  = Vowel  OpenMid Central Unrounded Voiced
analyzeIPA "ɞ"  = Vowel  OpenMid Central Rounded   Voiced
analyzeIPA "ʌ"  = Vowel  OpenMid Back    Unrounded Voiced
analyzeIPA "ɔ"  = Vowel  OpenMid Back    Rounded   Voiced

-- Near-open
analyzeIPA "æ"  = Vowel  NearOpen Front   Unrounded Voiced
analyzeIPA "ɐ"  = Vowel  NearOpen Central UnmarkedRounding  Voiced

-- Open Vowels:
analyzeIPA "a"  = Vowel  Open Front Unrounded Voiced
analyzeIPA "ɶ"  = Vowel  Open Front Rounded   Voiced
analyzeIPA "ɑ"  = Vowel  Open Back  Unrounded Voiced
analyzeIPA "ɒ"  = Vowel  Open Back  Rounded   Voiced


-- Handle Diacritics:
analyzeIPA [firstChar, '̥'] =
  let fullGrapheme = analyzeIPA [firstChar]
  in case fullGrapheme of
          Consonant _ place manner airstream    -> Consonant Voiceless place manner airstream
          Vowel height backness rounding _      -> Vowel height backness rounding Voiceless

analyzeIPA [firstChar, '̼'] =
  let fullGrapheme = analyzeIPA [firstChar]
  in case fullGrapheme of
          Consonant _ place manner airstream    -> Consonant Voiced place manner airstream
          Vowel height backness rounding _      -> Vowel height backness rounding Voiced


constructIPA :: Phonet -> IPAText
constructIPA phoneDescription =
  -- If it can represent it as a single character it will
  -- return the single character result (i.e. without diacritics),
  -- otherwise
  -- it will try to represent it in IPA with more than
  -- one character
  let simpleResult = constructIPA1 phoneDescription
  in if simpleResult == "∅"
       then constructIPA2 phoneDescription
       else simpleResult

-- Note to Software Developer: the reason there are three
-- functions for constructing the IPA is to prevent
-- infinite recursion. The reason is that
-- if we only had one function, it would -- for some
-- cases never halt if it could not find a character
-- to place a diacritic on.

-- | Given an analysis construct an IPA symbol
-- | This function will allow us to convert an analyzed form
-- | to its IPA symbol.
-- | Note this only returns one character without diacritics.
constructIPA1  ::  Phonet -> IPAText
-- Plosives:
constructIPA1 (Consonant  Voiced    Bilabial  Plosive PulmonicEgressive) = "b"
constructIPA1 (Consonant  Voiceless Bilabial  Plosive PulmonicEgressive) = "p"
constructIPA1 (Consonant  Voiceless Alveolar  Plosive PulmonicEgressive) = "t"
constructIPA1 (Consonant  Voiced    Alveolar  Plosive PulmonicEgressive) = "d"
constructIPA1 (Consonant  Voiceless Retroflex Plosive PulmonicEgressive) = "ʈ"
constructIPA1 (Consonant  Voiced    Retroflex Plosive PulmonicEgressive) = "ɖ"
constructIPA1 (Consonant  Voiceless Palatal   Plosive PulmonicEgressive) = "c"
constructIPA1 (Consonant  Voiced    Palatal   Plosive PulmonicEgressive) = "ɟ"
constructIPA1 (Consonant  Voiceless Velar     Plosive PulmonicEgressive) = "k"
constructIPA1 (Consonant  Voiced    Velar     Plosive PulmonicEgressive) = "g"
constructIPA1 (Consonant  Voiceless Uvular    Plosive PulmonicEgressive) = "q"
constructIPA1 (Consonant  Voiced    Uvular    Plosive PulmonicEgressive) = "ɢ"
constructIPA1 (Consonant  Voiceless Glottal   Plosive PulmonicEgressive) = "ʔ"

-- Nasals:
constructIPA1 (Consonant  Voiced    Bilabial    Nasal PulmonicEgressive) = "m"
constructIPA1 (Consonant  Voiced    LabioDental Nasal PulmonicEgressive) = "ɱ"
constructIPA1 (Consonant  Voiced    Alveolar    Nasal PulmonicEgressive) = "n"
constructIPA1 (Consonant  Voiced    Retroflex   Nasal PulmonicEgressive) = "ɳ"
constructIPA1 (Consonant  Voiced    Palatal     Nasal PulmonicEgressive) = "ɲ"
constructIPA1 (Consonant  Voiced    Velar       Nasal PulmonicEgressive) = "ŋ"
constructIPA1 (Consonant  Voiced    Uvular      Nasal PulmonicEgressive) = "ɴ"

-- Trills:
constructIPA1 (Consonant  Voiced   Bilabial    Trill PulmonicEgressive) = "ʙ"
constructIPA1 (Consonant  Voiced   Alveolar    Trill PulmonicEgressive) = "r"
constructIPA1 (Consonant  Voiced   Uvular      Trill PulmonicEgressive) = "ʀ" -- Taps or flaps:
constructIPA1 (Consonant  Voiced   LabioDental TapOrFlap PulmonicEgressive) = "ⱱ"
constructIPA1 (Consonant  Voiced   Alveolar    TapOrFlap PulmonicEgressive) = "ɾ"
constructIPA1 (Consonant  Voiced   Retroflex   TapOrFlap PulmonicEgressive) = "ɽ"

-- Fricatives:
constructIPA1 (Consonant  Voiceless Bilabial       Fricative     PulmonicEgressive) = "ɸ"
constructIPA1 (Consonant  Voiced    Bilabial       Fricative        PulmonicEgressive) = "β"
constructIPA1 (Consonant  Voiceless LabioDental    Fricative  PulmonicEgressive) = "f"
constructIPA1 (Consonant  Voiced    LabioDental    Fricative     PulmonicEgressive) = "v"
constructIPA1 (Consonant  Voiceless Dental         Fricative       PulmonicEgressive) = "θ"
constructIPA1 (Consonant  Voiced    Dental         Fricative          PulmonicEgressive) = "ð"
constructIPA1 (Consonant  Voiceless Alveolar       Fricative     PulmonicEgressive) = "s"
constructIPA1 (Consonant  Voiced    Alveolar       Fricative        PulmonicEgressive) = "z"
constructIPA1 (Consonant  Voiceless PostAlveolar   Fricative PulmonicEgressive) = "ʃ"
constructIPA1 (Consonant  Voiced    PostAlveolar   Fricative    PulmonicEgressive) = "ʒ"
constructIPA1 (Consonant  Voiceless Retroflex      Fricative    PulmonicEgressive) = "ʂ"
constructIPA1 (Consonant  Voiced    Retroflex      Fricative       PulmonicEgressive) = "ʐ"
constructIPA1 (Consonant  Voiceless Palatal        Fricative      PulmonicEgressive) = "ç"
constructIPA1 (Consonant  Voiced    Palatal        Fricative         PulmonicEgressive) = "ʝ"
constructIPA1 (Consonant  Voiceless Velar          Fricative        PulmonicEgressive) = "x"
constructIPA1 (Consonant  Voiced    Velar          Fricative           PulmonicEgressive) = "ɣ"
constructIPA1 (Consonant  Voiceless Uvular         Fricative       PulmonicEgressive) = "χ"
constructIPA1 (Consonant  Voiced    Uvular         Fricative          PulmonicEgressive) = "ʁ"
constructIPA1 (Consonant  Voiceless Pharyngeal     Fricative   PulmonicEgressive) = "ħ"
constructIPA1 (Consonant  Voiced    Pharyngeal     Fricative      PulmonicEgressive) = "ʕ"
constructIPA1 (Consonant  Voiceless Glottal        Fricative      PulmonicEgressive) = "h"
constructIPA1 (Consonant  Voiced    Glottal        Fricative         PulmonicEgressive) = "ɦ"



-- Lateral Fricatives:
constructIPA1 (Consonant  Voiceless Alveolar LateralFricative PulmonicEgressive) = "ɬ"
constructIPA1 (Consonant  Voiced    Alveolar LateralFricative PulmonicEgressive) = "ɮ"


-- Approximants:
constructIPA1 (Consonant  Voiced LabioDental   Approximant PulmonicEgressive) = "ʋ"
constructIPA1 (Consonant  Voiced Alveolar      Approximant PulmonicEgressive) = "ɹ"

constructIPA1 (Consonant  Voiced  Retroflex    Approximant PulmonicEgressive) = "ɻ"
constructIPA1 (Consonant  Voiced  Palatal      Approximant PulmonicEgressive) = "j"
constructIPA1 (Consonant  Voiced  Velar        Approximant PulmonicEgressive) = "ɰ"

-- Lateral Approximants:
constructIPA1 (Consonant  Voiced Alveolar   LateralApproximant PulmonicEgressive) = "l"
constructIPA1 (Consonant  Voiced Retroflex  LateralApproximant PulmonicEgressive) = "ɭ"
constructIPA1 (Consonant  Voiced Palatal    LateralApproximant PulmonicEgressive) = "ʎ"
constructIPA1 (Consonant  Voiced Velar      LateralApproximant PulmonicEgressive) = "ʟ"


-- Under the Other Symbols part of the IPA chart:

constructIPA1 (Consonant  Voiced LabialVelar Approximant PulmonicEgressive) = "w"
constructIPA1 (Consonant Voiceless LabialVelar Fricative PulmonicEgressive) = "ʍ"
constructIPA1 (Consonant Voiced LabialPalatal Approximant PulmonicEgressive) = "ɥ"
constructIPA1 (Consonant Voiceless Epiglottal Fricative PulmonicEgressive) = "ʜ"
constructIPA1 (Consonant Voiced Epiglottal Fricative PulmonicEgressive) = "ʢ"
constructIPA1 (Consonant Voiceless Epiglottal Plosive PulmonicEgressive) = "ʡ"
-- Is the epiglottal plosive voiceless? The IPA chart does not specify.

constructIPA1 (Consonant Voiceless AlveoloPalatal Fricative PulmonicEgressive) = "ɕ"
constructIPA1 (Consonant Voiced AlveoloPalatal Fricative PulmonicEgressive) = "ʑ"
constructIPA1 (Consonant Voiced Alveolar LateralFlap PulmonicEgressive) = "ɺ"

-- We cannot handle the ɧ (simultaneous ʃ and x) because
-- we did not define our data types to handle it yet.
-- constructIPA (simultaneous (analyzeIPA "ʃ") (analyzeIPA "x")) = "ɧ"

-- Other Consonants:
constructIPA1 (Consonant UnmarkedVocalFolds Bilabial UnmarkedManner Click) = "ʘ"
constructIPA1 (Consonant UnmarkedVocalFolds Dental UnmarkedManner  Click) = "ǀ"
constructIPA1 (Consonant UnmarkedVocalFolds Alveolar UnmarkedManner Click) = "ǃ" -- Or it could be PostAlveolar.
constructIPA1 (Consonant UnmarkedVocalFolds PalatoAlveolar UnmarkedManner Click) = "ǂ"
constructIPA1 (Consonant UnmarkedVocalFolds Alveolar Lateral Click) = "ǁ"
constructIPA1 (Consonant Voiced Bilabial UnmarkedManner Implosive) = "ɓ"
constructIPA1 (Consonant Voiced Dental UnmarkedManner Implosive) = "ɗ"  -- Or Alveolar
constructIPA1 (Consonant Voiced Palatal UnmarkedManner Implosive) = "ʄ"
constructIPA1 (Consonant Voiced Velar UnmarkedManner Implosive) = "ɠ"
constructIPA1 (Consonant Voiced Uvular UnmarkedManner Implosive) = "ʛ"


-- Close Vowels:
constructIPA1 (Vowel  Close Front   Unrounded Voiced) = "i"
constructIPA1 (Vowel  Close Front   Rounded   Voiced) = "y"
constructIPA1 (Vowel  Close Central Unrounded Voiced) = "ɨ"
constructIPA1 (Vowel  Close Central Rounded   Voiced) = "ʉ"
constructIPA1 (Vowel  Close Back    Unrounded Voiced) = "ɯ"
constructIPA1 (Vowel  Close Back    Rounded   Voiced) = "u"

-- Near-close Vowels:
constructIPA1 (Vowel NearClose Front Unrounded Voiced) = "ɪ"
constructIPA1 (Vowel NearClose Front Rounded   Voiced) = "ʏ"
constructIPA1 (Vowel NearClose Back  Rounded   Voiced) = "ʊ"

-- Close-mid Vowels:
constructIPA1 (Vowel  CloseMid Front   Unrounded Voiced) = "e"
constructIPA1 (Vowel  CloseMid Front   Rounded   Voiced) = "ø"
constructIPA1 (Vowel  CloseMid Central Unrounded Voiced) = "ɘ"
constructIPA1 (Vowel  CloseMid Central Rounded   Voiced) = "ɵ"
constructIPA1 (Vowel  CloseMid Back    Unrounded Voiced) = "ɤ"
constructIPA1 (Vowel  CloseMid Back    Rounded   Voiced) = "o"

-- Mid Vowels:
constructIPA1 (Vowel Mid Central UnmarkedRounding Voiced) = "ə"


-- Open-mid Vowels:
constructIPA1 (Vowel  OpenMid Front   Unrounded Voiced) = "ɛ"
constructIPA1 (Vowel  OpenMid Front   Rounded   Voiced) = "œ"
constructIPA1 (Vowel  OpenMid Central Unrounded Voiced) = "ɜ"
constructIPA1 (Vowel  OpenMid Central Rounded   Voiced) = "ɞ"
constructIPA1 (Vowel  OpenMid Back    Unrounded Voiced) = "ʌ"
constructIPA1 (Vowel  OpenMid Back    Rounded   Voiced) = "ɔ"

-- Near-open
constructIPA1 (Vowel  NearOpen Front   Unrounded Voiced) = "æ"
constructIPA1 (Vowel  NearOpen Central UnmarkedRounding  Voiced) = "ɐ"

-- Open Vowels:
constructIPA1 (Vowel  Open Front Unrounded Voiced) = "a"
constructIPA1 (Vowel  Open Front Rounded   Voiced) = "ɶ"
constructIPA1 (Vowel  Open Back  Unrounded Voiced) = "ɑ"
constructIPA1 (Vowel  Open Back  Rounded   Voiced) = "ɒ"
constructIPA1 _ = "∅"


constructIPA2 :: Phonet -> IPAText
-- Affricates
constructIPA2 (Consonant  Voiceless PostAlveolar  Affricate PulmonicEgressive) = "t͡ʃ"
constructIPA2 (Consonant  Voiced    PostAlveolar  Affricate PulmonicEgressive) = "d͡ʒ"
constructIPA2 (Consonant  Voiceless Bilabial Affricate PulmonicEgressive) = "p͡ɸ"
-- constructIPA2 (Consonant  Voiceless LabialVelar? Affricate PulmonicEgressive) = "k͡p"
constructIPA2 (Consonant  Voiceless Alveolar Affricate PulmonicEgressive) = "t͜s"
constructIPA2 (Consonant  Voiced Alveolar Affricate PulmonicEgressive) = "d͡z"
constructIPA2 (Consonant  Voiceless Velar Affricate PulmonicEgressive) = "k͡x"
-- constructIPA2 (Consonant  Voiceless Palatal (or AlveolaPalatal?) Affricate PulmonicEgressive) = "c͡ɕ"
constructIPA2 (Consonant Voiceless Uvular Affricate PulmonicEgressive) = "q͡χ"

-- If there isn't a symbol, and the consonant we want is voiceless,
-- Just take the symbol for a voiced consonant,
-- and then put that diacritic that means voiceless after.
-- (The following two definitions are intended to implement that)
-- Add the small circle diacritic to consonants to make them voiceless.
constructIPA2 (Consonant Voiceless x y z) =
  constructIPA1 (Consonant Voiced x y z) ++ "̥" -- add diacritic for voiceless

-- Add the small circle diacritic to vowels to make them voiceless.
constructIPA2 (Vowel x y z Voiceless) =
    constructIPA1 (Vowel x y z Voiced) ++ "̥"

-- If there is no way to express a voiced consonant in a single
-- grapheme add a diacritic to the grapheme that represents
-- the voiceless counterpart.
constructIPA2 (Consonant Voiced x y z) =
   constructIPA1 (Consonant Voiceless x y z) ++ "̼"

constructIPA2 (Vowel x y z Voiced) =
  constructIPA1 (Vowel x y z Voiceless) ++ "̼"


constructIPA2 (Consonant  x PostAlveolar y z) =
    constructIPA1 (Consonant x Alveolar y z) ++ "̠"  -- Add the diacritic for "retracted"


constructIPA2 _ = "∅" -- This return value ( a symbol representing the empty set)
-- is not a full answer. It really means we don't have an answer.
-- We are only using it here so that we can ignore values we have not programmed
-- yet. We just want it to show that we do not have it.

-- | This is a list of the sounds of English. Just the basic ones.
-- | It is somewhat more complicated in reality, but for now this will
-- | suffice.
-- | This following sound inventory of English is from page 20 of
-- | (2013, Elizabeth C. Zsiga, The Sounds of Language)
englishPhonetInventory :: PhonetInventory
englishPhonetInventory = PhonetInventory
  [
  Consonant  Voiced    Bilabial      Plosive   PulmonicEgressive,
  Consonant  Voiceless Bilabial      Plosive   PulmonicEgressive,
  Consonant  Voiced    Alveolar      Plosive   PulmonicEgressive,
  Consonant  Voiceless Alveolar      Plosive   PulmonicEgressive,
  Consonant  Voiced    Velar         Plosive   PulmonicEgressive,
  Consonant  Voiceless Velar         Plosive   PulmonicEgressive,
  Consonant  Voiceless Glottal       Plosive   PulmonicEgressive,
  Consonant  Voiced    LabioDental   Fricative PulmonicEgressive,
  Consonant  Voiceless LabioDental   Fricative PulmonicEgressive,
  Consonant  Voiced    Dental        Fricative PulmonicEgressive,
  Consonant  Voiceless Dental        Fricative PulmonicEgressive,
  Consonant  Voiced    Alveolar      Fricative PulmonicEgressive,
  Consonant  Voiceless Alveolar      Fricative PulmonicEgressive,
  Consonant  Voiced    PostAlveolar  Fricative PulmonicEgressive,
  Consonant  Voiceless PostAlveolar  Fricative PulmonicEgressive,
  Consonant  Voiceless Glottal       Fricative PulmonicEgressive,
  Consonant  Voiced    PostAlveolar  Affricate PulmonicEgressive,
  Consonant  Voiceless PostAlveolar  Affricate PulmonicEgressive,
  Consonant  Voiced    Bilabial      Nasal PulmonicEgressive,
  Consonant  Voiced    Alveolar      Nasal PulmonicEgressive,
  Consonant  Voiced    Velar         Nasal PulmonicEgressive,
  -- The Postalveolar version is technically correct, even though the convention
  -- is to write it in IPA as if it were alveolar. See
  -- Wikipedia article titled "Voiced alveolar and postalveolar approximants"
  -- at the following URL:
  -- https://en.wikipedia.org/wiki/Voiced_alveolar_and_postalveolar_approximants
  Consonant  Voiced PostAlveolar Approximant PulmonicEgressive,
  Consonant  Voiced Palatal      Approximant PulmonicEgressive,
  Consonant  Voiced LabialVelar  Approximant PulmonicEgressive,



  Vowel  Close Front   Unrounded Voiced, -- "i"
  -- English lacks the following vowel: (Vowel  Close Front   Rounded   Voiced) "y"
  -- English lacks (Vowel  Close Central Unrounded Voiced) "ɨ"
  -- constructIPA (Vowel  Close Central Rounded   Voiced) = "ʉ"
  -- English lacks constructIPA (Vowel  Close Back    Unrounded Voiced) = "ɯ"
  Vowel  Close Back    Rounded   Voiced, -- "u"

  -- Near-close Vowels:
  Vowel NearClose Front Unrounded Voiced, -- "ɪ"
  -- English lacks: constructIPA (Vowel NearClose Front Rounded   Voiced) = "ʏ"
  Vowel NearClose Back  Rounded   Voiced, --  "ʊ"

  -- Close-mid Vowels:
  Vowel  CloseMid Front   Unrounded Voiced, -- "e"
  -- English lacks:  Vowel  CloseMid Front   Rounded   Voiced -- "ø"
  -- English lacks:  Vowel  CloseMid Central Unrounded Voiced -- "ɘ"
  -- English lacks: Vowel  CloseMid Central Rounded   Voiced --  "ɵ"
  -- English lacks:  Vowel  CloseMid Back    Unrounded Voiced --  "ɤ"
  Vowel  CloseMid Back    Rounded   Voiced, -- "o"

  -- Mid Vowels:
  Vowel Mid Central UnmarkedRounding Voiced, -- "ə"


  -- Open-mid Vowels:
  Vowel  OpenMid Front   Unrounded Voiced, -- "ɛ"
  -- English lacks: constructIPA (Vowel  OpenMid Front   Rounded   Voiced) = "œ"
  Vowel  OpenMid Central Unrounded Voiced, -- "ɜ"
  --  Vowel  OpenMid Central Rounded   Voiced -- "ɞ"
  Vowel  OpenMid Back    Unrounded Voiced, --  "ʌ"
  Vowel  OpenMid Back    Rounded   Voiced, -- "ɔ"

  -- Near-open
  Vowel  NearOpen Front   Unrounded Voiced, -- "æ"
  Vowel  NearOpen Central UnmarkedRounding  Voiced, -- "ɐ"

  -- Open Vowels:
  -- English lacks: Vowel  Open Front Unrounded Voiced -- "a"
  -- English lacks: Vowel  Open Front Rounded   Voiced --"ɶ"
  Vowel  Open Back  Unrounded Voiced, -- "ɑ"
  Vowel  Open Back  Rounded   Voiced -- "ɒ"

  -- I added some English vowels. I did not choose any specific dialect.
  -- I got all my information from the Wikipedia page titled
  -- "English Phonology"
  -- at the following URL: https://en.wikipedia.org/wiki/English_phonology#Vowels
  -- on Monday, February 24, 2020.
  -- To do: Get better information on English vowels from a more reliable source.
  -- To do: model separate dialects of English or only one.
  ]




voicedPhonet :: Phonet -> Phonet
-- | A function that given an IPA symbol will convert it to the voiced equivalent.
voicedPhonet (Consonant Voiceless x  y z) = Consonant Voiced x y z
voicedPhonet (Consonant Voiced    x  y z) = Consonant Voiced x y z
voicedPhonet (Vowel x y z Voiced   ) = Vowel x y z     Voiced
voicedPhonet (Vowel x y z Voiceless) = Vowel x y z     Voiced

voicedIPA :: IPAText -> IPAText
voicedIPA = constructIPA . voicedPhonet . analyzeIPA


devoicedPhonet :: Phonet -> Phonet
-- | A function that given an IPA symbol will convert it to the voiceless equivalent.
devoicedPhonet (Consonant _ x  y z) = Consonant Voiceless x  y z
devoicedPhonet (Vowel x y z _) = Vowel x y z        Voiceless

devoicedIPA :: IPAText -> IPAText
devoicedIPA = constructIPA . devoicedPhonet . analyzeIPA

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


spirantizedIPA :: IPAText -> IPAText
spirantizedIPA = constructIPA . spirantizedPhonet . analyzeIPA


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


welcome :: IO ()
welcome = putStrLn "Please read README.md file for instructions on how to use."

