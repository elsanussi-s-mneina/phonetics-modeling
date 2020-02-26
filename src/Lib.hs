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
data Height = Close | NearClose | CloseMid | Mid | OpenMid | NearOpen | Open | UnmarkedHeight
              deriving (Eq, Show)
data TenseLax = Tense | Lax | UnmarkedTenseness
                deriving (Eq, Show)
data Rounding = Rounded | Unrounded | UnmarkedRounding
                deriving (Eq, Show)

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

data Manner = Plosive | Nasal | Trill | TapOrFlap | Approximant | Fricative
              | Affricate  -- Affricate is not included on the IPA chart, we may
                           -- want to remove it.
              | LateralFricative
              | LateralApproximant
              | LateralFlap
              | Lateral -- we need this one for the lateral click.
              | UnmarkedManner -- There are very few IPA symbols for lateral flaps
              deriving (Eq, Show)

data Airstream = PulmonicEgressive | Click | Implosive | UnmarkedAirstream
                 deriving (Eq, Show)

data VocalFolds = Voiced | Voiceless | UnmarkedVocalFolds
                  deriving (Eq, Show)

data PhonetInventory = PhonetInventory [Phonet]


instance Show PhonetInventory where
    show (PhonetInventory phonetes) = foldr (++) "" (map constructIPA phonetes)


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
analyzeIPA "tʃ" = Consonant  Voiceless PostAlveolar Affricate PulmonicEgressive
analyzeIPA "dʒ" = Consonant  Voiced PostAlveolar Affricate PulmonicEgressive
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


-- | given an analysis construct an IPA symbol
-- | This function will allow us to convert an analyzed form
-- | to its IPA symbol(s).
constructIPA  ::  Phonet -> IPAText
-- Plosives:
constructIPA (Consonant  Voiced    Bilabial  Plosive PulmonicEgressive) = "b"
constructIPA (Consonant  Voiceless Bilabial  Plosive PulmonicEgressive) = "p"
constructIPA (Consonant  Voiceless Alveolar  Plosive PulmonicEgressive) = "t"
constructIPA (Consonant  Voiced    Alveolar  Plosive PulmonicEgressive) = "d"
constructIPA (Consonant  Voiceless Retroflex Plosive PulmonicEgressive) = "ʈ"
constructIPA (Consonant  Voiced    Retroflex Plosive PulmonicEgressive) = "ɖ"
constructIPA (Consonant  Voiceless Palatal   Plosive PulmonicEgressive) = "c"
constructIPA (Consonant  Voiced    Palatal   Plosive PulmonicEgressive) = "ɟ"
constructIPA (Consonant  Voiceless Velar     Plosive PulmonicEgressive) = "k"
constructIPA (Consonant  Voiced    Velar     Plosive PulmonicEgressive) = "g"
constructIPA (Consonant  Voiceless Uvular    Plosive PulmonicEgressive) = "q"
constructIPA (Consonant  Voiced    Uvular    Plosive PulmonicEgressive) = "ɢ"
constructIPA (Consonant  Voiceless Glottal   Plosive PulmonicEgressive) = "ʔ"

-- Nasals:
constructIPA (Consonant  Voiced    Bilabial    Nasal PulmonicEgressive) = "m"
constructIPA (Consonant  Voiced    LabioDental Nasal PulmonicEgressive) = "ɱ"
constructIPA (Consonant  Voiced    Alveolar    Nasal PulmonicEgressive) = "n"
constructIPA (Consonant  Voiced    Retroflex   Nasal PulmonicEgressive) = "ɳ"
constructIPA (Consonant  Voiced    Palatal     Nasal PulmonicEgressive) = "ɲ"
constructIPA (Consonant  Voiced    Velar       Nasal PulmonicEgressive) = "ŋ"
constructIPA (Consonant  Voiced    Uvular      Nasal PulmonicEgressive) = "ɴ"

-- Trills:
constructIPA (Consonant  Voiced   Bilabial    Trill PulmonicEgressive) = "ʙ"
constructIPA (Consonant  Voiced   Alveolar    Trill PulmonicEgressive) = "r"
constructIPA (Consonant  Voiced   Uvular      Trill PulmonicEgressive) = "ʀ" -- Taps or flaps:
constructIPA (Consonant  Voiced   LabioDental TapOrFlap PulmonicEgressive) = "ⱱ"
constructIPA (Consonant  Voiced   Alveolar    TapOrFlap PulmonicEgressive) = "ɾ"
constructIPA (Consonant  Voiced   Retroflex   TapOrFlap PulmonicEgressive) = "ɽ"

-- Fricatives:
constructIPA (Consonant  Voiceless Bilabial       Fricative     PulmonicEgressive) = "ɸ"
constructIPA (Consonant  Voiced    Bilabial       Fricative        PulmonicEgressive) = "β"
constructIPA (Consonant  Voiceless LabioDental    Fricative  PulmonicEgressive) = "f"
constructIPA (Consonant  Voiced    LabioDental    Fricative     PulmonicEgressive) = "v"
constructIPA (Consonant  Voiceless Dental         Fricative       PulmonicEgressive) = "θ"
constructIPA (Consonant  Voiced    Dental         Fricative          PulmonicEgressive) = "ð"
constructIPA (Consonant  Voiceless Alveolar       Fricative     PulmonicEgressive) = "s"
constructIPA (Consonant  Voiced    Alveolar       Fricative        PulmonicEgressive) = "z"
constructIPA (Consonant  Voiceless PostAlveolar   Fricative PulmonicEgressive) = "ʃ"
constructIPA (Consonant  Voiced    PostAlveolar   Fricative    PulmonicEgressive) = "ʒ"
constructIPA (Consonant  Voiceless Retroflex      Fricative    PulmonicEgressive) = "ʂ"
constructIPA (Consonant  Voiced    Retroflex      Fricative       PulmonicEgressive) = "ʐ"
constructIPA (Consonant  Voiceless Palatal        Fricative      PulmonicEgressive) = "ç"
constructIPA (Consonant  Voiced    Palatal        Fricative         PulmonicEgressive) = "ʝ"
constructIPA (Consonant  Voiceless Velar          Fricative        PulmonicEgressive) = "x"
constructIPA (Consonant  Voiced    Velar          Fricative           PulmonicEgressive) = "ɣ"
constructIPA (Consonant  Voiceless Uvular         Fricative       PulmonicEgressive) = "χ"
constructIPA (Consonant  Voiced    Uvular         Fricative          PulmonicEgressive) = "ʁ"
constructIPA (Consonant  Voiceless Pharyngeal     Fricative   PulmonicEgressive) = "ħ"
constructIPA (Consonant  Voiced    Pharyngeal     Fricative      PulmonicEgressive) = "ʕ"
constructIPA (Consonant  Voiceless Glottal        Fricative      PulmonicEgressive) = "h"
constructIPA (Consonant  Voiced    Glottal        Fricative         PulmonicEgressive) = "ɦ"

-- Affricates
constructIPA (Consonant  Voiceless PostAlveolar  Affricate PulmonicEgressive) = "tʃ"
constructIPA (Consonant  Voiced    PostAlveolar  Affricate PulmonicEgressive) = "dʒ"

-- Lateral Fricatives:
constructIPA (Consonant  Voiceless Alveolar LateralFricative PulmonicEgressive) = "ɬ"
constructIPA (Consonant  Voiced    Alveolar LateralFricative PulmonicEgressive) = "ɮ"


-- Approximants:
constructIPA (Consonant  Voiced LabioDental   Approximant PulmonicEgressive) = "ʋ"
constructIPA (Consonant  Voiced Alveolar      Approximant PulmonicEgressive) = "ɹ"

  -- To do: find a way to express, ɹ̠ with multiple functions instead of
  -- here as a single line, because this will not work for other underbar
  -- phonemes.

constructIPA (Consonant  Voiced  Retroflex    Approximant PulmonicEgressive) = "ɻ"
constructIPA (Consonant  Voiced  Palatal      Approximant PulmonicEgressive) = "j"
constructIPA (Consonant  Voiced  LabialVelar  Approximant PulmonicEgressive) = "w"
constructIPA (Consonant  Voiced  Velar        Approximant PulmonicEgressive) = "ɰ"

-- Lateral Approximants:
constructIPA (Consonant  Voiced Alveolar   LateralApproximant PulmonicEgressive) = "l"
constructIPA (Consonant  Voiced Retroflex  LateralApproximant PulmonicEgressive) = "ɭ"
constructIPA (Consonant  Voiced Palatal    LateralApproximant PulmonicEgressive) = "ʎ"
constructIPA (Consonant  Voiced Velar      LateralApproximant PulmonicEgressive) = "ʟ"

-- Close Vowels:
constructIPA (Vowel  Close Front   Unrounded Voiced) = "i"
constructIPA (Vowel  Close Front   Rounded   Voiced) = "y"
constructIPA (Vowel  Close Central Unrounded Voiced) = "ɨ"
constructIPA (Vowel  Close Central Rounded   Voiced) = "ʉ"
constructIPA (Vowel  Close Back    Unrounded Voiced) = "ɯ"
constructIPA (Vowel  Close Back    Rounded   Voiced) = "u"

-- Near-close Vowels:
constructIPA (Vowel NearClose Front Unrounded Voiced) = "ɪ"
constructIPA (Vowel NearClose Front Rounded   Voiced) = "ʏ"
constructIPA (Vowel NearClose Back  Rounded   Voiced) = "ʊ"

-- Close-mid Vowels:
constructIPA (Vowel  CloseMid Front   Unrounded Voiced) = "e"
constructIPA (Vowel  CloseMid Front   Rounded   Voiced) = "ø"
constructIPA (Vowel  CloseMid Central Unrounded Voiced) = "ɘ"
constructIPA (Vowel  CloseMid Central Rounded   Voiced) = "ɵ"
constructIPA (Vowel  CloseMid Back    Unrounded Voiced) = "ɤ"
constructIPA (Vowel  CloseMid Back    Rounded   Voiced) = "o"

-- Mid Vowels:
constructIPA (Vowel Mid Central UnmarkedRounding Voiced) = "ə"


-- Open-mid Vowels:
constructIPA (Vowel  OpenMid Front   Unrounded Voiced) = "ɛ"
constructIPA (Vowel  OpenMid Front   Rounded   Voiced) = "œ"
constructIPA (Vowel  OpenMid Central Unrounded Voiced) = "ɜ"
constructIPA (Vowel  OpenMid Central Rounded   Voiced) = "ɞ"
constructIPA (Vowel  OpenMid Back    Unrounded Voiced) = "ʌ"
constructIPA (Vowel  OpenMid Back    Rounded   Voiced) = "ɔ"

-- Near-open
constructIPA (Vowel  NearOpen Front   Unrounded Voiced) = "æ"
constructIPA (Vowel  NearOpen Central UnmarkedRounding  Voiced) = "ɐ"

-- Open Vowels:
constructIPA (Vowel  Open Front Unrounded Voiced) = "a"
constructIPA (Vowel  Open Front Rounded   Voiced) = "ɶ"
constructIPA (Vowel  Open Back  Unrounded Voiced) = "ɑ"
constructIPA (Vowel  Open Back  Rounded   Voiced) = "ɒ"

-- If there isn't a symbol, and the consonant we want is voiceless,
-- Just take the symbol for a voiced consonant,
-- and then put that diacritic that means voiceless after.
-- \805 is the code for that small circle diacritic that goes under
-- a character to mean voiceless. See https://linguistlist.org/unicode/ipa.html
-- (The following two definitions are intended to implmeent that)
-- Add the small circle diacritic to consonants to make them voiceless.
constructIPA (Consonant Voiceless x y z) =
  constructIPA (Consonant Voiced x y z) ++ "\805"

-- Add the small circle diacritic to vowels to make them voiceless.
constructIPA (Vowel x y z Voiceless) =
    constructIPA (Vowel x y z Voiced) ++ "\805"


constructIPA (Consonant  x PostAlveolar y z) =
    constructIPA (Consonant x Alveolar y z) ++ "̠"


-- | This is a list of the sounds of English. Just the basic ones.
-- | It is somewhat more complicated in reality, but for now this will
-- | suffice.
-- | This following sound inventory of English is from page 20 of
-- | (2013, Elizabeth C. Zsiga, The Sounds of Language)
englishPhonetInventory :: PhonetInventory
englishPhonetInventory = PhonetInventory
  [
  Consonant  Voiced    Bilabial      Plosive PulmonicEgressive,
  Consonant  Voiceless Bilabial      Plosive PulmonicEgressive,
  Consonant  Voiced    Alveolar      Plosive PulmonicEgressive,
  Consonant  Voiceless Alveolar      Plosive PulmonicEgressive,
  Consonant  Voiced    Velar         Plosive PulmonicEgressive,
  Consonant  Voiceless Velar         Plosive PulmonicEgressive,
  Consonant  Voiceless Glottal       Plosive PulmonicEgressive,
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
  Consonant  Voiced    Alveolar      Approximant PulmonicEgressive, -- This line should probably be removed.
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
voicedPhonet (Consonant Voiceless x  y z) = (Consonant Voiced x y z)
voicedPhonet (Consonant Voiced    x  y z) = (Consonant Voiced x y z)
voicedPhonet (Vowel x y z Voiced   ) = (Vowel x y z     Voiced)
voicedPhonet (Vowel x y z Voiceless) = (Vowel x y z     Voiced)

voicedIPA :: IPAText -> IPAText
voicedIPA = constructIPA . voicedPhonet . analyzeIPA


devoicedPhonet :: Phonet -> Phonet
-- | A function that given an IPA symbol will convert it to the voiceless equivalent.
devoicedPhonet (Consonant Voiceless x  y z) = (Consonant Voiceless x  y z)
devoicedPhonet (Consonant Voiced x y z) = (Consonant Voiceless x y z)
devoicedPhonet (Vowel x y z Voiced) = (Vowel x y z        Voiceless)
devoicedPhonet (Vowel x y z Voiceless) = (Vowel x y z     Voiceless)

devoicedIPA :: IPAText -> IPAText
devoicedIPA = constructIPA . devoicedPhonet . analyzeIPA

spirantizedPhonet :: Phonet -> Phonet
spirantizedPhonet (Consonant x place Plosive z) | place /= Alveolar
  = (Consonant x place Fricative z)

-- The following is inelegant, but there is no other way in the system,
-- right now.
spirantizedPhonet (Consonant x Alveolar Plosive z) | otherwise =
  (Consonant x Dental Fricative z)


spirantizedIPA :: IPAText -> IPAText
spirantizedIPA = constructIPA . spirantizedPhonet . analyzeIPA



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
