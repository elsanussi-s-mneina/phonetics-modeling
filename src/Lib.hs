module Lib where

data Phonet = Consonant { place :: Place   -- | Place of articulation
                        , vocalFolds :: VocalFolds
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
analyzeIPA "p"  = Consonant  Bilabial Voiceless Plosive PulmonicEgressive
analyzeIPA "b"  = Consonant  Bilabial Voiced Plosive PulmonicEgressive
analyzeIPA "t"  = Consonant  Alveolar Voiceless Plosive PulmonicEgressive
analyzeIPA "d"  = Consonant  Alveolar Voiced Plosive PulmonicEgressive
analyzeIPA "ʈ"  = Consonant  Retroflex Voiceless Plosive PulmonicEgressive
analyzeIPA "ɖ"  = Consonant  Retroflex Voiced Plosive PulmonicEgressive
analyzeIPA "c"  = Consonant  Palatal Voiceless Plosive PulmonicEgressive
analyzeIPA "ɟ"  = Consonant  Palatal Voiced Plosive PulmonicEgressive
analyzeIPA "k"  = Consonant  Velar Voiceless Plosive PulmonicEgressive
analyzeIPA "g"  = Consonant  Velar Voiced Plosive PulmonicEgressive
analyzeIPA "q"  = Consonant  Uvular Voiceless Plosive PulmonicEgressive
analyzeIPA "ɢ"  = Consonant  Uvular Voiced Plosive PulmonicEgressive
analyzeIPA "ʔ"  = Consonant  Glottal Voiceless Plosive PulmonicEgressive

-- Nasals:
analyzeIPA "m"  = Consonant  Bilabial Voiced Nasal PulmonicEgressive
analyzeIPA "ɱ"  = Consonant  LabioDental Voiced Nasal PulmonicEgressive
analyzeIPA "n"  = Consonant  Alveolar Voiced Nasal PulmonicEgressive
analyzeIPA "ɳ"  = Consonant  Retroflex Voiced Nasal PulmonicEgressive
analyzeIPA "ɲ"  = Consonant  Palatal Voiced Nasal PulmonicEgressive
analyzeIPA "ŋ"  = Consonant  Velar Voiced Nasal PulmonicEgressive
analyzeIPA "ɴ"  = Consonant  Uvular Voiced Nasal PulmonicEgressive

-- Trills:
analyzeIPA "ʙ"  = Consonant  Bilabial Voiced Trill PulmonicEgressive
analyzeIPA "r"  = Consonant  Alveolar Voiced Trill PulmonicEgressive
analyzeIPA "ʀ"  = Consonant  Uvular Voiced Trill PulmonicEgressive

-- Taps or flaps:
analyzeIPA "ⱱ"  = Consonant  LabioDental Voiced TapOrFlap PulmonicEgressive
analyzeIPA "ɾ"  = Consonant  Alveolar Voiced TapOrFlap PulmonicEgressive
analyzeIPA "ɽ"  = Consonant  Retroflex Voiced TapOrFlap PulmonicEgressive

-- Fricatives:
analyzeIPA "ɸ"  = Consonant  Bilabial Voiceless Fricative PulmonicEgressive
analyzeIPA "β"  = Consonant  Bilabial Voiced Fricative PulmonicEgressive
analyzeIPA "f"  = Consonant  LabioDental Voiceless Fricative PulmonicEgressive
analyzeIPA "v"  = Consonant  LabioDental Voiced Fricative PulmonicEgressive
analyzeIPA "θ"  = Consonant  Dental Voiceless Fricative PulmonicEgressive
analyzeIPA "ð"  = Consonant  Dental Voiced Fricative PulmonicEgressive
analyzeIPA "s"  = Consonant  Alveolar Voiceless Fricative PulmonicEgressive
analyzeIPA "z"  = Consonant  Alveolar Voiced Fricative PulmonicEgressive
analyzeIPA "ʃ"  = Consonant  PostAlveolar Voiceless Fricative PulmonicEgressive
analyzeIPA "ʒ"  = Consonant  PostAlveolar Voiced Fricative PulmonicEgressive
analyzeIPA "ʂ"  = Consonant  Retroflex Voiceless Fricative PulmonicEgressive
analyzeIPA "ʐ"  = Consonant  Retroflex Voiced Fricative PulmonicEgressive
analyzeIPA "ç"  = Consonant  Palatal Voiceless Fricative PulmonicEgressive
analyzeIPA "ʝ"  = Consonant  Palatal Voiced Fricative PulmonicEgressive
analyzeIPA "x"  = Consonant  Velar Voiceless Fricative PulmonicEgressive
analyzeIPA "ɣ"  = Consonant  Velar Voiced Fricative PulmonicEgressive
analyzeIPA "χ"  = Consonant  Uvular Voiceless Fricative PulmonicEgressive
analyzeIPA "ʁ"  = Consonant  Uvular Voiced Fricative PulmonicEgressive
analyzeIPA "ħ"  = Consonant  Pharyngeal Voiceless Fricative PulmonicEgressive
analyzeIPA "ʕ"  = Consonant  Pharyngeal Voiced Fricative PulmonicEgressive
analyzeIPA "h"  = Consonant  Glottal Voiceless Fricative PulmonicEgressive
analyzeIPA "ɦ"  = Consonant  Glottal Voiced Fricative PulmonicEgressive

-- Affricates
analyzeIPA "tʃ" = Consonant  PostAlveolar Voiceless Affricate PulmonicEgressive
analyzeIPA "dʒ" = Consonant  PostAlveolar Voiced Affricate PulmonicEgressive

-- Lateral Fricatives:
analyzeIPA "ɬ" = Consonant Alveolar Voiceless LateralFricative PulmonicEgressive
analyzeIPA "ɮ" = Consonant  Alveolar Voiced LateralFricative PulmonicEgressive


-- Approximants:
analyzeIPA "ʋ"  = Consonant  LabioDental Voiced Approximant PulmonicEgressive
analyzeIPA "ɹ"  = Consonant  PostAlveolar Voiced Approximant PulmonicEgressive
analyzeIPA "ɻ"  = Consonant  Retroflex Voiced Approximant PulmonicEgressive
analyzeIPA "j"  = Consonant  Palatal Voiced Approximant PulmonicEgressive
analyzeIPA "ɰ"  = Consonant  Velar Voiced Approximant PulmonicEgressive

-- Lateral Approximants:
analyzeIPA "l"  = Consonant  Alveolar Voiced LateralApproximant PulmonicEgressive
analyzeIPA "ɭ"  = Consonant  Retroflex Voiced LateralApproximant PulmonicEgressive
analyzeIPA "ʎ"  = Consonant  Palatal Voiced LateralApproximant PulmonicEgressive
analyzeIPA "ʟ"  = Consonant  Velar Voiced LateralApproximant PulmonicEgressive


-- Under the Other Symbols part of the IPA chart:

analyzeIPA "w"  = Consonant  LabialVelar Voiced Approximant PulmonicEgressive

analyzeIPA "ʍ" = Consonant LabialVelar Voiceless Fricative PulmonicEgressive
analyzeIPA "ɥ" = Consonant LabialPalatal Voiced Approximant PulmonicEgressive
analyzeIPA "ʜ" = Consonant Epiglottal Voiceless Fricative PulmonicEgressive
analyzeIPA "ʢ" = Consonant Epiglottal Voiced Fricative PulmonicEgressive
analyzeIPA "ʡ" = Consonant Epiglottal Voiceless Plosive PulmonicEgressive
-- Is the epiglottal plosive voiceless? The IPA chart does not specify.

analyzeIPA "ɕ" = Consonant AlveoloPalatal Voiceless Fricative PulmonicEgressive
analyzeIPA "ʑ" = Consonant AlveoloPalatal Voiced Fricative PulmonicEgressive
analyzeIPA "ɺ" = Consonant Alveolar Voiced LateralFlap PulmonicEgressive

-- We cannot handle the ɧ (simultaneous ʃ and x) because
-- we did not define our data types to handle it yet.
-- analyzeIPA "ɧ" = simultaneous (analyzeIPA "ʃ") (analyzeIPA "x")

-- Other Consonants:
analyzeIPA "ʘ" = Consonant Bilabial UnmarkedVocalFolds UnmarkedManner Click
analyzeIPA "ǀ" = Consonant Dental UnmarkedVocalFolds UnmarkedManner  Click
analyzeIPA "ǃ" = Consonant Alveolar UnmarkedVocalFolds UnmarkedManner Click -- Or it could be PostAlveolar.
analyzeIPA "ǂ" = Consonant PalatoAlveolar UnmarkedVocalFolds UnmarkedManner Click
analyzeIPA "ǁ" = Consonant Alveolar UnmarkedVocalFolds Lateral Click
analyzeIPA "ɓ" = Consonant Bilabial Voiced UnmarkedManner Implosive
analyzeIPA "ɗ" = Consonant Dental Voiced UnmarkedManner Implosive  -- Or Alveolar
analyzeIPA "ʄ" = Consonant Palatal Voiced UnmarkedManner Implosive
analyzeIPA "ɠ" = Consonant Velar Voiced UnmarkedManner Implosive
analyzeIPA "ʛ" = Consonant Uvular Voiced UnmarkedManner Implosive

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
constructIPA (Consonant  Bilabial  Voiceless Plosive PulmonicEgressive) = "p"
constructIPA (Consonant  Bilabial  Voiced    Plosive PulmonicEgressive) = "b"
constructIPA (Consonant  Alveolar  Voiceless Plosive PulmonicEgressive) = "t"
constructIPA (Consonant  Alveolar  Voiced    Plosive PulmonicEgressive) = "d"
constructIPA (Consonant  Retroflex Voiceless Plosive PulmonicEgressive) = "ʈ"
constructIPA (Consonant  Retroflex Voiced    Plosive PulmonicEgressive) = "ɖ"
constructIPA (Consonant  Palatal   Voiceless Plosive PulmonicEgressive) = "c"
constructIPA (Consonant  Palatal   Voiced    Plosive PulmonicEgressive) = "ɟ"
constructIPA (Consonant  Velar     Voiceless Plosive PulmonicEgressive) = "k"
constructIPA (Consonant  Velar     Voiced    Plosive PulmonicEgressive) = "g"
constructIPA (Consonant  Uvular    Voiceless Plosive PulmonicEgressive) = "q"
constructIPA (Consonant  Uvular    Voiced    Plosive PulmonicEgressive) = "ɢ"
constructIPA (Consonant  Glottal   Voiceless Plosive PulmonicEgressive) = "ʔ"

-- Nasals:
constructIPA (Consonant  Bilabial    Voiced Nasal PulmonicEgressive) = "m"
constructIPA (Consonant  LabioDental Voiced Nasal PulmonicEgressive) = "ɱ"
constructIPA (Consonant  Alveolar    Voiced Nasal PulmonicEgressive) = "n"
constructIPA (Consonant  Retroflex   Voiced Nasal PulmonicEgressive) = "ɳ"
constructIPA (Consonant  Palatal     Voiced Nasal PulmonicEgressive) = "ɲ"
constructIPA (Consonant  Velar       Voiced Nasal PulmonicEgressive) = "ŋ"
constructIPA (Consonant  Uvular      Voiced Nasal PulmonicEgressive) = "ɴ"

-- Trills:
constructIPA (Consonant  Bilabial Voiced Trill PulmonicEgressive) = "ʙ"
constructIPA (Consonant  Alveolar Voiced Trill PulmonicEgressive) = "r"
constructIPA (Consonant  Uvular   Voiced Trill PulmonicEgressive) = "ʀ"

-- Taps or flaps:
constructIPA (Consonant  LabioDental Voiced TapOrFlap PulmonicEgressive) = "ⱱ"
constructIPA (Consonant  Alveolar    Voiced TapOrFlap PulmonicEgressive) = "ɾ"
constructIPA (Consonant  Retroflex   Voiced TapOrFlap PulmonicEgressive) = "ɽ"

-- Fricatives:
constructIPA (Consonant  Bilabial Voiceless Fricative     PulmonicEgressive) = "ɸ"
constructIPA (Consonant  Bilabial Voiced Fricative        PulmonicEgressive) = "β"
constructIPA (Consonant  LabioDental Voiceless Fricative  PulmonicEgressive) = "f"
constructIPA (Consonant  LabioDental Voiced Fricative     PulmonicEgressive) = "v"
constructIPA (Consonant  Dental Voiceless Fricative       PulmonicEgressive) = "θ"
constructIPA (Consonant  Dental Voiced Fricative          PulmonicEgressive) = "ð"
constructIPA (Consonant  Alveolar Voiceless Fricative     PulmonicEgressive) = "s"
constructIPA (Consonant  Alveolar Voiced Fricative        PulmonicEgressive) = "z"
constructIPA (Consonant  PostAlveolar Voiceless Fricative PulmonicEgressive) = "ʃ"
constructIPA (Consonant  PostAlveolar Voiced Fricative    PulmonicEgressive) = "ʒ"
constructIPA (Consonant  Retroflex Voiceless Fricative    PulmonicEgressive) = "ʂ"
constructIPA (Consonant  Retroflex Voiced Fricative       PulmonicEgressive) = "ʐ"
constructIPA (Consonant  Palatal Voiceless Fricative      PulmonicEgressive) = "ç"
constructIPA (Consonant  Palatal Voiced Fricative         PulmonicEgressive) = "ʝ"
constructIPA (Consonant  Velar Voiceless Fricative        PulmonicEgressive) = "x"
constructIPA (Consonant  Velar Voiced Fricative           PulmonicEgressive) = "ɣ"
constructIPA (Consonant  Uvular Voiceless Fricative       PulmonicEgressive) = "χ"
constructIPA (Consonant  Uvular Voiced Fricative          PulmonicEgressive) = "ʁ"
constructIPA (Consonant  Pharyngeal Voiceless Fricative   PulmonicEgressive) = "ħ"
constructIPA (Consonant  Pharyngeal Voiced Fricative      PulmonicEgressive) = "ʕ"
constructIPA (Consonant  Glottal Voiceless Fricative      PulmonicEgressive) = "h"
constructIPA (Consonant  Glottal Voiced Fricative         PulmonicEgressive) = "ɦ"

-- Affricates
constructIPA (Consonant  PostAlveolar Voiceless Affricate PulmonicEgressive) = "tʃ"
constructIPA (Consonant  PostAlveolar Voiced    Affricate PulmonicEgressive) = "dʒ"

-- Lateral Fricatives:
constructIPA (Consonant  Alveolar Voiceless LateralFricative PulmonicEgressive) = "ɬ"
constructIPA (Consonant  Alveolar Voiced    LateralFricative PulmonicEgressive) = "ɮ"


-- Approximants:
constructIPA (Consonant  LabioDental  Voiced Approximant PulmonicEgressive) = "ʋ"
constructIPA (Consonant  Alveolar Voiced Approximant PulmonicEgressive) = "ɹ"

  -- To do: find a way to express, ɹ̠ with multiple functions instead of
  -- here as a single line, because this will not work for other underbar
  -- phonemes.

constructIPA (Consonant  Retroflex    Voiced Approximant PulmonicEgressive) = "ɻ"
constructIPA (Consonant  Palatal      Voiced Approximant PulmonicEgressive) = "j"
constructIPA (Consonant  LabialVelar  Voiced Approximant PulmonicEgressive) = "w"
constructIPA (Consonant  Velar        Voiced Approximant PulmonicEgressive) = "ɰ"

-- Lateral Approximants:
constructIPA (Consonant  Alveolar  Voiced LateralApproximant PulmonicEgressive) = "l"
constructIPA (Consonant  Retroflex Voiced LateralApproximant PulmonicEgressive) = "ɭ"
constructIPA (Consonant  Palatal   Voiced LateralApproximant PulmonicEgressive) = "ʎ"
constructIPA (Consonant  Velar     Voiced LateralApproximant PulmonicEgressive) = "ʟ"

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
constructIPA (Consonant  x Voiceless y z) =
  constructIPA (Consonant x Voiced y z) ++ "\805"

-- Add the small circle diacritic to vowels to make them voiceless.
constructIPA (Vowel x y z Voiceless) =
    constructIPA (Vowel x y z Voiced) ++ "\805"


constructIPA (Consonant  PostAlveolar x y z) =
    constructIPA (Consonant Alveolar x y z) ++ "̠"


-- | This is a list of the sounds of English. Just the basic ones.
-- | It is somewhat more complicated in reality, but for now this will
-- | suffice.
-- | This following sound inventory of English is from page 20 of
-- | (2013, Elizabeth C. Zsiga, The Sounds of Language)
englishPhonetInventory :: PhonetInventory
englishPhonetInventory = PhonetInventory
  [
  Consonant  Bilabial Voiced Plosive PulmonicEgressive,
  Consonant  Bilabial Voiceless Plosive PulmonicEgressive,
  Consonant  Alveolar Voiced Plosive PulmonicEgressive,
  Consonant  Alveolar Voiceless Plosive PulmonicEgressive,
  Consonant  Velar Voiced Plosive PulmonicEgressive,
  Consonant  Velar Voiceless Plosive PulmonicEgressive,
  Consonant  Glottal Voiceless Plosive PulmonicEgressive,
  Consonant  LabioDental Voiced Fricative PulmonicEgressive,
  Consonant  LabioDental Voiceless Fricative PulmonicEgressive,
  Consonant  Dental Voiced Fricative PulmonicEgressive,
  Consonant  Dental Voiceless Fricative PulmonicEgressive,
  Consonant  Alveolar Voiced Fricative PulmonicEgressive,
  Consonant  Alveolar Voiceless Fricative PulmonicEgressive,
  Consonant  PostAlveolar Voiced Fricative PulmonicEgressive,
  Consonant  PostAlveolar Voiceless Fricative PulmonicEgressive,
  Consonant  Glottal Voiceless Fricative PulmonicEgressive,
  Consonant  PostAlveolar Voiced Affricate PulmonicEgressive,
  Consonant  PostAlveolar Voiceless Affricate PulmonicEgressive,
  Consonant  Bilabial Voiced Nasal PulmonicEgressive,
  Consonant  Alveolar Voiced Nasal PulmonicEgressive,
  Consonant  Velar Voiced Nasal PulmonicEgressive,
  Consonant  Alveolar Voiced Approximant PulmonicEgressive, -- This line should probably be removed.
  -- The Postalveolar version is technically correct, even though the convention
  -- is to write it in IPA as if it were alveolar. See
  -- Wikipedia article titled "Voiced alveolar and postalveolar approximants"
  -- at the following URL:
  -- https://en.wikipedia.org/wiki/Voiced_alveolar_and_postalveolar_approximants
  Consonant  PostAlveolar Voiced Approximant PulmonicEgressive,
  Consonant  Palatal Voiced Approximant PulmonicEgressive,
  Consonant  LabialVelar Voiced Approximant PulmonicEgressive,



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
voicedPhonet (Consonant x Voiceless y z) = (Consonant x Voiced y z)
voicedPhonet (Consonant x Voiced y z) = (Consonant x    Voiced y z)
voicedPhonet (Vowel x y z Voiced) = (Vowel x y z        Voiced)
voicedPhonet (Vowel x y z Voiceless) = (Vowel x y z     Voiced)

voicedIPA :: IPAText -> IPAText
voicedIPA = constructIPA . voicedPhonet . analyzeIPA


devoicedPhonet :: Phonet -> Phonet
-- | A function that given an IPA symbol will convert it to the voiceless equivalent.
devoicedPhonet (Consonant x Voiceless y z) = (Consonant x Voiceless y z)
devoicedPhonet (Consonant x Voiced y z) = (Consonant x    Voiceless y z)
devoicedPhonet (Vowel x y z Voiced) = (Vowel x y z        Voiceless)
devoicedPhonet (Vowel x y z Voiceless) = (Vowel x y z     Voiceless)

devoicedIPA :: IPAText -> IPAText
devoicedIPA = constructIPA . devoicedPhonet . analyzeIPA

spirantizedPhonet :: Phonet -> Phonet
spirantizedPhonet (Consonant x y Plosive z) | x /= Alveolar
  = (Consonant x y Fricative z)

-- The following is inelegant, but there is no other way in the system,
-- right now.
spirantizedPhonet (Consonant Alveolar x Plosive z) | otherwise =
  (Consonant Dental x Fricative z)


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
