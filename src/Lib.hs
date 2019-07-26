module Lib
    ( analyzeIPA,
      someFunc
    ) where

data Phonet = Consonant { place :: Place   -- | Place of articulation
                     , vocalFolds :: VocalFolds
                     , manner :: Manner -- | Manner of articulation
                     , airstream :: Airstream
                     }
           | Vowel { height :: Height
                   , backness :: Backness
                   , rounding :: Rounding
                   }
                   deriving (Eq, Show)

data Backness = Front | Central | Back
                deriving (Eq, Show)
data Height = Close | NearClose | CloseMid | Mid | OpenMid | NearOpen | Open
              deriving (Eq, Show)
data TenseLax = Tense | Lax
                deriving (Eq, Show)
data Rounding = Rounded | Unrounded | Unmarked
                deriving (Eq, Show)

data Place = Bilabial | LabioDental | Dental | Alveolar | PostAlveolar
           | Retroflex
           | Palatal  | Velar  | Uvular | Pharyngeal | Glottal | LabialVelar
           deriving (Eq, Show)

data Manner = Plosive | Nasal | Trill | TapOrFlap | Approximant | Fricative
              | Affricate  -- Affricate is not included on the IPA chart, we may
                           -- want to remove it.
              | LateralFricative
              | LateralApproximant
              deriving (Eq, Show)

data Airstream = PulmonicEgressive
                 deriving (Eq, Show)

data VocalFolds = Voiced | Voiceless
                  deriving (Eq, Show)

data PhonetInventory = PhonetInventory [Phonet]

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
analyzeIPA "w"  = Consonant  LabialVelar Voiced Approximant PulmonicEgressive
analyzeIPA "ɰ"  = Consonant  Velar Voiced Approximant PulmonicEgressive

-- Lateral Approximants:
analyzeIPA "l"  = Consonant  Alveolar Voiced LateralApproximant PulmonicEgressive
analyzeIPA "ɭ"  = Consonant  Retroflex Voiced LateralApproximant PulmonicEgressive
analyzeIPA "ʎ"  = Consonant  Palatal Voiced LateralApproximant PulmonicEgressive
analyzeIPA "ʟ"  = Consonant  Velar Voiced LateralApproximant PulmonicEgressive


-- Close Vowels:
analyzeIPA "i"  = Vowel  Close Front Unrounded
analyzeIPA "y"  = Vowel  Close Front Rounded
analyzeIPA "ɨ"  = Vowel  Close Central Unrounded
analyzeIPA "ʉ"  = Vowel  Close Central Rounded
analyzeIPA "ɯ"  = Vowel  Close Back Unrounded
analyzeIPA "u"  = Vowel  Close Back Rounded

-- Near-close Vowels:
analyzeIPA "ɪ"  = Vowel NearClose Front Unrounded
analyzeIPA "ʏ"  = Vowel NearClose Front Rounded
analyzeIPA "ʊ"  = Vowel NearClose Back  Rounded

-- Close-mid Vowels:
analyzeIPA "e"  = Vowel  CloseMid Front Unrounded
analyzeIPA "ø"  = Vowel  CloseMid Front Rounded
analyzeIPA "ɘ"  = Vowel  CloseMid Central Unrounded
analyzeIPA "ɵ"  = Vowel  CloseMid Central Rounded
analyzeIPA "ɤ"  = Vowel  CloseMid Back Unrounded
analyzeIPA "o"  = Vowel  CloseMid Back Rounded

-- Mid Vowels:
analyzeIPA "ə"  = Vowel Mid Central Unmarked


-- Open-mid Vowels:
analyzeIPA "ɛ"  = Vowel  OpenMid Front Unrounded
analyzeIPA "œ"  = Vowel  OpenMid Front Rounded
analyzeIPA "ɜ"  = Vowel  OpenMid Central Unrounded
analyzeIPA "ɞ"  = Vowel  OpenMid Central Rounded
analyzeIPA "ʌ"  = Vowel  OpenMid Back Unrounded
analyzeIPA "ɔ"  = Vowel  OpenMid Back Rounded

-- Near-open
analyzeIPA "æ"  = Vowel  NearOpen Front Unrounded
analyzeIPA "ɐ"  = Vowel NearOpen Central Unmarked

-- Open Vowels:
analyzeIPA "a"  = Vowel  Open Front Unrounded
analyzeIPA "ɶ"  = Vowel  Open Front Rounded
analyzeIPA "ɑ"  = Vowel  Open Back Unrounded
analyzeIPA "ɒ"  = Vowel  Open Back Rounded


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
    Consonant  Alveolar Voiced Approximant PulmonicEgressive,
    Consonant  PostAlveolar Voiced Approximant PulmonicEgressive,
    Consonant  Palatal Voiced Approximant PulmonicEgressive,
    Consonant  LabialVelar Voiced Approximant PulmonicEgressive
    ]






someFunc :: IO ()
someFunc = putStrLn "Please read README.md file for instructions on how to use."
