module Lib
    ( analyzeIPA,
      someFunc
    ) where

data Phonet = Phonet { place :: Place   -- | Place of articulation
                     , vocalFolds :: VocalFolds
                     , manner :: Manner -- | Manner of articulation
                     , airstream :: Airstream
                     }
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
analyzeIPA "p"  = Phonet  Bilabial Voiceless Plosive PulmonicEgressive
analyzeIPA "b"  = Phonet  Bilabial Voiced Plosive PulmonicEgressive
analyzeIPA "t"  = Phonet  Alveolar Voiceless Plosive PulmonicEgressive
analyzeIPA "d"  = Phonet  Alveolar Voiced Plosive PulmonicEgressive
analyzeIPA "ʈ"  = Phonet  Retroflex Voiceless Plosive PulmonicEgressive
analyzeIPA "ɖ"  = Phonet  Retroflex Voiced Plosive PulmonicEgressive
analyzeIPA "c"  = Phonet  Palatal Voiceless Plosive PulmonicEgressive
analyzeIPA "ɟ"  = Phonet  Palatal Voiced Plosive PulmonicEgressive
analyzeIPA "k"  = Phonet  Velar Voiceless Plosive PulmonicEgressive
analyzeIPA "g"  = Phonet  Velar Voiced Plosive PulmonicEgressive
analyzeIPA "q"  = Phonet  Uvular Voiceless Plosive PulmonicEgressive
analyzeIPA "ɢ"  = Phonet  Uvular Voiced Plosive PulmonicEgressive
analyzeIPA "ʔ"  = Phonet  Glottal Voiceless Plosive PulmonicEgressive

-- Nasals:
analyzeIPA "m"  = Phonet  Bilabial Voiced Nasal PulmonicEgressive
analyzeIPA "ɱ"  = Phonet  LabioDental Voiced Nasal PulmonicEgressive
analyzeIPA "n"  = Phonet  Alveolar Voiced Nasal PulmonicEgressive
analyzeIPA "ɳ"  = Phonet  Retroflex Voiced Nasal PulmonicEgressive
analyzeIPA "ɲ"  = Phonet  Palatal Voiced Nasal PulmonicEgressive
analyzeIPA "ŋ"  = Phonet  Velar Voiced Nasal PulmonicEgressive
analyzeIPA "ɴ"  = Phonet  Uvular Voiced Nasal PulmonicEgressive

-- Trills:
analyzeIPA "ʙ"  = Phonet  Bilabial Voiced Trill PulmonicEgressive
analyzeIPA "r"  = Phonet  Alveolar Voiced Trill PulmonicEgressive
analyzeIPA "ʀ"  = Phonet  Uvular Voiced Trill PulmonicEgressive

-- Taps or flaps:
analyzeIPA "ⱱ"  = Phonet  LabioDental Voiced TapOrFlap PulmonicEgressive
analyzeIPA "ɾ"  = Phonet  Alveolar Voiced TapOrFlap PulmonicEgressive
analyzeIPA "ɽ"  = Phonet  Retroflex Voiced TapOrFlap PulmonicEgressive

-- Fricatives:
analyzeIPA "ɸ"  = Phonet  Bilabial Voiceless Fricative PulmonicEgressive
analyzeIPA "β"  = Phonet  Bilabial Voiced Fricative PulmonicEgressive
analyzeIPA "f"  = Phonet  LabioDental Voiceless Fricative PulmonicEgressive
analyzeIPA "v"  = Phonet  LabioDental Voiced Fricative PulmonicEgressive
analyzeIPA "θ"  = Phonet  Dental Voiceless Fricative PulmonicEgressive
analyzeIPA "ð"  = Phonet  Dental Voiced Fricative PulmonicEgressive
analyzeIPA "s"  = Phonet  Alveolar Voiceless Fricative PulmonicEgressive
analyzeIPA "z"  = Phonet  Alveolar Voiced Fricative PulmonicEgressive
analyzeIPA "ʃ"  = Phonet  PostAlveolar Voiceless Fricative PulmonicEgressive
analyzeIPA "ʒ"  = Phonet  PostAlveolar Voiced Fricative PulmonicEgressive
analyzeIPA "ʂ"  = Phonet  Retroflex Voiceless Fricative PulmonicEgressive
analyzeIPA "ʐ"  = Phonet  Retroflex Voiced Fricative PulmonicEgressive
analyzeIPA "ç"  = Phonet  Palatal Voiceless Fricative PulmonicEgressive
analyzeIPA "ʝ"  = Phonet  Palatal Voiced Fricative PulmonicEgressive
analyzeIPA "x"  = Phonet  Velar Voiceless Fricative PulmonicEgressive
analyzeIPA "ɣ"  = Phonet  Velar Voiced Fricative PulmonicEgressive
analyzeIPA "χ"  = Phonet  Uvular Voiceless Fricative PulmonicEgressive
analyzeIPA "ʁ"  = Phonet  Uvular Voiced Fricative PulmonicEgressive
analyzeIPA "ħ"  = Phonet  Pharyngeal Voiceless Fricative PulmonicEgressive
analyzeIPA "ʕ"  = Phonet  Pharyngeal Voiced Fricative PulmonicEgressive
analyzeIPA "h"  = Phonet  Glottal Voiceless Fricative PulmonicEgressive
analyzeIPA "ɦ"  = Phonet  Glottal Voiced Fricative PulmonicEgressive

-- Affricates
analyzeIPA "tʃ" = Phonet  PostAlveolar Voiceless Affricate PulmonicEgressive
analyzeIPA "dʒ" = Phonet  PostAlveolar Voiced Affricate PulmonicEgressive

-- Lateral Fricatives:
analyzeIPA "ɬ" = Phonet Alveolar Voiceless LateralFricative PulmonicEgressive
analyzeIPA "ɮ" = Phonet  Alveolar Voiced LateralFricative PulmonicEgressive


-- Approximants:
analyzeIPA "ʋ"  = Phonet  LabioDental Voiced Approximant PulmonicEgressive
analyzeIPA "ɹ"  = Phonet  PostAlveolar Voiced Approximant PulmonicEgressive
analyzeIPA "ɻ"  = Phonet  Retroflex Voiced Approximant PulmonicEgressive
analyzeIPA "j"  = Phonet  Palatal Voiced Approximant PulmonicEgressive
analyzeIPA "w"  = Phonet  LabialVelar Voiced Approximant PulmonicEgressive
analyzeIPA "ɰ"  = Phonet  Velar Voiced Approximant PulmonicEgressive

-- Lateral Approximants:
analyzeIPA "l"  = Phonet  Alveolar Voiced LateralApproximant PulmonicEgressive
analyzeIPA "ɭ"  = Phonet  Retroflex Voiced LateralApproximant PulmonicEgressive
analyzeIPA "ʎ"  = Phonet  Palatal Voiced LateralApproximant PulmonicEgressive
analyzeIPA "ʟ"  = Phonet  Velar Voiced LateralApproximant PulmonicEgressive



-- | This is a list of the sounds of English. Just the basic ones.
-- | It is somewhat more complicated in reality, but for now this will
-- | suffice.
-- | This following sound inventory of English is from page 20 of
-- | (2013, Elizabeth C. Zsiga, The Sounds of Language)
englishPhonetInventory :: PhonetInventory
englishPhonetInventory = PhonetInventory
  [
    Phonet  Bilabial Voiced Plosive PulmonicEgressive,
    Phonet  Bilabial Voiceless Plosive PulmonicEgressive,
    Phonet  Alveolar Voiced Plosive PulmonicEgressive,
    Phonet  Alveolar Voiceless Plosive PulmonicEgressive,
    Phonet  Velar Voiced Plosive PulmonicEgressive,
    Phonet  Velar Voiceless Plosive PulmonicEgressive,
    Phonet  Glottal Voiceless Plosive PulmonicEgressive,
    Phonet  LabioDental Voiced Fricative PulmonicEgressive,
    Phonet  LabioDental Voiceless Fricative PulmonicEgressive,
    Phonet  Dental Voiced Fricative PulmonicEgressive,
    Phonet  Dental Voiceless Fricative PulmonicEgressive,
    Phonet  Alveolar Voiced Fricative PulmonicEgressive,
    Phonet  Alveolar Voiceless Fricative PulmonicEgressive,
    Phonet  PostAlveolar Voiced Fricative PulmonicEgressive,
    Phonet  PostAlveolar Voiceless Fricative PulmonicEgressive,
    Phonet  Glottal Voiceless Fricative PulmonicEgressive,
    Phonet  PostAlveolar Voiced Affricate PulmonicEgressive,
    Phonet  PostAlveolar Voiceless Affricate PulmonicEgressive,
    Phonet  Bilabial Voiced Nasal PulmonicEgressive,
    Phonet  Alveolar Voiced Nasal PulmonicEgressive,
    Phonet  Velar Voiced Nasal PulmonicEgressive,
    Phonet  Alveolar Voiced Approximant PulmonicEgressive,
    Phonet  PostAlveolar Voiced Approximant PulmonicEgressive,
    Phonet  Palatal Voiced Approximant PulmonicEgressive,
    Phonet  LabialVelar Voiced Approximant PulmonicEgressive
    ]






someFunc :: IO ()
someFunc = putStrLn "Please read README.md file for instructions on how to use."
