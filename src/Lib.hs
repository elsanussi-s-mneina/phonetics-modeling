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
           | Palatal  | Velar       | Glottal
           deriving (Eq, Show)

data Manner = Plosive | Fricative | Affricate | Nasal | Approximant
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
analyzeIPA  :: IPAText -> Phonet
analyzeIPA "p"  = Phonet  Bilabial Voiceless Plosive PulmonicEgressive
analyzeIPA "b"  = Phonet  Bilabial Voiced Plosive PulmonicEgressive
analyzeIPA "t"  = Phonet  Alveolar Voiceless Plosive PulmonicEgressive
analyzeIPA "d"  = Phonet  Alveolar Voiced Plosive PulmonicEgressive
analyzeIPA "k"  = Phonet  Velar Voiceless Plosive PulmonicEgressive
analyzeIPA "g"  = Phonet  Velar Voiced Plosive PulmonicEgressive
analyzeIPA "ʔ"  = Phonet  Glottal Voiceless Plosive PulmonicEgressive
analyzeIPA "f"  = Phonet  LabioDental Voiceless Fricative PulmonicEgressive
analyzeIPA "v"  = Phonet  LabioDental Voiced Fricative PulmonicEgressive
analyzeIPA "ð"  = Phonet  Dental Voiced Fricative PulmonicEgressive,
analyzeIPA "θ"  = Phonet  Dental Voiceless Fricative PulmonicEgressive,
analyzeIPA "z"  = Phonet  Alveolar Voiced Fricative PulmonicEgressive,
analyzeIPA "s"  = Phonet  Alveolar Voiceless Fricative PulmonicEgressive,
analyzeIPA "ʒ"  = Phonet  PostAlveolar Voiced Fricative PulmonicEgressive,
analyzeIPA "ʃ"  = Phonet  PostAlveolar Voiceless Fricative PulmonicEgressive,
analyzeIPA "h"  = Phonet  Glottal Voiceless Fricative PulmonicEgressive,
analyzeIPA "dʒ" = Phonet  PostAlveolar Voiced Affricate PulmonicEgressive,
analyzeIPA "tʃ" = Phonet  PostAlveolar Voiceless Affricate PulmonicEgressive,
analyzeIPA "m"  = Phonet  Bilabial Voiced Nasal PulmonicEgressive,
analyzeIPA "n"  = Phonet  Alveolar Voiced Nasal PulmonicEgressive,
analyzeIPA "ŋ"  = Phonet  Velar Voiced Nasal PulmonicEgressive,
analyzeIPA "w"  = Phonet  Bilabial Voiced Approximant PulmonicEgressive,
analyzeIPA "l"  = Phonet  Alveolar Voiced Approximant PulmonicEgressive,
analyzeIPA "ɹ"  = Phonet  PostAlveolar Voiced Approximant PulmonicEgressive,
analyzeIPA "j"  = Phonet  Palatal Voiced Approximant PulmonicEgressive,
analyzeIPA "w"  = Phonet  Velar Voiced Approximant PulmonicEgressive



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
    Phonet  Bilabial Voiced Approximant PulmonicEgressive,
    Phonet  Alveolar Voiced Approximant PulmonicEgressive,
    Phonet  PostAlveolar Voiced Approximant PulmonicEgressive,
    Phonet  Palatal Voiced Approximant PulmonicEgressive,
    Phonet  Velar Voiced Approximant PulmonicEgressive
       -- [w] is technically both velar and bilabial
       -- We may want to encode it differently.
       -- currently it is encoded twice
    ]






someFunc :: IO ()
someFunc = putStrLn "Please read README.md file for instructions on how to use."
