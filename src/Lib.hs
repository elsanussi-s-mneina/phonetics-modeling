module Lib
    ( someFunc
    ) where

data Phonet = Phonet { place :: Place   -- | Place of articulation
                     , vocalFolds :: VocalFolds
                     , manner :: Manner -- | Manner of articulation
                     , airstream :: Airstream
                     }

data Place = Bilabial | LabioDental | Dental | Alveolar | PostAlveolar
           | Palatal  | Velar       | Glottal

data Manner = Plosive | Fricative | Affricate | Nasal | Approximant

data Airstream = PulmonicEgressive

data VocalFolds = Voiced | Voiceless


data PhonetInventory = PhonetInventory [Phonet]


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
someFunc = putStrLn "someFunc"
