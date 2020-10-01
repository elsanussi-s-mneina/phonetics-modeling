module LanguageSpecific.IrishSpecific where
import Prelude ()
import Lib_Types
  ( Airstream(..)
  , Backness(..)
  , Height(..)
  , Manner(..)
  , Phonet(..)
  , PhonetInventory(..)
  , Place(..)
  , Rounding(..)
  , SecondaryArticulation(..)
  , VocalFolds(..)
  , VowelLength(..)
  )

-- | According to
-- the Wikipedia article
-- (found at the URL:  https://en.wikipedia.org/wiki/Irish_language )
-- these are the phonemes of Irish.
-- (specific version of Wikipedia article used: https://en.wikipedia.org/w/index.php?title=Irish_language&oldid=980945249)
-- The article is accessed at Wednesday, September 30th, 2020.
irishPhonemeInventory :: PhonetInventory
irishPhonemeInventory =
  PhonetInventory
        [ Consonant Voiceless Bilabial     Plosive            PulmonicEgressive Velarized   -- pˠ
        , Consonant Voiceless Bilabial     Plosive            PulmonicEgressive Palatalized -- pʲ
        , Consonant Voiced    Bilabial     Plosive            PulmonicEgressive Velarized   -- bˠ
        , Consonant Voiced    Bilabial     Plosive            PulmonicEgressive Palatalized -- bʲ
        , Consonant Voiceless Dental       Plosive            PulmonicEgressive Velarized   -- t̪ˠ
        , Consonant Voiceless Alveolar     Plosive            PulmonicEgressive Palatalized -- tʲ
        , Consonant Voiced    Dental       Plosive            PulmonicEgressive Velarized   -- d̪ˠ
        , Consonant Voiced    Alveolar     Plosive            PulmonicEgressive Palatalized -- dʲ

        , Consonant Voiceless Velar        Plosive            PulmonicEgressive Normal      -- k
        , Consonant Voiceless Palatal      Plosive            PulmonicEgressive Normal      -- c

        , Consonant Voiced Velar           Plosive            PulmonicEgressive Normal      -- g
        , Consonant Voiced Palatal         Plosive            PulmonicEgressive Normal      -- ɟ

        , Consonant Voiced    Bilabial     Nasal              PulmonicEgressive Velarized   -- mˠ
        , Consonant Voiced    Bilabial     Nasal              PulmonicEgressive Palatalized -- mʲ
        , Consonant Voiced    Dental       Nasal              PulmonicEgressive Velarized   -- n̪ˠ
        , Consonant Voiced    Alveolar     Nasal              PulmonicEgressive Palatalized -- nʲ

        , Consonant Voiced    Velar        Nasal              PulmonicEgressive Normal      -- ŋ
        , Consonant Voiced    Palatal      Nasal              PulmonicEgressive Normal      -- ɲ


        , Consonant Voiced    Alveolar     TapOrFlap          PulmonicEgressive Velarized   -- ɾˠ
        , Consonant Voiced    Alveolar     TapOrFlap          PulmonicEgressive Palatalized -- ɾʲ

        , Consonant Voiced    Dental       LateralApproximant PulmonicEgressive Velarized   -- l̪ˠ
        , Consonant Voiced    Alveolar     LateralApproximant PulmonicEgressive Palatalized -- lʲ


        , Consonant Voiceless LabioDental  Fricative          PulmonicEgressive Velarized   -- fˠ
        , Consonant Voiceless LabioDental  Fricative          PulmonicEgressive Palatalized -- fʲ
        , Consonant Voiced    LabialVelar  Approximant        PulmonicEgressive Normal      -- w
        , Consonant Voiced    LabioDental  Fricative          PulmonicEgressive Normal      -- v
        , Consonant Voiced    LabioDental  Fricative          PulmonicEgressive Palatalized -- vʲ

        , Consonant Voiceless Alveolar     Fricative          PulmonicEgressive Velarized   -- sˠ
        , Consonant Voiceless PostAlveolar Fricative          PulmonicEgressive Normal      -- ʃ
        , Consonant Voiced    Velar        Fricative          PulmonicEgressive Normal      -- ɣ
        , Consonant Voiced    Palatal      Approximant        PulmonicEgressive Normal      -- j

        , Consonant Voiceless Velar       Fricative           PulmonicEgressive Normal      -- x
        , Consonant Voiceless Palatal     Fricative           PulmonicEgressive Normal      -- ç

        , Consonant Voiceless Glottal      Fricative          PulmonicEgressive Normal      -- h

        -- Short vowels:
        , Vowel     NearClose Front       Unrounded Voiced NormalLength -- ɪ
        , Vowel     NearClose Back        Rounded   Voiced NormalLength -- ʊ
        , Vowel     OpenMid   Front       Unrounded Voiced NormalLength -- ɛ
        , Vowel     Mid       Central     Unrounded Voiced NormalLength -- ə
        , Vowel     OpenMid   Back        Rounded   Voiced NormalLength -- ɔ
        , Vowel     Open      Front       Unrounded Voiced NormalLength -- a

        -- Long vowels:
        , Vowel     Close     Front   Unrounded Voiced Long -- iː
        , Vowel     Close     Back    Rounded   Voiced Long -- uː
        , Vowel     CloseMid  Front   Unrounded Voiced Long -- eː
        , Vowel     CloseMid  Back    Rounded   Voiced Long -- oː
        , Vowel     Open      Back    Unrounded Voiced Long -- ɑː
        ]

