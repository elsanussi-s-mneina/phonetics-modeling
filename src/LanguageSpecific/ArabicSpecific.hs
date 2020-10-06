module LanguageSpecific.ArabicSpecific where
import Prelude ()

import Types.Airstream ( Airstream(..))
import Types.Backness ( Backness(..) )
import Types.Height ( Height(..) )
import Types.Manner ( Manner(..) )
import Types.Phonet ( Phonet(..) )
import Types.PhonetInventory ( PhonetInventory(..) )
import Types.Place ( Place(..) )
import Types.Rounding ( Rounding(..) )
import Types.SecondaryArticulation ( SecondaryArticulation(..) )
import Types.VocalFolds ( VocalFolds(..) )
import Types.VowelLength ( VowelLength(..) )


-- | This is the phoneme inventory found in Modern Standard Arabic
-- that are considered the correct pronunciation by the Arabic tradition today.
-- This is what they will teach in school.
-- Note: These are just phonemes, the allophones are not included for simplicity.
arabicPhonemeInventory :: PhonetInventory
arabicPhonemeInventory =
  PhonetInventory
        [ Consonant Voiced    Bilabial     Plosive            PulmonicEgressive Normal
        , Consonant Voiceless Alveolar     Plosive            PulmonicEgressive Normal
        , Consonant Voiced    Alveolar     Plosive            PulmonicEgressive Normal
        , Consonant Voiceless Velar        Plosive            PulmonicEgressive Normal
        , Consonant Voiceless Uvular       Plosive            PulmonicEgressive Normal
        , Consonant Voiceless Glottal      Plosive            PulmonicEgressive Normal
        , Consonant Voiced    Bilabial     Nasal              PulmonicEgressive Normal
        , Consonant Voiced    Alveolar     Nasal              PulmonicEgressive Normal
        , Consonant Voiced    Alveolar     Trill              PulmonicEgressive Normal
        , Consonant Voiceless LabioDental  Fricative          PulmonicEgressive Normal
        , Consonant Voiceless Dental       Fricative          PulmonicEgressive Normal
        , Consonant Voiced    Dental       Fricative          PulmonicEgressive Normal
        , Consonant Voiceless Alveolar     Fricative          PulmonicEgressive Normal
        , Consonant Voiced    Alveolar     Fricative          PulmonicEgressive Normal
        , Consonant Voiceless PostAlveolar Fricative          PulmonicEgressive Normal
        , Consonant Voiced    Uvular       Fricative          PulmonicEgressive Normal
        , Consonant Voiceless Uvular       Fricative          PulmonicEgressive Normal
        , Consonant Voiced    Pharyngeal   Fricative          PulmonicEgressive Normal
        , Consonant Voiceless Pharyngeal   Fricative          PulmonicEgressive Normal
        , Consonant Voiceless Glottal      Fricative          PulmonicEgressive Normal
        , Consonant Voiced    PostAlveolar Affricate          PulmonicEgressive Normal
        , Consonant Voiced    Palatal      Approximant        PulmonicEgressive Normal
        , Consonant Voiced    Alveolar     LateralApproximant PulmonicEgressive Normal
        , Consonant Voiced    LabialVelar  Approximant        PulmonicEgressive Normal

        -- Consonants with phonemic secondary articulation
        , Consonant Voiceless Alveolar     Plosive            PulmonicEgressive Pharyngealized
        , Consonant Voiced    Alveolar     Plosive            PulmonicEgressive Pharyngealized
        , Consonant Voiced    Dental       Fricative          PulmonicEgressive Pharyngealized
        , Consonant Voiceless Alveolar     Fricative          PulmonicEgressive Pharyngealized


        -- Short vowels:
        , Vowel     Close     Front   Unrounded Voiced NormalLength
        , Vowel     Open      Front   Unrounded Voiced NormalLength
        , Vowel     Close     Back    Rounded   Voiced NormalLength

        -- Long vowels:
        , Vowel     Close     Front   Unrounded Voiced Long
        , Vowel     Open      Front   Unrounded Voiced Long
        , Vowel     Close     Back    Rounded   Voiced Long
        ]

