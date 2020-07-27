{-# LANGUAGE OverloadedStrings #-}

module LanguageSpecific.EnglishSpecific where

import           Prelude ()
import           Relude  (fromList)
import           Lib_Types (PhonetInventory(PhonetInventory), Phonet(Consonant, Vowel), VocalFolds(..),
                            Place(Bilabial, LabioDental, LabialVelar, Dental, Alveolar, PostAlveolar, Palatal, Velar,
                            Glottal),
                            Manner(Affricate, Approximant, LateralApproximant, Fricative, Nasal, Plosive),
                            Airstream(PulmonicEgressive), Height(Close, CloseMid, Mid, OpenMid, NearClose, NearOpen, Open),
                            Backness(Front, Central, Back),
                            Rounding(Rounded, Unrounded))


-- These are the sounds found in most native English dialects.
-- This information was taken from "the speech accent archive"
-- on July 27, 2020, at the following URL:
-- http://accent.gmu.edu/browse_native.php?function=detail&languageid=18
-- They have a note on their web page that their information
-- was adapted from Ladefoged, P. (1993)
-- The information on the webpage was in two charts.
-- I took the information and turned it into source code
-- in this file as the value titled "englishPhonetInventory".
-- Note: there are 5 dipthongs that are not included here.
englishPhonetInventory :: PhonetInventory
englishPhonetInventory =
  PhonetInventory
    ( fromList
        [ Consonant Voiceless Bilabial     Plosive            PulmonicEgressive
        , Consonant Voiced    Bilabial     Plosive            PulmonicEgressive
        , Consonant Voiceless Alveolar     Plosive            PulmonicEgressive
        , Consonant Voiced    Alveolar     Plosive            PulmonicEgressive
        , Consonant Voiceless Velar        Plosive            PulmonicEgressive
        , Consonant Voiced    Velar        Plosive            PulmonicEgressive
        , Consonant Voiced    Bilabial     Nasal              PulmonicEgressive
        , Consonant Voiced    Alveolar     Nasal              PulmonicEgressive
        , Consonant Voiced    Velar        Nasal              PulmonicEgressive
        , Consonant Voiceless LabioDental  Fricative          PulmonicEgressive
        , Consonant Voiced    LabioDental  Fricative          PulmonicEgressive
        , Consonant Voiceless Dental       Fricative          PulmonicEgressive
        , Consonant Voiced    Dental       Fricative          PulmonicEgressive
        , Consonant Voiceless Alveolar     Fricative          PulmonicEgressive
        , Consonant Voiced    Alveolar     Fricative          PulmonicEgressive
        , Consonant Voiceless PostAlveolar Fricative          PulmonicEgressive
        , Consonant Voiced    PostAlveolar Fricative          PulmonicEgressive
        , Consonant Voiceless Glottal      Fricative          PulmonicEgressive
        , Consonant Voiceless PostAlveolar Affricate          PulmonicEgressive
        , Consonant Voiced    PostAlveolar Affricate          PulmonicEgressive
        , Consonant Voiced    Alveolar     Approximant        PulmonicEgressive
        , Consonant Voiced    Palatal      Approximant        PulmonicEgressive
        , Consonant Voiced    Alveolar     LateralApproximant PulmonicEgressive
        , Consonant Voiced    LabialVelar  Approximant        PulmonicEgressive
        , Vowel     Close     Front   Unrounded Voiced
        , Vowel     NearClose Front   Unrounded Voiced
        , Vowel     CloseMid  Front   Unrounded Voiced
        , Vowel     OpenMid   Front   Unrounded Voiced
        , Vowel     NearOpen  Front   Unrounded Voiced
        , Vowel     Mid       Central Unrounded Voiced
        , Vowel     Close     Back    Rounded   Voiced
        , Vowel     NearClose Back    Rounded   Voiced
        , Vowel     CloseMid  Back    Rounded   Voiced
        , Vowel     OpenMid   Back    Unrounded Voiced
        , Vowel     OpenMid   Back    Rounded   Voiced
        , Vowel     Open      Back    Unrounded Voiced
        ]
    )

