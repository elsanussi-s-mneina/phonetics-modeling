{-# LANGUAGE OverloadedStrings #-}

module LanguageSpecific.EnglishSpecific where

import           Prelude ()
import           Relude  (fromList)
import           Lib_Types (PhonetInventory(PhonetInventory), Phonet(Consonant, Vowel), VocalFolds(..),
                            Place(Bilabial, LabioDental, LabialVelar, Dental, Alveolar, PostAlveolar, Palatal, Velar,
                            Glottal),
                            Manner(Affricate, Approximant, LateralApproximant, Fricative, Nasal, Plosive),
                            Airstream(PulmonicEgressive),
                            SecondaryArticulation(Normal),
                            Height(Close, CloseMid, Mid, OpenMid, NearClose, NearOpen, Open),

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
        [ Consonant Voiceless Bilabial     Plosive            PulmonicEgressive Normal
        , Consonant Voiced    Bilabial     Plosive            PulmonicEgressive Normal
        , Consonant Voiceless Alveolar     Plosive            PulmonicEgressive Normal
        , Consonant Voiced    Alveolar     Plosive            PulmonicEgressive Normal
        , Consonant Voiceless Velar        Plosive            PulmonicEgressive Normal
        , Consonant Voiced    Velar        Plosive            PulmonicEgressive Normal
        , Consonant Voiced    Bilabial     Nasal              PulmonicEgressive Normal
        , Consonant Voiced    Alveolar     Nasal              PulmonicEgressive Normal
        , Consonant Voiced    Velar        Nasal              PulmonicEgressive Normal
        , Consonant Voiceless LabioDental  Fricative          PulmonicEgressive Normal
        , Consonant Voiced    LabioDental  Fricative          PulmonicEgressive Normal
        , Consonant Voiceless Dental       Fricative          PulmonicEgressive Normal
        , Consonant Voiced    Dental       Fricative          PulmonicEgressive Normal
        , Consonant Voiceless Alveolar     Fricative          PulmonicEgressive Normal
        , Consonant Voiced    Alveolar     Fricative          PulmonicEgressive Normal
        , Consonant Voiceless PostAlveolar Fricative          PulmonicEgressive Normal
        , Consonant Voiced    PostAlveolar Fricative          PulmonicEgressive Normal
        , Consonant Voiceless Glottal      Fricative          PulmonicEgressive Normal
        , Consonant Voiceless PostAlveolar Affricate          PulmonicEgressive Normal
        , Consonant Voiced    PostAlveolar Affricate          PulmonicEgressive Normal
        , Consonant Voiced    Alveolar     Approximant        PulmonicEgressive Normal
        , Consonant Voiced    Palatal      Approximant        PulmonicEgressive Normal
        , Consonant Voiced    Alveolar     LateralApproximant PulmonicEgressive Normal
        , Consonant Voiced    LabialVelar  Approximant        PulmonicEgressive Normal
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

