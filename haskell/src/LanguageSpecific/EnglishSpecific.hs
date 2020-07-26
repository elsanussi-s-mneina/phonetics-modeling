{-# LANGUAGE OverloadedStrings #-}

module LanguageSpecific.EnglishSpecific where

import           Prelude ()
import           Relude  (fromList)
import           Lib_Types

-- | This is a list of the sounds of English. Just the basic ones.
--   It is somewhat more complicated in reality, but for now this will
--   suffice.
--   This following sound inventory of English is from page 20 of
--   (2013, Elizabeth C. Zsiga, The Sounds of Language)
englishPhonetInventory :: PhonetInventory
englishPhonetInventory =
  PhonetInventory
    ( fromList
        [ Consonant Voiced Bilabial Plosive PulmonicEgressive,
          Consonant Voiceless Bilabial Plosive PulmonicEgressive,
          Consonant Voiced Alveolar Plosive PulmonicEgressive,
          Consonant Voiceless Alveolar Plosive PulmonicEgressive,
          Consonant Voiced Velar Plosive PulmonicEgressive,
          Consonant Voiceless Velar Plosive PulmonicEgressive,
          Consonant Voiceless Glottal Plosive PulmonicEgressive,
          Consonant Voiced LabioDental Fricative PulmonicEgressive,
          Consonant Voiceless LabioDental Fricative PulmonicEgressive,
          Consonant Voiced Dental Fricative PulmonicEgressive,
          Consonant Voiceless Dental Fricative PulmonicEgressive,
          Consonant Voiced Alveolar Fricative PulmonicEgressive,
          Consonant Voiceless Alveolar Fricative PulmonicEgressive,
          Consonant Voiced PostAlveolar Fricative PulmonicEgressive,
          Consonant Voiceless PostAlveolar Fricative PulmonicEgressive,
          Consonant Voiceless Glottal Fricative PulmonicEgressive,
          Consonant Voiced PostAlveolar Affricate PulmonicEgressive,
          Consonant Voiceless PostAlveolar Affricate PulmonicEgressive,
          Consonant Voiced Bilabial Nasal PulmonicEgressive,
          Consonant Voiced Alveolar Nasal PulmonicEgressive,
          Consonant Voiced Velar Nasal PulmonicEgressive,
          -- The Postalveolar version is technically correct, even though the convention
          -- is to write it in IPA as if it were alveolar. See
          -- Wikipedia article titled "Voiced alveolar and postalveolar approximants"
          -- at the following URL:
          -- https://en.wikipedia.org/wiki/Voiced_alveolar_and_postalveolar_approximants
          Consonant Voiced PostAlveolar Approximant PulmonicEgressive,
          Consonant Voiced Palatal Approximant PulmonicEgressive,
          Consonant Voiced LabialVelar Approximant PulmonicEgressive,
          Vowel Close Front Unrounded Voiced, -- "i"
          Vowel Close Back Rounded Voiced, -- "u"

          -- Near-close Vowels:
          Vowel NearClose Front Unrounded Voiced, -- "ɪ"
          Vowel NearClose Back Rounded Voiced, --  "ʊ"

          -- Close-mid Vowels:
          Vowel CloseMid Front Unrounded Voiced, -- "e"
          Vowel CloseMid Back Rounded Voiced, -- "o"

          -- Mid Vowels:
          Vowel Mid Central Unrounded Voiced, -- "ə"

          -- Open-mid Vowels:
          Vowel OpenMid Front Unrounded Voiced, -- "ɛ"
          Vowel OpenMid Central Unrounded Voiced, -- "ɜ"
          Vowel OpenMid Back Unrounded Voiced, --  "ʌ"
          Vowel OpenMid Back Rounded Voiced, -- "ɔ"

          -- Near-open
          Vowel NearOpen Front Unrounded Voiced, -- "æ"
          Vowel NearOpen Central Unrounded Voiced, -- "ɐ"

          -- Open Vowels:
          Vowel Open Back Unrounded Voiced, -- "ɑ"
          Vowel Open Back Rounded Voiced -- "ɒ"
        ]
    )

-- I added some English vowels. I did not choose any specific dialect.
-- I got all my information from the Wikipedia page titled
-- "English Phonology"
-- at the following URL: https://en.wikipedia.org/wiki/English_phonology#Vowels
-- on Monday, February 24, 2020.
-- To do: Get better information on English vowels from a more reliable source.
-- To do: model separate dialects of English or only one.
