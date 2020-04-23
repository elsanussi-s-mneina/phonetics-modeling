{-# LANGUAGE UnicodeSyntax #-}


module Lib_Functions where

import Lib_Types

import Prelude
  ( Bool(False, True)      , Int       , Maybe(Just, Nothing)
  , Show(show)             , String
  , concat                 , concatMap , elem
  , filter                 , head      , init
  , last                   , length    , map
  , not                    , notElem   , null
  , otherwise
  , (>)                    , (+)       , (<)
  )

import Data.List
  ( intercalate
  )

import Data.Maybe
  ( catMaybes
  )

import Prelude.Unicode
  ( (∘) , (≡) , (⧺) , (∧) , (∨)
  , (∈)
  )


retractedPlace ∷ Place → Place
retractedPlace Bilabial     = LabioDental
retractedPlace LabioDental  = Dental
retractedPlace Dental       = Alveolar
retractedPlace Alveolar     = PostAlveolar
retractedPlace PostAlveolar = Retroflex
retractedPlace Retroflex    = Palatal
retractedPlace Palatal      = Velar
retractedPlace Velar        = Uvular
retractedPlace Uvular       = Pharyngeal
retractedPlace Pharyngeal   = Glottal
retractedPlace Glottal      = Epiglottal
retractedPlace same         = same




englishDescription ∷ Phonet → String
englishDescription x = show x


-- | A function that given an IPA symbol will convert it to the voiced equivalent.
voicedPhonet ∷ Phonet → Phonet
voicedPhonet (Consonant   VoicelessAspirated x y z) = Consonant   VoicedAspirated x y z
voicedPhonet (Consonant   Voiceless          x y z) = Consonant   Voiced x y z
voicedPhonet (Consonant   Voiced             x y z) = Consonant   Voiced x y z
voicedPhonet (Consonant   VoicedAspirated    x y z) = Consonant   VoicedAspirated x y z
voicedPhonet (Consonant   _                  x y z) = Consonant   Voiced x y z
voicedPhonet (Vowel x y z _                       ) = Vowel x y z Voiced

-- | A function that given an IPA symbol will convert it to the voiceless equivalent.
devoicedPhonet ∷ Phonet → Phonet
devoicedPhonet (Consonant   Voiced             x y z) = Consonant   Voiceless          x y z
devoicedPhonet (Consonant   CreakyVoiced       x y z) = Consonant   Voiceless          x y z
devoicedPhonet (Consonant   Voiceless          x y z) = Consonant   Voiceless          x y z
devoicedPhonet (Consonant   VoicedAspirated    x y z) = Consonant   VoicelessAspirated x y z
devoicedPhonet (Consonant   VoicelessAspirated x y z) = Consonant   VoicelessAspirated x y z
devoicedPhonet (Vowel x y z _                       ) = Vowel x y z Voiceless



spirantizedPhonet ∷ Phonet → Phonet

-- The following is inelegant, but there is no other way in the system,
-- right now. The part that is inelegant is that,
-- a [t] which is considered alveolar, when spirantized becomes [θ] which is dental.
-- So the following line implements this
-- change in place of articulation.
spirantizedPhonet (Consonant x Alveolar Plosive z) =
  Consonant x Dental Fricative z

spirantizedPhonet (Consonant x place1 Plosive z)
  = Consonant x place1 Fricative z
spirantizedPhonet other = other



unmarkDifferences ∷ Phonet → Phonet → UnmarkablePhonet
unmarkDifferences (Consonant voice1 place1 manner1 airstream1)
                  (Consonant voice2 place2 manner2 airstream2) =
  let voice'     = if voice1     ≡ voice2
                     then MarkedVocalFolds voice1
                     else UnmarkedVocalFolds
      place'     = if place1     ≡ place2
                     then MarkedPlace      place1
                     else UnmarkedPlace
      manner'    = if manner1    ≡ manner2
                     then MarkedManner     manner1
                     else UnmarkedManner
      airstream' = if airstream1 ≡ airstream2
                     then MarkedAirstream  airstream1
                     else UnmarkedAirstream
  in UnmarkableConsonant voice' place' manner' airstream'

unmarkDifferences (Vowel height1 backness1 rounding1 voice1)
                  (Vowel height2 backness2 rounding2 voice2) =
  let voice'    = if voice1    ≡ voice2
                    then MarkedVocalFolds voice1
                    else UnmarkedVocalFolds
      height'   = if height1   ≡ height2
                    then MarkedHeight     height1
                    else UnmarkedHeight
      backness' = if backness1 ≡ backness2
                    then MarkedBackness   backness1
                    else UnmarkedBackness
      rounding' = if rounding1 ≡ rounding2
                    then MarkedRounding   rounding1
                    else UnmarkedRounding
  in UnmarkableVowel height' backness' rounding' voice'

unmarkDifferences (Vowel _ _ _ voice1) (Consonant voice2 _ _ _) =
  let voice' = if voice1 ≡ voice2
                 then MarkedVocalFolds voice1
                 else UnmarkedVocalFolds
  in UnmarkableVowel UnmarkedHeight UnmarkedBackness UnmarkedRounding voice'


unmarkDifferences c@(Consonant _ _ _ _) v@(Vowel _ _ _ _) =
  unmarkDifferences v c -- Change the order of arguments



-- This function (I realize it is poorly named)
-- takes any unmarked attributes in the phoneme definition,
-- and returns a list with all possibilities for that attribute.
generateFromUnmarked ∷ UnmarkablePhonet → [Phonet]
generateFromUnmarked (UnmarkableConsonant voice1 place1 manner1 airstream1) =
  let voice'     = unmarkableVoiceToList     voice1
      place'     = unmarkablePlaceToList     place1
      manner'    = unmarkableMannerToList    manner1
      airstream' = unmarkableAirstreamToList airstream1
  in [Consonant v p m a | p ← place', v ← voice',  m ← manner', a ← airstream']

generateFromUnmarked (UnmarkableVowel height1 backness1 rounding1 voice1) =
  let voice'    = unmarkableVoiceToList    voice1
      height'   = unmarkableHeightToList   height1
      backness' = unmarkableBacknessToList backness1
      rounding' = unmarkableRoundingToList rounding1
  in [Vowel h b r v | h ← height', b ← backness', r ← rounding', v ← voice']


unmarkableVoiceToList ∷ UnmarkableVocalFolds → [VocalFolds]
unmarkableVoiceToList voice1 =
  case voice1 of
       MarkedVocalFolds x → [x]
       UnmarkedVocalFolds → vocalFoldStates

unmarkablePlaceToList ∷ UnmarkablePlace → [Place]
unmarkablePlaceToList place1 =
  case place1 of
       MarkedPlace x → [x]
       UnmarkedPlace → placeStates


unmarkableMannerToList ∷ UnmarkableManner → [Manner]
unmarkableMannerToList manner1 =
  case manner1 of
       MarkedManner x → [x]
       UnmarkedManner → mannerStates

unmarkableAirstreamToList ∷ UnmarkableAirstream → [Airstream]
unmarkableAirstreamToList airstream1 =
  case airstream1 of
       MarkedAirstream x → [x]
       UnmarkedAirstream → airstreamStates


unmarkableHeightToList ∷ UnmarkableHeight → [Height]
unmarkableHeightToList height1 =
  case height1 of
       MarkedHeight x → [x]
       UnmarkedHeight → heightStates

unmarkableBacknessToList ∷ UnmarkableBackness → [Backness]
unmarkableBacknessToList backness1 =
  case backness1 of
       MarkedBackness x → [x]
       UnmarkedBackness → backnessStates

unmarkableRoundingToList ∷ UnmarkableRounding → [Rounding]
unmarkableRoundingToList rounding1 =
  case rounding1 of
       MarkedRounding x → [x]
       UnmarkedRounding → roundingStates

-- The following function returns whether an articulation is
-- considered impossible according to the IPA (pulmonic) consonants chart.
-- Does not work for other values.
impossible ∷ Phonet → Bool
impossible (Consonant Voiced          Pharyngeal  Plosive            PulmonicEgressive) = True
impossible (Consonant VoicedAspirated Pharyngeal  Plosive            PulmonicEgressive) = True
impossible (Consonant Voiceless       Glottal     Plosive            PulmonicEgressive) = False  -- [ʔ] is not impossible.
impossible (Consonant _               Glottal     Fricative          PulmonicEgressive) = False  -- [h] and [ɦ] are not impossible.
impossible (Consonant _               Glottal     _                  PulmonicEgressive) = True   -- all other pulmonary egressive consonants are impossible..
impossible (Consonant _               Pharyngeal  Nasal              PulmonicEgressive) = True
impossible (Consonant _               Pharyngeal  LateralFricative   PulmonicEgressive) = True
impossible (Consonant _               Pharyngeal  LateralApproximant PulmonicEgressive) = True
impossible (Consonant _               Velar       Trill              PulmonicEgressive) = True
impossible (Consonant _               Velar       TapOrFlap          PulmonicEgressive) = True
impossible (Consonant _               Bilabial    LateralFricative   PulmonicEgressive) = True
impossible (Consonant _               Bilabial    LateralApproximant PulmonicEgressive) = True
impossible (Consonant _               LabioDental LateralFricative   PulmonicEgressive) = True
impossible (Consonant _               LabioDental LateralApproximant PulmonicEgressive) = True
impossible _ = False -- Everything else is assumed to be possible.




-- | This is a list of the sounds of English. Just the basic ones.
-- | It is somewhat more complicated in reality, but for now this will
-- | suffice.
-- | This following sound inventory of English is from page 20 of
-- | (2013, Elizabeth C. Zsiga, The Sounds of Language)
englishPhonetInventory ∷ PhonetInventory
englishPhonetInventory = PhonetInventory
  [
  Consonant  Voiced    Bilabial      Plosive   PulmonicEgressive,
  Consonant  Voiceless Bilabial      Plosive   PulmonicEgressive,
  Consonant  Voiced    Alveolar      Plosive   PulmonicEgressive,
  Consonant  Voiceless Alveolar      Plosive   PulmonicEgressive,
  Consonant  Voiced    Velar         Plosive   PulmonicEgressive,
  Consonant  Voiceless Velar         Plosive   PulmonicEgressive,
  Consonant  Voiceless Glottal       Plosive   PulmonicEgressive,
  Consonant  Voiced    LabioDental   Fricative PulmonicEgressive,
  Consonant  Voiceless LabioDental   Fricative PulmonicEgressive,
  Consonant  Voiced    Dental        Fricative PulmonicEgressive,
  Consonant  Voiceless Dental        Fricative PulmonicEgressive,
  Consonant  Voiced    Alveolar      Fricative PulmonicEgressive,
  Consonant  Voiceless Alveolar      Fricative PulmonicEgressive,
  Consonant  Voiced    PostAlveolar  Fricative PulmonicEgressive,
  Consonant  Voiceless PostAlveolar  Fricative PulmonicEgressive,
  Consonant  Voiceless Glottal       Fricative PulmonicEgressive,
  Consonant  Voiced    PostAlveolar  Affricate PulmonicEgressive,
  Consonant  Voiceless PostAlveolar  Affricate PulmonicEgressive,
  Consonant  Voiced    Bilabial      Nasal     PulmonicEgressive,
  Consonant  Voiced    Alveolar      Nasal     PulmonicEgressive,
  Consonant  Voiced    Velar         Nasal     PulmonicEgressive,
  -- The Postalveolar version is technically correct, even though the convention
  -- is to write it in IPA as if it were alveolar. See
  -- Wikipedia article titled "Voiced alveolar and postalveolar approximants"
  -- at the following URL:
  -- https://en.wikipedia.org/wiki/Voiced_alveolar_and_postalveolar_approximants
  Consonant  Voiced PostAlveolar Approximant PulmonicEgressive,
  Consonant  Voiced Palatal      Approximant PulmonicEgressive,
  Consonant  Voiced LabialVelar  Approximant PulmonicEgressive,



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
  Vowel Mid Central Unrounded Voiced, -- "ə"


  -- Open-mid Vowels:
  Vowel  OpenMid Front   Unrounded Voiced, -- "ɛ"
  -- English lacks: constructIPA (Vowel  OpenMid Front   Rounded   Voiced) = "œ"
  Vowel  OpenMid Central Unrounded Voiced, -- "ɜ"
  --  Vowel  OpenMid Central Rounded   Voiced -- "ɞ"
  Vowel  OpenMid Back    Unrounded Voiced, --  "ʌ"
  Vowel  OpenMid Back    Rounded   Voiced, -- "ɔ"

  -- Near-open
  Vowel  NearOpen Front   Unrounded Voiced, -- "æ"
  Vowel  NearOpen Central Unrounded  Voiced, -- "ɐ"

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



exponentials ∷ [IPAText]
exponentials = ["ʰ" , "ʷ" , "ʲ" , "ˠ" , "ˤ" , "ⁿ" , "ˡ"]

{-|
Whether an IPA character is written above the base line
and to the right of the previous character,
like how exponents of a power are written
in mathematical notation.
|-}
isExponential ∷ IPAText → Bool
isExponential character = character ∈ exponentials
{-|
Whether a diacritic goes above
the character it is placed on.
|-}
isDiacriticAbove ∷ IPAText → Bool
isDiacriticAbove "̊" = True
isDiacriticAbove  _  = False

{-|
Whether a diacritic goes below
the character which it is placed on.
|-}
isDiacriticBelow ∷ IPAText → Bool
isDiacriticBelow "̥" = True
isDiacriticBelow  _  = False

{-|
When given a diacritic that goes above,
replaces it with one that goes below,
and has the same meaning.
otherwise does nothing.
  |-}
lowerDiacritic ∷ IPAText → IPAText
lowerDiacritic "̊" = "̥"
lowerDiacritic x  = x


{-|
When given a diacritic that goes below,
replaces it with one that goes below, and
has the same meaning;
otherwise it does nothing.
  |-}
raiseDiacritic ∷ IPAText → IPAText
raiseDiacritic "̥" = "̊"
raiseDiacritic x  = x


{-|
Whether a character (but not a diacritic)
takes up space
below the imaginary horizontal line
on which it is written.

This could be useful later for determining
where to put diacritics so that
they are readable.
|-}
ascenders ∷ [IPAText]
ascenders =
  ["b", "t", "d", "k", "ʔ", "f", "θ", "ð", "ħ", "ʕ", "h", "ɦ", "ɬ", "l", "ʎ",
  "ʘ", "ɓ", "ǀ", "ɗ", "ǃ", "ǂ", "ɠ", "ʄ", "ǁ", "ʛ", "ɺ", "ʢ", "ʡ", "ɤ", "ʈ", "ɖ",
  "ɸ", "β", "ʃ", "ɮ", "ɭ", "ɧ"]


isAscender ∷ IPAText → Bool
isAscender character = character ∈ ascenders

descenders ∷ [IPAText]
descenders =
  ["p", "ɟ", "g", "q", "ɱ", "ɽ", "ʒ", "ʂ", "ʐ", "ç", "ʝ", "ɣ", "χ", "ɻ", "j",
   "ɰ", "ɥ", "y", "ɳ", "ɲ", "ʈ", "ɖ", "ɸ", "β", "ʃ", "ɮ", "ɭ", "ɧ"]


{-|
Whether a character (but not a diacritic)
takes up space
below the imaginary horizontal line
on which it is written.

This could be useful later for determining
where to put diacritics so that
they are readable.
|-}
isDescender ∷ IPAText → Bool
isDescender character = character ∈ descenders


{-|
Prevent placement of diacrtic's below a full-width
character,
when doing so would likely make the result
difficult to read, whenever there is another
diacrtiic with the same meaning, but can go above.
And vice-versa (above - below).

Only support the voiceless diacritic so far.
  |-}
preventProhibitedCombination :: IPAText → IPAText
preventProhibitedCombination [] = []
preventProhibitedCombination [y] = [y]
preventProhibitedCombination noChange@(x:y:z) =
  if isAscender [x] ∧ isDiacriticAbove [y]
  then [x] ⧺ lowerDiacritic [y] ⧺ z
  else if isDescender [x] ∧ isDiacriticBelow [y]
  then [x] ⧺ raiseDiacritic [y] ⧺ z
  else noChange






graphemesOfIPA ∷ [IPAText]
graphemesOfIPA = consonantsPulmonic
  ⧺ consonantsNonPulmonic
  ⧺ otherSymbols
  ⧺ vowels
  ⧺ suprasegmentals
  ⧺ toneAndWordAccents
  ⧺ diacriticsAndSuprasegmentals
-- See: https://www.internationalphoneticassociation.org/sites/default/files/IPA_Kiel_2015.pdf
-- For the source of this information..

-- CONSONANTS (PULMONIC)
consonantsPulmonic ∷ [IPAText]
consonantsPulmonic = concat consonantsPulmonicTable

plosivePulmonic ∷ [String]
plosivePulmonic            = [ "p", "b", " ", " ", " ", " ", "t", "d", " ", " "
                             , "ʈ", "ɖ", "c", "ɟ", "k", "g", "q", "ɢ", " ", " "
                             , "ʔ", " "
                             ] -- Plosive

nasalPulmonic ∷ [String]
nasalPulmonic              = [ " ", "m", " ", "ɱ", " ", " ", " ", "n", " ", " "
                             , " ", "ɳ", " ", "ɲ", " ", "ŋ", " ", "ɴ", " ", " "
                             , " ", " "
                             ] -- Nasal

trillPulmonic ∷ [String]
trillPulmonic              = [ " ", "ʙ", " ", " ", " ", " ", " ", "r", " ", " "
                             , " ", " ", " ", " ", " ", " ", " ", "ʀ", " ", " "
                             , " ", " "
                             ] -- Trill

tapOrFlapPulmonic ∷ [String]
tapOrFlapPulmonic          = [ " ", " ", " ", "ⱱ", " ", " ", " ", "ɾ", " ", " "
                             , " ", "ɽ", " ", " ", " ", " ", " ", " ", " ", " "
                             , " ", " "
                             ] -- Tap or Flap

fricativePulmonic ∷ [String]
fricativePulmonic          = [ "ɸ", "β", "f", "v", "θ", "ð", "s", "z", "ʃ", "ʒ"
                             , "ʂ", "ʐ", "ç", "ʝ", "x", "ɣ", "χ", "ʁ", "ħ", "ʕ"
                             , "h", "ɦ"
                             ]  -- Fricative

lateralFricativePulmonic ∷ [String]
lateralFricativePulmonic   = [ " ", " ", " ", " ", " ", " ", "ɬ", "ɮ", " ", " "
                             , " ", " ", " ", " ", " ", " ", " ", " ", " ", " "
                             , " ", " "
                             ] -- Lateral fricative

approximantPulmonic ∷ [String]
approximantPulmonic        = [ " ", " ", " ", "ʋ", " ", " ", " ", "ɹ", " ", " "
                             , " ", "ɻ", " ", "j", " ", "ɰ", " ", " ", " ", " "
                             , " ", " "
                             ] -- Approximant

lateralApproximantPulmonic ∷ [String]
lateralApproximantPulmonic = [ " ", " ", " ", " ", " ", " ", " ", "l", " ", " "
                             , " ", "ɭ", " ", "ʎ", " ", "ʟ", " ", " ", " ", " "
                             , " ", " "
                             ] -- Lateral approximant




consonantsPulmonicTable ∷ [[IPAText]]
consonantsPulmonicTable =
 [ plosivePulmonic
 , nasalPulmonic
 , trillPulmonic
 , tapOrFlapPulmonic
 , fricativePulmonic
 , lateralFricativePulmonic
 , approximantPulmonic
 , lateralApproximantPulmonic
 ]


consonantsNonPulmonic ∷ [IPAText]
consonantsNonPulmonic =
-- Clicks   Voiced implosives
 [ "ʘ",     "ɓ" -- Bilabial
 , "ǀ", {- Dental -}    "ɗ" -- Dental/alveolar
 , "ǃ", {-  (Post)alveolar -}  "ʄ"
 , "ǂ",  "ɠ"
 , "ǁ",  "ʛ"
 ]

otherSymbols ∷ [IPAText]
otherSymbols =
  ["ʍ",  "ɕ"
  ,"w",  "ʑ"
  ,"ɥ",  "ɺ"
  ,"ʜ",  "ɧ"
  ,"ʢ"
  ,"ʡ"
  ]

vowels ∷ [IPAText]
vowels =
  ["i", "y",   "ɨ", "ʉ",   "ɯ", "u"   -- Close
  ,"ɪ", "ʏ",            "ʊ"
  ,"e", "ø",   "ɘ", "ɵ",   "ɤ", "o"   -- Close-mid
  ,               "ə"
  ,"ɛ", "œ",   "ɜ", "ɞ",   "ʌ", "ɔ"   -- Open-mid
  , "æ",           "ɐ"
  , "a", "ɶ",              "ɑ", "ɒ"  -- Open
  ]

suprasegmentals ∷ [IPAText]
suprasegmentals =
  [ "ˈ"   -- Primary stress
  , "ˌ"   -- Secondary stress
  , "ː"   -- Long
  , "ˑ"   -- Half long

  , "̆"    -- Extra short
  , "|"   -- Minor (foot) group
  , "‖"   -- Major (intonation) group
  , "."   -- Syllable break
  , "‿"   -- Linking (absence of a break
  ]


toneAndWordAccents ∷ [IPAText]
toneAndWordAccents =
{- Level -}
  [ "˥", "̋"  -- Extra high
  , "˦", "́"  -- High
  , "˧", "̄"  -- Mid
  , "˨", "̀"  -- Low
  , "˩", "̏"  -- Extra low
  ,      "ꜜ"  -- Downstep
  ,      "ꜛ"  -- Upstep

{- Countour -}
  , "̌" -- Rising
  , "̂" -- Falling
  , "᷄" -- High rising
  , "᷅" -- Low rising
  , "᷈" -- Rising-falling
  , "↗" -- Global rise
  , "↘" -- Global fall
  ]

diacriticsAndSuprasegmentals ∷ [IPAText]
diacriticsAndSuprasegmentals =
  [ "ʰ"  -- Aspirated
  , "ʷ"  -- Labialised
  , "ʲ"  -- Palatalised
  , "ˠ"  -- Velarised
  , "ˤ"  -- Pharyngealised
  , "ⁿ"  -- Pre/post nasalised
  , "ˡ"  -- Lateral release

  , "˞"  -- Rhoticity
  , "ʼ"  -- Ejective
  , "̚"   -- No audible release

  , "̩"   -- Syllabic
  , "̯"   -- Non-syllabic
  , "̰"   -- Creaky voiced
  , "̥"   -- Voiceless
  , "̬"   -- Voiced
  , "̤"   -- Breathy voiced
  , "̊"   -- Voiceless (diacritic placed above symbol with descender)
  , "̍"   -- Syllabic (diacritic placed above)
  , "̪"   -- Dental
  , "̺"   -- Apical
  , "̻"   -- Laminal
  , "̼"   -- Linguolabial
  , "̣"   -- Closer variety/Fricative
  , "̃"   -- Nasalised
  , "̈"   -- Centralised
  , "̽"   -- Mid centralised
  , "̇"    -- Palatalization/Centralization
  ]

showIPA ∷ PhonetInventory → IPAText
showIPA (PhonetInventory phonetes) = concatMap constructIPA phonetes



-- | This function will allow us to convert an IPA symbol
-- | to its analyzed form (its phonetic features)
-- Currently, only the consonants (pulmonic) in the 2005 IPA chart are included.
analyzeIPA  ∷ IPAText → Maybe Phonet


-- | This function will allow us to convert an IPA symbol
-- | to its analyzed form (its phonetic features)
-- Currently, only the consonants (pulmonic) in the 2005 IPA chart are included.
-- Plosives:
analyzeIPA "p"  = Just (Consonant  Voiceless Bilabial  Plosive PulmonicEgressive)
analyzeIPA "b"  = Just (Consonant  Voiced    Bilabial  Plosive PulmonicEgressive)
analyzeIPA "t"  = Just (Consonant  Voiceless Alveolar  Plosive PulmonicEgressive)
analyzeIPA "d"  = Just (Consonant  Voiced    Alveolar  Plosive PulmonicEgressive)
analyzeIPA "ʈ"  = Just (Consonant  Voiceless Retroflex Plosive PulmonicEgressive)
analyzeIPA "ɖ"  = Just (Consonant  Voiced    Retroflex Plosive PulmonicEgressive)
analyzeIPA "c"  = Just (Consonant  Voiceless Palatal   Plosive PulmonicEgressive)
analyzeIPA "ɟ"  = Just (Consonant  Voiced    Palatal   Plosive PulmonicEgressive)
analyzeIPA "k"  = Just (Consonant  Voiceless Velar     Plosive PulmonicEgressive)
analyzeIPA "g"  = Just (Consonant  Voiced    Velar     Plosive PulmonicEgressive)
analyzeIPA "q"  = Just (Consonant  Voiceless Uvular    Plosive PulmonicEgressive)
analyzeIPA "ɢ"  = Just (Consonant  Voiced    Uvular    Plosive PulmonicEgressive)
analyzeIPA "ʔ"  = Just (Consonant  Voiceless Glottal   Plosive PulmonicEgressive)

-- Nasals:
analyzeIPA "m"  = Just (Consonant  Voiced Bilabial    Nasal PulmonicEgressive)
analyzeIPA "ɱ"  = Just (Consonant  Voiced LabioDental Nasal PulmonicEgressive)
analyzeIPA "n"  = Just (Consonant  Voiced Alveolar    Nasal PulmonicEgressive)
analyzeIPA "ɳ"  = Just (Consonant  Voiced Retroflex   Nasal PulmonicEgressive)
analyzeIPA "ɲ"  = Just (Consonant  Voiced Palatal     Nasal PulmonicEgressive)
analyzeIPA "ŋ"  = Just (Consonant  Voiced Velar       Nasal PulmonicEgressive)
analyzeIPA "ɴ"  = Just (Consonant  Voiced Uvular      Nasal PulmonicEgressive)

-- Trills:
analyzeIPA "ʙ"  = Just (Consonant  Voiced Bilabial Trill PulmonicEgressive)
analyzeIPA "r"  = Just (Consonant  Voiced Alveolar Trill PulmonicEgressive)
analyzeIPA "ʀ"  = Just (Consonant  Voiced Uvular   Trill PulmonicEgressive)

-- Taps or flaps:
analyzeIPA "ⱱ"  = Just (Consonant  Voiced LabioDental TapOrFlap PulmonicEgressive)
analyzeIPA "ɾ"  = Just (Consonant  Voiced Alveolar    TapOrFlap PulmonicEgressive)
analyzeIPA "ɽ"  = Just (Consonant  Voiced Retroflex   TapOrFlap PulmonicEgressive)

-- Fricatives:
analyzeIPA "ɸ"  = Just (Consonant  Voiceless Bilabial     Fricative PulmonicEgressive)
analyzeIPA "β"  = Just (Consonant  Voiced    Bilabial     Fricative PulmonicEgressive)
analyzeIPA "f"  = Just (Consonant  Voiceless LabioDental  Fricative PulmonicEgressive)
analyzeIPA "v"  = Just (Consonant  Voiced    LabioDental  Fricative PulmonicEgressive)
analyzeIPA "θ"  = Just (Consonant  Voiceless Dental       Fricative PulmonicEgressive)
analyzeIPA "ð"  = Just (Consonant  Voiced    Dental       Fricative PulmonicEgressive)
analyzeIPA "s"  = Just (Consonant  Voiceless Alveolar     Fricative PulmonicEgressive)
analyzeIPA "z"  = Just (Consonant  Voiced    Alveolar     Fricative PulmonicEgressive)
analyzeIPA "ʃ"  = Just (Consonant  Voiceless PostAlveolar Fricative PulmonicEgressive)
analyzeIPA "ʒ"  = Just (Consonant  Voiced    PostAlveolar Fricative PulmonicEgressive)
analyzeIPA "ʂ"  = Just (Consonant  Voiceless Retroflex    Fricative PulmonicEgressive)
analyzeIPA "ʐ"  = Just (Consonant  Voiced    Retroflex    Fricative PulmonicEgressive)
analyzeIPA "ç"  = Just (Consonant  Voiceless Palatal      Fricative PulmonicEgressive)
analyzeIPA "ʝ"  = Just (Consonant  Voiced    Palatal      Fricative PulmonicEgressive)
analyzeIPA "x"  = Just (Consonant  Voiceless Velar        Fricative PulmonicEgressive)
analyzeIPA "ɣ"  = Just (Consonant  Voiced    Velar        Fricative PulmonicEgressive)
analyzeIPA "χ"  = Just (Consonant  Voiceless Uvular       Fricative PulmonicEgressive)
analyzeIPA "ʁ"  = Just (Consonant  Voiced    Uvular       Fricative PulmonicEgressive)
analyzeIPA "ħ"  = Just (Consonant  Voiceless Pharyngeal   Fricative PulmonicEgressive)
analyzeIPA "ʕ"  = Just (Consonant  Voiced    Pharyngeal   Fricative PulmonicEgressive)
analyzeIPA "h"  = Just (Consonant  Voiceless Glottal      Fricative PulmonicEgressive)
analyzeIPA "ɦ"  = Just (Consonant  Voiced    Glottal      Fricative PulmonicEgressive)


-- Lateral Fricatives:
analyzeIPA "ɬ" = Just (Consonant  Voiceless Alveolar LateralFricative PulmonicEgressive)
analyzeIPA "ɮ" = Just (Consonant  Voiced    Alveolar LateralFricative PulmonicEgressive)


-- Approximants:
analyzeIPA "ʋ"  = Just (Consonant  Voiced LabioDental  Approximant PulmonicEgressive)
analyzeIPA "ɹ"  = Just (Consonant  Voiced Alveolar     Approximant PulmonicEgressive)
analyzeIPA "ɻ"  = Just (Consonant  Voiced Retroflex    Approximant PulmonicEgressive)
analyzeIPA "j"  = Just (Consonant  Voiced Palatal      Approximant PulmonicEgressive)
analyzeIPA "ɰ"  = Just (Consonant  Voiced Velar        Approximant PulmonicEgressive)

-- Lateral Approximants:
analyzeIPA "l"  = Just (Consonant  Voiced Alveolar  LateralApproximant PulmonicEgressive)
analyzeIPA "ɭ"  = Just (Consonant  Voiced Retroflex LateralApproximant PulmonicEgressive)
analyzeIPA "ʎ"  = Just (Consonant  Voiced Palatal   LateralApproximant PulmonicEgressive)
analyzeIPA "ʟ"  = Just (Consonant  Voiced Velar     LateralApproximant PulmonicEgressive)



-- Affricates
analyzeIPA "t͡ʃ" = Just (Consonant  Voiceless PostAlveolar Affricate PulmonicEgressive)
analyzeIPA "d͡ʒ" = Just (Consonant  Voiced    PostAlveolar Affricate PulmonicEgressive)
-- We should probably enforce use of the tie-bar underneath, otherwise
-- it would not be deterministic to determine whether two graphemes here
-- represent affricates or a plosive followed by a fricative.




-- Under the Other Symbols part of the IPA chart:

analyzeIPA "w" = Just (Consonant Voiced    LabialVelar    Approximant PulmonicEgressive)
analyzeIPA "ʍ" = Just (Consonant Voiceless LabialVelar    Fricative   PulmonicEgressive)
analyzeIPA "ɥ" = Just (Consonant Voiced    LabialPalatal  Approximant PulmonicEgressive)
analyzeIPA "ʜ" = Just (Consonant Voiceless Epiglottal     Fricative   PulmonicEgressive)
analyzeIPA "ʢ" = Just (Consonant Voiced    Epiglottal     Fricative   PulmonicEgressive)
analyzeIPA "ʡ" = Just (Consonant Voiceless Epiglottal     Plosive     PulmonicEgressive) -- Is the epiglottal plosive voiceless? The IPA chart does not specify.
analyzeIPA "ɕ" = Just (Consonant Voiceless AlveoloPalatal Fricative   PulmonicEgressive)
analyzeIPA "ʑ" = Just (Consonant Voiced    AlveoloPalatal Fricative   PulmonicEgressive)
analyzeIPA "ɺ" = Just (Consonant Voiced    Alveolar       LateralFlap PulmonicEgressive)

analyzeIPA "ɧ" = Just (Consonant Voiceless (Places [PostAlveolar, Velar]) Fricative PulmonicEgressive)

-- Other Consonants:
analyzeIPA "ʘ" = Just (Consonant Voiceless          Bilabial       Plosive        Click    )
analyzeIPA "ǀ" = Just (Consonant Voiceless          Dental         Plosive        Click    )
analyzeIPA "ǃ" = Just (Consonant Voiceless          Alveolar       Plosive        Click    ) -- Or it could be PostAlveolar.
analyzeIPA "ǂ" = Just (Consonant Voiceless          PalatoAlveolar Plosive        Click    )
analyzeIPA "ǁ" = Just (Consonant Voiceless          Alveolar       Lateral        Click    )
analyzeIPA "ɓ" = Just (Consonant Voiced             Bilabial       Plosive        Implosive)
analyzeIPA "ɗ" = Just (Consonant Voiced             Dental         Plosive        Implosive)  -- Or Alveolar
analyzeIPA "ʄ" = Just (Consonant Voiced             Palatal        Plosive        Implosive)
analyzeIPA "ɠ" = Just (Consonant Voiced             Velar          Plosive        Implosive)
analyzeIPA "ʛ" = Just (Consonant Voiced             Uvular         Plosive        Implosive)

-- Close Vowels:
analyzeIPA "i"  = Just (Vowel  Close Front   Unrounded Voiced)
analyzeIPA "y"  = Just (Vowel  Close Front   Rounded   Voiced)
analyzeIPA "ɨ"  = Just (Vowel  Close Central Unrounded Voiced)
analyzeIPA "ʉ"  = Just (Vowel  Close Central Rounded   Voiced)
analyzeIPA "ɯ"  = Just (Vowel  Close Back    Unrounded Voiced)
analyzeIPA "u"  = Just (Vowel  Close Back    Rounded   Voiced)

-- Near-close Vowels:
analyzeIPA "ɪ"  = Just (Vowel NearClose Front Unrounded Voiced)
analyzeIPA "ʏ"  = Just (Vowel NearClose Front Rounded   Voiced)
analyzeIPA "ʊ"  = Just (Vowel NearClose Back  Rounded   Voiced)

-- Close-mid Vowels:
analyzeIPA "e"  = Just (Vowel  CloseMid Front   Unrounded Voiced)
analyzeIPA "ø"  = Just (Vowel  CloseMid Front   Rounded   Voiced)
analyzeIPA "ɘ"  = Just (Vowel  CloseMid Central Unrounded Voiced)
analyzeIPA "ɵ"  = Just (Vowel  CloseMid Central Rounded   Voiced)
analyzeIPA "ɤ"  = Just (Vowel  CloseMid Back    Unrounded Voiced)
analyzeIPA "o"  = Just (Vowel  CloseMid Back    Rounded   Voiced)

-- Mid Vowels:
analyzeIPA "ə"  = Just (Vowel Mid Central Unrounded Voiced)


-- Open-mid Vowels:
analyzeIPA "ɛ"  = Just (Vowel  OpenMid Front   Unrounded Voiced)
analyzeIPA "œ"  = Just (Vowel  OpenMid Front   Rounded   Voiced)
analyzeIPA "ɜ"  = Just (Vowel  OpenMid Central Unrounded Voiced)
analyzeIPA "ɞ"  = Just (Vowel  OpenMid Central Rounded   Voiced)
analyzeIPA "ʌ"  = Just (Vowel  OpenMid Back    Unrounded Voiced)
analyzeIPA "ɔ"  = Just (Vowel  OpenMid Back    Rounded   Voiced)

-- Near-open
analyzeIPA "æ"  = Just (Vowel  NearOpen Front   Unrounded  Voiced)
analyzeIPA "ɐ"  = Just (Vowel  NearOpen Central Unrounded  Voiced)

-- Open Vowels:
analyzeIPA "a"  = Just (Vowel  Open Front Unrounded Voiced)
analyzeIPA "ɶ"  = Just (Vowel  Open Front Rounded   Voiced)
analyzeIPA "ɑ"  = Just (Vowel  Open Back  Unrounded Voiced)
analyzeIPA "ɒ"  = Just (Vowel  Open Back  Rounded   Voiced)



-- Handle Diacritics:
analyzeIPA ipaText =
  case [last ipaText] of
    "̥" →
      let fullGrapheme = analyzeIPA (init ipaText)
      in case fullGrapheme of
              Just (Consonant _ place manner airstream)    → Just (Consonant Voiceless place manner airstream)
              Just (Vowel height backness rounding _  )    → Just (Vowel height backness rounding Voiceless)
              _                                            → Nothing
    "̬" →
      let fullGrapheme = analyzeIPA (init ipaText)
      in case fullGrapheme of
              Just (Consonant _ place manner airstream)    → Just (Consonant Voiced place manner airstream)
              Just (Vowel height backness rounding _  )    → Just (Vowel height backness rounding Voiced)
              _                                            → Nothing

    "ʰ" →
      let fullGrapheme = analyzeIPA (init ipaText)
      in case fullGrapheme of
              Just (Consonant Voiced place manner airstream   ) → Just (Consonant VoicedAspirated    place manner airstream)
              Just (Consonant Voiceless place manner airstream) → Just (Consonant VoicelessAspirated place manner airstream)
              Just (Vowel height backness rounding voicing    ) → Just (Vowel height backness rounding voicing             )
              anythingElse                                      → anythingElse
              -- (About the preceding line:) It is strange but we will just do nothing if they give us an aspirated vowel.
              -- since we have no way to represent it in the type system. to do: determine
              -- if the idea of an aspirated vowel makes sense
    _ → Nothing -- not recognized.


constructIPA ∷ Phonet → IPAText
constructIPA phoneme =
  case constructIPARecursive 3 0 phoneme of
    Just graphemes → graphemes
    Nothing        → "∅"

constructIPARecursive ∷ Int → Int → Phonet → Maybe IPAText
constructIPARecursive recursionLimit recursionLevel _
  | recursionLevel ≡ recursionLimit = Nothing

-- Plosives:
constructIPARecursive _ _ (Consonant  Voiceless          Bilabial                       Plosive            PulmonicEgressive) = Just "p"
constructIPARecursive _ _ (Consonant  Voiced             Bilabial                       Plosive            PulmonicEgressive) = Just "b"
constructIPARecursive _ _ (Consonant  Voiceless          Alveolar                       Plosive            PulmonicEgressive) = Just "t"
constructIPARecursive _ _ (Consonant  Voiced             Alveolar                       Plosive            PulmonicEgressive) = Just "d"
constructIPARecursive _ _ (Consonant  Voiceless          Retroflex                      Plosive            PulmonicEgressive) = Just "ʈ"
constructIPARecursive _ _ (Consonant  Voiced             Retroflex                      Plosive            PulmonicEgressive) = Just "ɖ"
constructIPARecursive _ _ (Consonant  Voiceless          Palatal                        Plosive            PulmonicEgressive) = Just "c"
constructIPARecursive _ _ (Consonant  Voiced             Palatal                        Plosive            PulmonicEgressive) = Just "ɟ"
constructIPARecursive _ _ (Consonant  Voiceless          Velar                          Plosive            PulmonicEgressive) = Just "k"
constructIPARecursive _ _ (Consonant  Voiced             Velar                          Plosive            PulmonicEgressive) = Just "g"
constructIPARecursive _ _ (Consonant  Voiceless          Uvular                         Plosive            PulmonicEgressive) = Just "q"
constructIPARecursive _ _ (Consonant  Voiced             Uvular                         Plosive            PulmonicEgressive) = Just "ɢ"
constructIPARecursive _ _ (Consonant  Voiceless          Glottal                        Plosive            PulmonicEgressive) = Just "ʔ"  -- Nasals (next line):
constructIPARecursive _ _ (Consonant  Voiced             Bilabial                       Nasal              PulmonicEgressive) = Just "m"
constructIPARecursive _ _ (Consonant  Voiced             LabioDental                    Nasal              PulmonicEgressive) = Just "ɱ"
constructIPARecursive _ _ (Consonant  Voiced             Alveolar                       Nasal              PulmonicEgressive) = Just "n"
constructIPARecursive _ _ (Consonant  Voiced             Retroflex                      Nasal              PulmonicEgressive) = Just "ɳ"
constructIPARecursive _ _ (Consonant  Voiced             Palatal                        Nasal              PulmonicEgressive) = Just "ɲ"
constructIPARecursive _ _ (Consonant  Voiced             Velar                          Nasal              PulmonicEgressive) = Just "ŋ"
constructIPARecursive _ _ (Consonant  Voiced             Uvular                         Nasal              PulmonicEgressive) = Just "ɴ"  -- Trills (next line):
constructIPARecursive _ _ (Consonant  Voiced             Bilabial                       Trill              PulmonicEgressive) = Just "ʙ"
constructIPARecursive _ _ (Consonant  Voiced             Alveolar                       Trill              PulmonicEgressive) = Just "r"
constructIPARecursive _ _ (Consonant  Voiced             Uvular                         Trill              PulmonicEgressive) = Just "ʀ"  -- Taps or flaps (next line):
constructIPARecursive _ _ (Consonant  Voiced             LabioDental                    TapOrFlap          PulmonicEgressive) = Just "ⱱ"
constructIPARecursive _ _ (Consonant  Voiced             Alveolar                       TapOrFlap          PulmonicEgressive) = Just "ɾ"
constructIPARecursive _ _ (Consonant  Voiced             Retroflex                      TapOrFlap          PulmonicEgressive) = Just "ɽ"  -- Fricatives (next line):
constructIPARecursive _ _ (Consonant  Voiceless          Bilabial                       Fricative          PulmonicEgressive) = Just "ɸ"
constructIPARecursive _ _ (Consonant  Voiced             Bilabial                       Fricative          PulmonicEgressive) = Just "β"
constructIPARecursive _ _ (Consonant  Voiceless          LabioDental                    Fricative          PulmonicEgressive) = Just "f"
constructIPARecursive _ _ (Consonant  Voiced             LabioDental                    Fricative          PulmonicEgressive) = Just "v"
constructIPARecursive _ _ (Consonant  Voiceless          Dental                         Fricative          PulmonicEgressive) = Just "θ"
constructIPARecursive _ _ (Consonant  Voiced             Dental                         Fricative          PulmonicEgressive) = Just "ð"
constructIPARecursive _ _ (Consonant  Voiceless          Alveolar                       Fricative          PulmonicEgressive) = Just "s"
constructIPARecursive _ _ (Consonant  Voiced             Alveolar                       Fricative          PulmonicEgressive) = Just "z"
constructIPARecursive _ _ (Consonant  Voiceless          PostAlveolar                   Fricative          PulmonicEgressive) = Just "ʃ"
constructIPARecursive _ _ (Consonant  Voiced             PostAlveolar                   Fricative          PulmonicEgressive) = Just "ʒ"
constructIPARecursive _ _ (Consonant  Voiceless          Retroflex                      Fricative          PulmonicEgressive) = Just "ʂ"
constructIPARecursive _ _ (Consonant  Voiced             Retroflex                      Fricative          PulmonicEgressive) = Just "ʐ"
constructIPARecursive _ _ (Consonant  Voiceless          Palatal                        Fricative          PulmonicEgressive) = Just "ç"
constructIPARecursive _ _ (Consonant  Voiced             Palatal                        Fricative          PulmonicEgressive) = Just "ʝ"
constructIPARecursive _ _ (Consonant  Voiceless          Velar                          Fricative          PulmonicEgressive) = Just "x"
constructIPARecursive _ _ (Consonant  Voiced             Velar                          Fricative          PulmonicEgressive) = Just "ɣ"
constructIPARecursive _ _ (Consonant  Voiceless          Uvular                         Fricative          PulmonicEgressive) = Just "χ"
constructIPARecursive _ _ (Consonant  Voiced             Uvular                         Fricative          PulmonicEgressive) = Just "ʁ"
constructIPARecursive _ _ (Consonant  Voiceless          Pharyngeal                     Fricative          PulmonicEgressive) = Just "ħ"
constructIPARecursive _ _ (Consonant  Voiced             Pharyngeal                     Fricative          PulmonicEgressive) = Just "ʕ"
constructIPARecursive _ _ (Consonant  Voiceless          Glottal                        Fricative          PulmonicEgressive) = Just "h"
constructIPARecursive _ _ (Consonant  Voiced             Glottal                        Fricative          PulmonicEgressive) = Just "ɦ"  -- Lateral Fricatives (next line):
constructIPARecursive _ _ (Consonant  Voiceless          Alveolar                       LateralFricative   PulmonicEgressive) = Just "ɬ"
constructIPARecursive _ _ (Consonant  Voiced             Alveolar                       LateralFricative   PulmonicEgressive) = Just "ɮ" -- Approximants (next line):
constructIPARecursive _ _ (Consonant  Voiced             LabioDental                    Approximant        PulmonicEgressive) = Just "ʋ"
constructIPARecursive _ _ (Consonant  Voiced             Alveolar                       Approximant        PulmonicEgressive) = Just "ɹ"
constructIPARecursive _ _ (Consonant  Voiced             Retroflex                      Approximant        PulmonicEgressive) = Just "ɻ"
constructIPARecursive _ _ (Consonant  Voiced             Palatal                        Approximant        PulmonicEgressive) = Just "j"
constructIPARecursive _ _ (Consonant  Voiced             Velar                          Approximant        PulmonicEgressive) = Just "ɰ"  -- Lateral Approximants (next line):
constructIPARecursive _ _ (Consonant  Voiced             Alveolar                       LateralApproximant PulmonicEgressive) = Just "l"
constructIPARecursive _ _ (Consonant  Voiced             Retroflex                      LateralApproximant PulmonicEgressive) = Just "ɭ"
constructIPARecursive _ _ (Consonant  Voiced             Palatal                        LateralApproximant PulmonicEgressive) = Just "ʎ"
constructIPARecursive _ _ (Consonant  Voiced             Velar                          LateralApproximant PulmonicEgressive) = Just "ʟ" -- Affricates (next line)
constructIPARecursive _ _ (Consonant  Voiceless          PostAlveolar                   Affricate          PulmonicEgressive) = Just "t͡ʃ"
constructIPARecursive _ _ (Consonant  Voiced             PostAlveolar                   Affricate          PulmonicEgressive) = Just "d͡ʒ"
constructIPARecursive _ _ (Consonant  Voiceless          Bilabial                       Affricate          PulmonicEgressive) = Just "p͡ɸ"
constructIPARecursive _ _ (Consonant  Voiceless          Alveolar                       Affricate          PulmonicEgressive) = Just "t͜s"
constructIPARecursive _ _ (Consonant  Voiced             Alveolar                       Affricate          PulmonicEgressive) = Just "d͡z"
constructIPARecursive _ _ (Consonant  Voiceless          Velar                          Affricate          PulmonicEgressive) = Just "k͡x"
constructIPARecursive _ _ (Consonant  Voiceless          Uvular                         Affricate          PulmonicEgressive) = Just "q͡χ" -- Under the Other Symbols part of the IPA chart:
constructIPARecursive _ _ (Consonant  Voiced             LabialVelar                    Approximant        PulmonicEgressive) = Just "w"
constructIPARecursive _ _ (Consonant  Voiceless          LabialVelar                    Fricative          PulmonicEgressive) = Just "ʍ"
constructIPARecursive _ _ (Consonant  Voiced             LabialPalatal                  Approximant        PulmonicEgressive) = Just "ɥ"
constructIPARecursive _ _ (Consonant  Voiceless          Epiglottal                     Fricative          PulmonicEgressive) = Just "ʜ"
constructIPARecursive _ _ (Consonant  Voiced             Epiglottal                     Fricative          PulmonicEgressive) = Just "ʢ"
constructIPARecursive _ _ (Consonant  Voiceless          Epiglottal                     Plosive            PulmonicEgressive) = Just "ʡ"-- Is the epiglottal plosive voiceless? The IPA chart does not specify.
constructIPARecursive _ _ (Consonant  Voiceless          AlveoloPalatal                 Fricative          PulmonicEgressive) = Just "ɕ"
constructIPARecursive _ _ (Consonant  Voiced             AlveoloPalatal                 Fricative          PulmonicEgressive) = Just "ʑ"
constructIPARecursive _ _ (Consonant  Voiced             Alveolar                       LateralFlap        PulmonicEgressive) = Just "ɺ"
constructIPARecursive _ _ (Consonant  Voiceless          (Places [PostAlveolar, Velar]) Fricative          PulmonicEgressive) = Just "ɧ" -- Other Consonants:
constructIPARecursive _ _ (Consonant  Voiceless          Bilabial                       Plosive            Click            ) = Just "ʘ"
constructIPARecursive _ _ (Consonant  Voiceless          Dental                         Plosive            Click            ) = Just "ǀ"
constructIPARecursive _ _ (Consonant  Voiceless          Alveolar                       Plosive            Click            ) = Just "ǃ" -- Or it could be PostAlveolar.
constructIPARecursive _ _ (Consonant  Voiceless          PalatoAlveolar                 Plosive            Click            ) = Just "ǂ"
constructIPARecursive _ _ (Consonant  Voiceless          Alveolar                       Lateral            Click            ) = Just "ǁ"
constructIPARecursive _ _ (Consonant  Voiced             Bilabial                       Plosive            Implosive        ) = Just "ɓ"
constructIPARecursive _ _ (Consonant  Voiced             Dental                         Plosive            Implosive        ) = Just "ɗ"  -- Or Alveolar
constructIPARecursive _ _ (Consonant  Voiced             Palatal                        Plosive            Implosive        ) = Just "ʄ"
constructIPARecursive _ _ (Consonant  Voiced             Velar                          Plosive            Implosive        ) = Just "ɠ"
constructIPARecursive _ _ (Consonant  Voiced             Uvular                         Plosive            Implosive        ) = Just "ʛ" -- Close Vowels (next line):
constructIPARecursive _ _ (Vowel      Close              Front                          Unrounded          Voiced           ) = Just "i"
constructIPARecursive _ _ (Vowel      Close              Front                          Rounded            Voiced           ) = Just "y"
constructIPARecursive _ _ (Vowel      Close              Central                        Unrounded          Voiced           ) = Just "ɨ"
constructIPARecursive _ _ (Vowel      Close              Central                        Rounded            Voiced           ) = Just "ʉ"
constructIPARecursive _ _ (Vowel      Close              Back                           Unrounded          Voiced           ) = Just "ɯ"
constructIPARecursive _ _ (Vowel      Close              Back                           Rounded            Voiced           ) = Just "u" -- Near-close Vowels (next line):
constructIPARecursive _ _ (Vowel      NearClose          Front                          Unrounded          Voiced           ) = Just "ɪ"
constructIPARecursive _ _ (Vowel      NearClose          Front                          Rounded            Voiced           ) = Just "ʏ"
constructIPARecursive _ _ (Vowel      NearClose          Back                           Rounded            Voiced           ) = Just "ʊ" -- Close-mid Vowels (next line):
constructIPARecursive _ _ (Vowel      CloseMid           Front                          Unrounded          Voiced           ) = Just "e"
constructIPARecursive _ _ (Vowel      CloseMid           Front                          Rounded            Voiced           ) = Just "ø"
constructIPARecursive _ _ (Vowel      CloseMid           Central                        Unrounded          Voiced           ) = Just "ɘ"
constructIPARecursive _ _ (Vowel      CloseMid           Central                        Rounded            Voiced           ) = Just "ɵ"
constructIPARecursive _ _ (Vowel      CloseMid           Back                           Unrounded          Voiced           ) = Just "ɤ"
constructIPARecursive _ _ (Vowel      CloseMid           Back                           Rounded            Voiced           ) = Just "o" -- Mid Vowels (next line):
constructIPARecursive _ _ (Vowel      Mid                Central                        Unrounded          Voiced           ) = Just "ə" -- Open-mid Vowels (next line):
constructIPARecursive _ _ (Vowel      OpenMid            Front                          Unrounded          Voiced           ) = Just "ɛ"
constructIPARecursive _ _ (Vowel      OpenMid            Front                          Rounded            Voiced           ) = Just "œ"
constructIPARecursive _ _ (Vowel      OpenMid            Central                        Unrounded          Voiced           ) = Just "ɜ"
constructIPARecursive _ _ (Vowel      OpenMid            Central                        Rounded            Voiced           ) = Just "ɞ"
constructIPARecursive _ _ (Vowel      OpenMid            Back                           Unrounded          Voiced           ) = Just "ʌ"
constructIPARecursive _ _ (Vowel      OpenMid            Back                           Rounded            Voiced           ) = Just "ɔ" -- Near-open (next line)
constructIPARecursive _ _ (Vowel      NearOpen           Front                          Unrounded          Voiced           ) = Just "æ"
constructIPARecursive _ _ (Vowel      NearOpen           Central                        Unrounded          Voiced           ) = Just "ɐ" -- Open Vowels (next line):
constructIPARecursive _ _ (Vowel      Open               Front                          Unrounded          Voiced           ) = Just "a"
constructIPARecursive _ _ (Vowel      Open               Front                          Rounded            Voiced           ) = Just "ɶ"
constructIPARecursive _ _ (Vowel      Open               Back                           Unrounded          Voiced           ) = Just "ɑ"
constructIPARecursive _ _ (Vowel      Open               Back                           Rounded            Voiced           ) = Just "ɒ"




-- The following two lines are commented out, because I am unsure about their place of articulation:
-- constructIPARecursive _ _ (Consonant  Voiceless LabialVelar? Affricate PulmonicEgressive) = "k͡p"
-- constructIPARecursive _ _ (Consonant  Voiceless Palatal (or AlveolaPalatal?) Affricate PulmonicEgressive) = "c͡ɕ"




  -- If it can represent it as a single character it will
  -- return the single character result (i.e. without diacritics),
  -- otherwise
  -- it will try to represent it in IPA with more than
  -- one character


constructIPARecursive recursionLimit recursionLevel  (Consonant  x PostAlveolar y z)
  | recursionLevel <  recursionLimit
    = case constructIPARecursive recursionLimit (1 + recursionLevel) (Consonant x Alveolar y z) of
           Nothing → Nothing
           Just regularIPA → Just (regularIPA ⧺ "̠")  -- Add the diacritic for "retracted"



-- If there isn't a symbol, and the consonant we want is voiceless,
-- Just take the symbol for a voiced consonant,
-- and then put that diacritic that means voiceless after.
-- (The following two definitions are intended to implement that)
-- Add the small circle diacritic to consonants to make them voiceless.
constructIPARecursive recursionLimit recursionLevel  (Consonant Voiceless x y z)
  | recursionLevel <  recursionLimit
    = case constructIPARecursive recursionLimit (1 + recursionLevel)  (Consonant Voiced x y z) of
           Nothing → Nothing
           Just regularIPA → Just (regularIPA ⧺ "̥") -- add diacritic for voiceless

-- Add the small circle diacritic to vowels to make them voiceless.
constructIPARecursive recursionLimit recursionLevel (Vowel x y z Voiceless)
  | recursionLevel <  recursionLimit
    = case constructIPARecursive recursionLimit (1 + recursionLevel) (Vowel x y z Voiced) of
           Nothing → Nothing
           Just regularIPA → Just (regularIPA ⧺ "̥")

-- If there is no way to express a voiced consonant in a single
-- grapheme add a diacritic to the grapheme that represents
-- the voiceless counterpart.
constructIPARecursive recursionLimit recursionLevel  (Consonant Voiced x y z)
  | recursionLevel <  recursionLimit
    = case constructIPARecursive recursionLimit (1 + recursionLevel) (Consonant Voiceless x y z) of
           Nothing → Nothing
           Just regularIPA → Just (regularIPA ⧺ "̬")

constructIPARecursive recursionLimit recursionLevel  (Vowel x y z Voiced)
  | recursionLevel <  recursionLimit
    = case constructIPARecursive recursionLimit (1 + recursionLevel) (Vowel x y z Voiceless) of
           Nothing → Nothing
           Just regularIPA → Just (regularIPA ⧺ "̬")

constructIPARecursive recursionLimit recursionLevel  c@(Consonant VoicedAspirated _ _ PulmonicEgressive)
  | recursionLevel <  recursionLimit
    = case constructIPARecursive recursionLimit (1 + recursionLevel) (deaspirate c) of
           Nothing         → Nothing
           Just regularIPA → Just (regularIPA ⧺ "ʰ")

constructIPARecursive recursionLimit recursionLevel  c@(Consonant VoicelessAspirated _ _ PulmonicEgressive)
  | recursionLevel <  recursionLimit
    = case constructIPARecursive recursionLimit (1 + recursionLevel) (deaspirate c) of
           Nothing         → Nothing
           Just regularIPA → Just (regularIPA ⧺ "ʰ")

constructIPARecursive recursionLimit recursionLevel  c@(Consonant CreakyVoiced _ _ PulmonicEgressive)
  | recursionLevel <  recursionLimit
    = case constructIPARecursive recursionLimit (1 + recursionLevel) (deaspirate c) of
           Nothing         → Nothing
           Just regularIPA → Just (regularIPA ⧺ "̰")


constructIPARecursive _ _ _
    = Nothing




deaspirate ∷ Phonet → Phonet
deaspirate (Consonant VoicedAspirated place manner airstream) =
  (Consonant Voiced place manner airstream)

deaspirate (Consonant VoicelessAspirated place1 manner1 airstream1) =
  (Consonant Voiceless place1 manner1 airstream1)

deaspirate x = x


constructDeconstruct ∷ (Phonet → Phonet) → IPAText → IPAText
constructDeconstruct func x =
  let something = analyzeIPA x
  in case something of
       Nothing → "∅"
       Just phonet → constructIPA (func phonet)

voicedIPA ∷ IPAText → IPAText
voicedIPA = constructDeconstruct voicedPhonet

devoicedIPA ∷ IPAText → IPAText
devoicedIPA = constructDeconstruct devoicedPhonet

spirantizedIPA ∷ IPAText → IPAText
spirantizedIPA = constructDeconstruct spirantizedPhonet


{-|
Return an english description of a phoneme,
given a phoneme's representation in the
international phonetic alphabet.
  |-}
describeIPA ∷ IPAText → String
describeIPA = show ∘ analyzeIPA



-- Go to Section 12.2 of the textbook to understand
-- the concept of phonological features.


-- Given a binary feature, and another feature.
-- returns whether they are the same kind of feature.
-- They don't have to be the same polarity.
-- For example, [+voice] and [−voice] are mutually relevant features.
--   As are [+sonorant] and [+sonorant].
--   But [+sonorant] and [+voice] are not relevant because
-- "voice" and "sonorant" are different.
relevantBinary ∷ (Polarity → PhonemeFeature) → PhonemeFeature → Bool
relevantBinary feature otherFeature =
  otherFeature ≡ feature Plus ∨ otherFeature ≡ feature Minus

binaryDifference ::
          (Polarity → PhonemeFeature)
                    → [PhonemeFeature]
                    → [PhonemeFeature]
                    → (Maybe PhonemeFeature, Maybe PhonemeFeature)
binaryDifference feature list1 list2
  | null list1Relevant ∧ null list2Relevant
       ∨ (not (null list1Relevant) ∧ not (null list2Relevant)
       ∧ head list1Relevant ≡ head list2Relevant)
  = (Nothing, Nothing)
  | not (null list1Relevant) ∧ not (null list2Relevant)
  = (Just (head list1Relevant), Just (head list2Relevant))
  | length list1Relevant > length list2Relevant
  = (Just (head list1Relevant), Nothing)
  | otherwise
  = (Nothing, Just (head list2Relevant))
   where
   list1Relevant = filter (relevantBinary feature) list1
   list2Relevant = filter (relevantBinary feature) list2


unaryDifference ∷ PhonemeFeature
                → [PhonemeFeature]
                → [PhonemeFeature]
                → (Maybe PhonemeFeature, Maybe PhonemeFeature)
unaryDifference feature list1 list2
  | elem feature list1 ≡ elem feature list2    = (Nothing, Nothing)
  | elem feature list1 ∧ notElem feature list2 = (Just feature, Nothing)
  | otherwise                                   = (Nothing, Just feature)



-- | This function takes two lists of phoneme features
-- and returns how they differ. Any phonemic
-- feature present in one list, and absent in the other
-- will be represented; and any phonemic
-- feature that is positive in one list but absent
-- in the other will be represented.
difference ∷ [PhonemeFeature]
           → [PhonemeFeature]
           → [(Maybe PhonemeFeature, Maybe PhonemeFeature)]
difference list1 list2 =
  [ binaryDifference SyllabicFeature           list1 list2
  , binaryDifference ConsonantalFeature        list1 list2
  , binaryDifference SonorantFeature           list1 list2
  , binaryDifference ContinuantFeature         list1 list2
  , binaryDifference VoiceFeature              list1 list2
  , binaryDifference AdvancedTongueRootFeature list1 list2
  , unaryDifference  NasalFeature              list1 list2
  , unaryDifference  LateralFeature            list1 list2
  , unaryDifference  DelayedReleaseFeature     list1 list2
  , unaryDifference  SpreadGlottisFeature      list1 list2
  , unaryDifference  ConstrictedGlottisFeature list1 list2
  , unaryDifference  LabialFeature             list1 list2
  , unaryDifference  CoronalFeature            list1 list2
  , unaryDifference  DorsalFeature             list1 list2
  , unaryDifference  PharyngealFeature         list1 list2
  , unaryDifference  LaryngealFeature          list1 list2
  , binaryDifference RoundFeature              list1 list2
  , binaryDifference AnteriorFeature           list1 list2
  , binaryDifference DistributedFeature        list1 list2
  , binaryDifference StridentFeature           list1 list2
  , binaryDifference HighFeature               list1 list2
  , binaryDifference LowFeature                list1 list2
  , binaryDifference BackFeature               list1 list2
  ]

{-|
Vowels are [+syllabic]
Consonants (glides included) are [-syllabic].

(Source: page 258)
|-}
syllabic ∷ Phonet → Maybe PhonemeFeature
syllabic (Vowel     _ _ _ _) = Just (SyllabicFeature Plus)
syllabic (Consonant _ _ _ _) = Just (SyllabicFeature Minus)

{-|
Whether a segment is a glide.
|-}
isGlide ∷ Phonet → Bool
isGlide (Consonant _ Palatal       Approximant PulmonicEgressive) = True
isGlide (Consonant _ LabialVelar   Approximant PulmonicEgressive) = True
isGlide (Consonant _ LabialPalatal Approximant PulmonicEgressive) = True
isGlide (Consonant _ Velar         Approximant PulmonicEgressive) = True
isGlide _                                                         = False

{-|
Vowels are [-consonantal].
Glides are [-consonantal].
Consonants (that are not glides) are [+consonantal].

(Source: page 258)
|-}
consonantal ∷ Phonet → Maybe PhonemeFeature
consonantal (Vowel _ _ _ _) = Just (ConsonantalFeature Minus)
consonantal consonant@(Consonant _ _ _ _)
  | isGlide consonant = Just (ConsonantalFeature Minus)
  | otherwise         = Just (ConsonantalFeature Plus)


{-|
Oral stops are [-sonorant].
Affricates are [-sonorant].
Fricatives are [-sonorant].
Nasals are [+sonorant].
Approximants are [+sonorant].
Laterals are [+sonorant].
Vowels are [+sonorant].
Glides are [+sonorant].

(Source: page 258)
|-}
sonorant ∷ Phonet → Maybe PhonemeFeature
sonorant (Consonant _ _ Plosive     _) = Just (SonorantFeature Minus)
sonorant (Consonant _ _ Affricate   _) = Just (SonorantFeature Minus)
sonorant (Consonant _ _ Fricative   _) = Just (SonorantFeature Minus)
sonorant (Consonant _ _ Nasal       _) = Just (SonorantFeature Plus)
sonorant (Consonant _ _ Approximant _) = Just (SonorantFeature Plus)
sonorant (Consonant _ _ Lateral     _) = Just (SonorantFeature Plus)
sonorant (Vowel               _ _ _ _) = Just (SonorantFeature Plus)
sonorant consonant@(Consonant _ _ _ _)
  | isGlide consonant = Just (SonorantFeature Plus)
  | otherwise         = Just (SonorantFeature Minus)

{-|
Oral stops are [-continuant].
Nasals stops are [-continuant].
Affricates are [-continuant].
Fricatives are [+continuant].
Approximants are [+continuant].
Vowels are [+continuant].
Glides are [+continuant].

(Source: page 258)

  Aside: we do not define lateral approximants for [+/-continuant] because the
  textbook puts it in parentheses. Usually this means, it depends on
  the language under study or
  it depends on the linguist.
  Lateral approximants may be considered [+continuant]. (arguable)
  (see chart on page 259))

|-}
continuant ∷ Phonet → Maybe PhonemeFeature
continuant (Consonant _ _ Plosive            _) = Just (ContinuantFeature Minus)
continuant (Consonant _ _ Nasal              _) = Just (ContinuantFeature Minus)
continuant (Consonant _ _ Affricate          _) = Just (ContinuantFeature Minus)
continuant (Consonant _ _ Approximant        _) = Just (ContinuantFeature Plus)
continuant (Vowel     _ _ _                  _) = Just (ContinuantFeature Plus)
continuant consonant@(Consonant _ _ _ _)
  | isGlide consonant = Just (ContinuantFeature Plus)
  | otherwise         = Nothing

{-|
Nasal consonants are [nasal].
-- to do: add support for nasal vowels.
All other segments are not defined for [nasal].
|-}
nasal ∷ Phonet → Maybe PhonemeFeature
nasal (Consonant _ _ Nasal _) = Just NasalFeature
nasal _                       = Nothing


{-|
Lateral consonants are [lateral].
Lateral Approximant consonants are [lateral].
Lateral fricative consonants are [lateral].
Lateral flap consonants are [lateral].
All other segments are not defined for [lateral].
|-}
lateral ∷ Phonet → Maybe PhonemeFeature
lateral (Consonant _ _ Lateral            _) = Just LateralFeature
lateral (Consonant _ _ LateralApproximant _) = Just LateralFeature
lateral (Consonant _ _ LateralFricative   _) = Just LateralFeature
lateral (Consonant _ _ LateralFlap        _) = Just LateralFeature
lateral _                                    = Nothing

{-|
Affricates are [+delayed release].
All other segments are [-delayed release].

(Source: page 260)
|-}
delayedRelease ∷ Phonet → Maybe PhonemeFeature
delayedRelease (Consonant _ _ Affricate _) = Just DelayedReleaseFeature
delayedRelease _                           = Nothing


{-|
Bilabial consonants are [labial].
Labio-dental consonants are [labial].
All other segments are undefined for [labial].

(Source: page 264)
|-}
labial ∷ Phonet → Maybe PhonemeFeature
labial (Consonant _ Bilabial    _ _) = Just LabialFeature
labial (Consonant _ LabioDental _ _) = Just LabialFeature
labial _                             = Nothing


{-|
Dentals are [coronal].
Alveolars are [coronal] also.
Alveolopalatals are [coronal] also.
Retroflexes are [coronal] also.
Palatals are [coronal] also.

Post-alveolars are [coronal] also.

All other sounds are undefined for [coronal].

(Source: page 264)
(The fact that Post-alveolar consonants are coronal is indicated by
 Table 12. on page 265.)
|-}
coronal ∷ Phonet → Maybe PhonemeFeature
coronal (Consonant _ Dental         _ _) = Just CoronalFeature
coronal (Consonant _ Alveolar       _ _) = Just CoronalFeature
coronal (Consonant _ AlveoloPalatal _ _) = Just CoronalFeature
coronal (Consonant _ Retroflex      _ _) = Just CoronalFeature
coronal (Consonant _ Palatal        _ _) = Just CoronalFeature

coronal (Consonant _ PostAlveolar   _ _) = Just CoronalFeature

coronal _                                = Nothing


{-|
Palatals are [dorsal].

  Aside: alveolo-palatals do not seem to be dorsals,
  although the table 12.4 is confusing
  because it uses the IPA symbol for one.
  TODO: find more information on whether
  alveolo-palatals are [dorsal].

Velars are [dorsal].
Uvulars are [dorsal].
All other segments are undefined for [dorsal].
|-}
dorsal ∷ Phonet → Maybe PhonemeFeature
dorsal (Consonant _ Palatal        _ _) = Just DorsalFeature
dorsal (Consonant _ Velar          _ _) = Just DorsalFeature
dorsal (Consonant _ Uvular         _ _) = Just DorsalFeature
dorsal _                                = Nothing


{-|
Pharyngeal fricatives are [pharyngeal].
All other segments are undefined for [pharyngeal].

(Source: page 264)
|-}
pharyngeal ∷ Phonet → Maybe PhonemeFeature
pharyngeal (Consonant _ Pharyngeal Fricative _) = Just PharyngealFeature
pharyngeal _                                    = Nothing

{-|
Glottal consonants are [laryngeal].
All other segments are undefined for [laryngeal].

(Source: page 265)
|-}
laryngeal ∷ Phonet → Maybe PhonemeFeature
laryngeal (Consonant _ Glottal _ _ ) = Just LaryngealFeature
laryngeal _                          = Nothing


{-|
Voiced Aspirated consonants are [+voice].
Voiced consonants are [+voice].
Voiced vowels are [+voice].
All other segments are [-voice].
|-}
voice ∷ Phonet → Maybe PhonemeFeature
voice (Consonant Voiceless Glottal Plosive PulmonicEgressive) = Just (VoiceFeature Minus)
-- The voiceless glottal plosive is [-voice]
voice (Consonant VoicedAspirated _ _ _)  = Just (VoiceFeature Plus)
voice (Consonant Voiced          _ _ _)  = Just (VoiceFeature Plus)
voice (Vowel _ _ _               Voiced) = Just (VoiceFeature Plus)
voice _                                  = Just (VoiceFeature Minus)

{-|
Voiceless aspirated plosives are [spread glottis].
Voiced aspirated plosives are [spread glottis].
All other segments are not defined for [spread glottis].
(Source: page 262)
|-}
spreadGlottis ∷ Phonet → Maybe PhonemeFeature
spreadGlottis (Consonant VoicelessAspirated _ Plosive _) = Just SpreadGlottisFeature
spreadGlottis (Consonant VoicedAspirated    _ Plosive _) = Just SpreadGlottisFeature
spreadGlottis _                                          = Nothing


{-|
Ejectives have the feature [constricted glottis].
Glottal stop have the feature [constricted glottis].
Creaky voiced sonorants have the feature [constricted glottis].

(Source: page 262)
|-}
constrictedGlottis ∷ Phonet → Maybe PhonemeFeature
constrictedGlottis (Consonant _ Glottal Plosive _) =
  Just ConstrictedGlottisFeature
constrictedGlottis consonant@(Consonant CreakyVoiced _ _ _) =
  if sonorant consonant ≡ Just (SonorantFeature Plus)
    then Just ConstrictedGlottisFeature
    else Nothing
constrictedGlottis vowel@(Vowel _ _ _ CreakyVoiced) =
  if sonorant vowel ≡ Just (SonorantFeature Plus)
    then Just ConstrictedGlottisFeature
    else Nothing
constrictedGlottis _  = Nothing

{-|
Dentals are [+anterior].
Alveolars are [+anterior].
Post-alveolars are [-anterior].
Retroflexes are [-anterior].
Palatals are [-anterior].

(Source: page 265)

TODO: answer the question:
Question: Are Alveolo-palatals [+anterior], or [-anterior]?
Alveolo-palatals are [-anterior].
(SOURCE: not found)

|-}
anterior ∷ Phonet → Maybe PhonemeFeature
anterior (Consonant _ Dental            _ _) = Just (AnteriorFeature Plus)
anterior (Consonant _ Alveolar          _ _) = Just (AnteriorFeature Plus)
anterior (Consonant _ PostAlveolar      _ _) = Just (AnteriorFeature Minus)
anterior (Consonant _ Retroflex         _ _) = Just (AnteriorFeature Minus)
anterior (Consonant _ Palatal           _ _) = Just (AnteriorFeature Minus)
anterior (Consonant _ AlveoloPalatal    _ _) = Just (AnteriorFeature Minus)
anterior _                                   = Nothing

distributed ∷ Phonet → Maybe PhonemeFeature
distributed (Consonant _ Dental         _ _) = Just (DistributedFeature Plus)
distributed (Consonant _ Alveolar       _ _) = Just (DistributedFeature Minus)
distributed (Consonant _ PostAlveolar   _ _) = Just (DistributedFeature Plus)
distributed (Consonant _ Retroflex      _ _) = Just (DistributedFeature Minus)
distributed (Consonant _ Palatal        _ _) = Just (DistributedFeature Plus)
distributed (Consonant _ AlveoloPalatal _ _) = Just (DistributedFeature Plus)
distributed _                                = Nothing


{-|
Alveolar fricatives are [+strident].
Alveolar affricates are [+strident], also.
Post-alveolar fricatives are [+strident], also.
Post-alveolar affricates are [+strident], also.
Labio-dental fricatives are [+strident] , also.
Labio-dental affricates are [+strident] , also.
Uvular fricatives are [+strident], also.
Uvular affricates are [+strident], also.

All other fricatives are [-strident].
All other affricates are [-strident], also.

All other segments are undefined for [+/-strident].

(Source: page 266, under [+/-strident] heading, under the subsection
"Natural classes".)
|-}
strident ∷ Phonet → Maybe PhonemeFeature
strident (Consonant _ Alveolar     Fricative _) = Just (StridentFeature Plus)
strident (Consonant _ Alveolar     Affricate _) = Just (StridentFeature Plus)
strident (Consonant _ PostAlveolar Fricative _) = Just (StridentFeature Plus)
strident (Consonant _ PostAlveolar Affricate _) = Just (StridentFeature Plus)
strident (Consonant _ LabioDental  Fricative _) = Just (StridentFeature Plus)
strident (Consonant _ LabioDental  Affricate _) = Just (StridentFeature Plus)
strident (Consonant _ Uvular       Fricative _) = Just (StridentFeature Plus)
strident (Consonant _ Uvular       Affricate _) = Just (StridentFeature Plus)

strident (Consonant _ _            Fricative _) = Just (StridentFeature Minus)
strident (Consonant _ _            Affricate _) = Just (StridentFeature Minus)

strident _                                      = Nothing


{-|
Palatal consonants are [+high].
Alveolo-palatal consonants are [+high].
Velar consonants are [+high].

Uvular consonants are [-high].
All other consonants are undefined for [+/-high].
Close vowels are [+high].
Near-close vowels are [+high].
All other vowels are [-high].
|-}
high ∷ Phonet → Maybe PhonemeFeature
high (Consonant _ Palatal        _ _) = Just (HighFeature Plus)
high (Consonant _ AlveoloPalatal _ _) = Just (HighFeature Plus)
high (Consonant _ Velar          _ _) = Just (HighFeature Plus)
high (Consonant _ Uvular         _ _) = Just (HighFeature Minus)
high (Consonant _ _              _ _) = Nothing
high (Vowel Close              _ _ _) = Just (HighFeature Plus)
high (Vowel NearClose          _ _ _) = Just (HighFeature Plus)
high (Vowel _                  _ _ _) = Just (HighFeature Minus)


{-|
Uvular consonants are [+low].
Pharyngeal consonants are [+low].
Glottal consonants are [+low].
All other consonants are undefined for [+/-low].
Open vowels are [+low].
Near open vowels are [+low].
All other vowels are [-low].
|-}
low ∷ Phonet → Maybe PhonemeFeature
low (Consonant _ Uvular     _ _) = Just (LowFeature Plus)
low (Consonant _ Pharyngeal _ _) = Just (LowFeature Plus)
low (Consonant _ Glottal    _ _) = Just (LowFeature Plus)
low (Consonant _ _          _ _) = Nothing
low (Vowel Open _           _ _) = Just (LowFeature Plus)
low (Vowel NearOpen _       _ _) = Just (LowFeature Plus)
low (Vowel _ _              _ _) = Just (LowFeature Minus)


{-|
Back vowels are [+back].
Central vowels are [+back].
Front vowels are [-back].
All other segments are undefined for [+/-back].
|-}
back ∷ Phonet → Maybe PhonemeFeature
back (Vowel _ Back    _ _) = Just (BackFeature Plus)
back (Vowel _ Central _ _) = Just (BackFeature Plus)
back (Vowel _ Front   _ _) = Just (BackFeature Minus)
back _                     = Nothing


{-|
Rounded vowels are [+round].
All other vowels are [-round].
All other segments are [-round].
|-}
round ∷ Phonet → Maybe PhonemeFeature
round (Vowel _ _ Rounded _) = Just (RoundFeature Plus)
round (Vowel _ _ _       _) = Just (RoundFeature Minus)
round _                     = Just (RoundFeature Minus)

{-|
Advanced tongue root
|-}
atr ∷ Phonet → Maybe PhonemeFeature
atr (Vowel  Close     Front   Unrounded Voiced) = Just (AdvancedTongueRootFeature Plus)
atr (Vowel  CloseMid  Front   Unrounded Voiced) = Just (AdvancedTongueRootFeature Plus)
atr (Vowel  Close     Back    Rounded   Voiced) = Just (AdvancedTongueRootFeature Plus)
atr (Vowel  CloseMid  Front   Rounded   Voiced) = Just (AdvancedTongueRootFeature Plus)
atr (Vowel  CloseMid  Back    Rounded   Voiced) = Just (AdvancedTongueRootFeature Plus)
atr (Vowel  Close     Front   Rounded   Voiced) = Just (AdvancedTongueRootFeature Plus)
atr (Vowel  NearOpen  Front   Unrounded Voiced) = Just (AdvancedTongueRootFeature Minus)
atr (Vowel  Open      Back    Unrounded Voiced) = Just (AdvancedTongueRootFeature Minus)
atr (Vowel  Close     Central Unrounded Voiced) = Just (AdvancedTongueRootFeature Minus)
atr (Vowel  OpenMid   Back    Unrounded Voiced) = Just (AdvancedTongueRootFeature Minus)
atr (Vowel  NearClose Front   Unrounded Voiced) = Just (AdvancedTongueRootFeature Minus)
atr (Vowel  NearClose Back    Rounded   Voiced) = Just (AdvancedTongueRootFeature Minus)
atr (Vowel  OpenMid   Front   Unrounded Voiced) = Just (AdvancedTongueRootFeature Minus)
atr (Vowel  OpenMid   Back    Rounded   Voiced) = Just (AdvancedTongueRootFeature Minus)
atr _                                           = Nothing


{-|
Given a phoneme (representation)
Gives a feature matrix.

Note: to non-linguists, feature matrices
are 1-dimensional, always displayed
as a single column.

For example:
/p/

  |-}
featureMatrix ∷ Phonet → [Maybe PhonemeFeature]
featureMatrix phonete
  = [ consonantal          phonete
    , syllabic             phonete
    , continuant           phonete
    , sonorant             phonete
    , delayedRelease       phonete
    , anterior             phonete
    , distributed          phonete
    , strident             phonete
    , high                 phonete
    , low                  phonete
    , nasal                phonete
    , lateral              phonete
    , labial               phonete
    , coronal              phonete
    , dorsal               phonete
    , pharyngeal           phonete
    , laryngeal            phonete
    , back                 phonete
    , round                phonete
    , voice                phonete
    , atr                  phonete
    , spreadGlottis        phonete
    , constrictedGlottis   phonete
    ]


-- | A function that takes data representing
-- how a phoneme is pronounced, and returns
-- a list of phonemic features.
analyzeFeatures ∷ Phonet → [PhonemeFeature]
analyzeFeatures phonete =
  catMaybes (featureMatrix phonete)

showFeatures ∷ [PhonemeFeature] → String
showFeatures features =
  let featuresStrings = map show features
  in "[" ⧺ intercalate "; " featuresStrings ⧺ "]"

toTextFeatures ∷ Phonet → String
toTextFeatures phonete =
  let features = analyzeFeatures phonete
  in showFeatures features
