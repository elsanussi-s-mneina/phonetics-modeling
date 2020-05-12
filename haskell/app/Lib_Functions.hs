{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib_Functions where

import Lib_Types

import Prelude ()
import Relude
  ( Bool(False, True), Natural, Maybe(Just, Nothing) , NonEmpty((:|)), Text
  , catMaybes , one , sconcat
  , filter                 , fmap      , fromMaybe, fromList, map  , maybe
  , otherwise              , toList, unwords
  , (+), (<), (!!?)
  )

import qualified Data.Text as T


import Prelude.Unicode ( (≡) , (∧) , (∨) )
import Data.Monoid.Unicode ( (⊕) )
import MyLocal_Data_Semigroup_Unicode ((◇))
import Data.Foldable.Unicode ((∈) , (∉))
import EnglishUSText

retractedPlace ∷ Place → Place
retractedPlace place =
  case place of
    Bilabial     → LabioDental
    LabioDental  → Dental
    Dental       → Alveolar
    Alveolar     → PostAlveolar
    PostAlveolar → Retroflex
    Retroflex    → Palatal
    Palatal      → Velar
    Velar        → Uvular
    Uvular       → Pharyngeal
    Pharyngeal   → Glottal
    Glottal      → Epiglottal
    same         → same




englishDescription ∷ Phonet → Text
englishDescription = showPhonet


-- | A function that given an IPA symbol will convert it to the voiced
-- |equivalent.
voicedPhonet ∷ Phonet → Phonet
voicedPhonet p = case p of
  (Consonant   VoicelessAspirated x y z) → Consonant   VoicedAspirated x y z
  (Consonant   Voiceless          x y z) → Consonant   Voiced x y z
  (Consonant   Voiced             x y z) → Consonant   Voiced x y z
  (Consonant   VoicedAspirated    x y z) → Consonant   VoicedAspirated x y z
  (Consonant   _                  x y z) → Consonant   Voiced x y z
  (Vowel x y z _                       ) → Vowel x y z Voiced

-- | A function that given an IPA symbol will convert it to the voiceless
-- | equivalent.
devoicedPhonet ∷ Phonet → Phonet
devoicedPhonet p = case p of
  (Consonant   Voiced             x y z) → Consonant   Voiceless          x y z
  (Consonant   CreakyVoiced       x y z) → Consonant   Voiceless          x y z
  (Consonant   Voiceless          x y z) → Consonant   Voiceless          x y z
  (Consonant   VoicedAspirated    x y z) → Consonant   VoicelessAspirated x y z
  (Consonant   VoicelessAspirated x y z) → Consonant   VoicelessAspirated x y z
  (Vowel x y z _                       ) → Vowel x y z Voiceless



spirantizedPhonet ∷ Phonet → Phonet

-- The following is inelegant, but there is no other way in the system,
-- right now. The part that is inelegant is that,
-- a [t] which is considered alveolar, when spirantized becomes [θ]
-- which is dental.
-- So the following line implements this
-- change in place of articulation.
spirantizedPhonet p = case p of
  (Consonant x Alveolar Plosive z) → Consonant x Dental Fricative z
  (Consonant x place₁ Plosive z)   → Consonant x place₁ Fricative z
  other                            → other


unmarkDifferences ∷ Phonet → Phonet → UnmarkablePhonet
unmarkDifferences p₁ p₂ = case (p₁, p₂) of
  ( Consonant voice₁ place₁ manner₁ airstream₁
    , Consonant voice₂ place₂ manner₂ airstream₂) →
    let voice'     = unmarkVoice voice₁ voice₂
        place'     = unmarkPlace place₁ place₂
        manner'    = unmarkManner manner₁ manner₂
        airstream' = unmarkAirstream airstream₁ airstream₂
    in UnmarkableConsonant voice' place' manner' airstream'

  ( Vowel height₁ backness₁ rounding₁ voice₁
    , Vowel height₂ backness₂ rounding₂ voice₂) →
    let voice'    = unmarkVoice voice₁ voice₂
        height'   = unmarkHeight height₁ height₂
        backness' = unmarkBackness backness₁ backness₂
        rounding' = unmarkRounding rounding₁ rounding₂
    in UnmarkableVowel height' backness' rounding' voice'

  ( Vowel _ _ _ voice₁
    , Consonant voice₂ _ _ _) →
    let voice' = unmarkVoice voice₁ voice₂
    in UnmarkableVowel UnmarkedHeight UnmarkedBackness UnmarkedRounding voice'

  ( Consonant {}
    , Vowel {}) →
    unmarkDifferences p₂ p₁ -- Change the order of arguments
  where
    unmarkVoice voice₁ voice₂ =
      if voice₁ ≡ voice₂
        then MarkedVocalFolds voice₁
        else UnmarkedVocalFolds

    unmarkPlace place₁ place₂ =
      if place₁ ≡ place₂
        then MarkedPlace place₁
        else UnmarkedPlace

    unmarkManner manner₁ manner₂ =
      if manner₁ ≡ manner₂
        then MarkedManner manner₁
        else UnmarkedManner

    unmarkAirstream airstream₁ airstream₂ =
      if airstream₁ ≡ airstream₂
        then MarkedAirstream  airstream₁
        else UnmarkedAirstream

    unmarkHeight height₁ height₂ =
      if height₁   ≡ height₂
        then MarkedHeight     height₁
        else UnmarkedHeight

    unmarkBackness backness₁ backness₂ =
      if backness₁ ≡ backness₂
        then MarkedBackness   backness₁
        else UnmarkedBackness

    unmarkRounding rounding₁ rounding₂ =
      if rounding₁ ≡ rounding₂
        then MarkedRounding   rounding₁
        else UnmarkedRounding



-- This function
-- takes any unmarked attributes in the phoneme definition,
-- and returns a list with all possible phonemes that have that attribute.
similarPhonemesTo ∷ UnmarkablePhonet → [Phonet]
similarPhonemesTo (UnmarkableConsonant voice₁ place₁ manner₁ airstream₁) =
  let voice'     = toList (similarInVoice     voice₁    )
      place'     = toList (similarInPlace     place₁    )
      manner'    = toList (similarInManner    manner₁   )
      airstream' = toList (similarInAirstream airstream₁)
  in [Consonant v p m a | p ← place', v ← voice',  m ← manner', a ← airstream']

similarPhonemesTo (UnmarkableVowel height₁ backness₁ rounding₁ voice₁) =
  let voice'    = toList (similarInVoice    voice₁   )
      height'   = toList (similarInHeight   height₁  )
      backness' = toList (similarInBackness backness₁)
      rounding' = toList (similarInRounding rounding₁)
  in [Vowel h b r v | h ← height', b ← backness', r ← rounding', v ← voice']


similarInVoice ∷ UnmarkableVocalFolds → NonEmpty VocalFolds
similarInVoice voice₁ =
  case voice₁ of
       MarkedVocalFolds x → one x
       UnmarkedVocalFolds → vocalFoldStates

similarInPlace ∷ UnmarkablePlace → NonEmpty Place
similarInPlace place₁ =
  case place₁ of
       MarkedPlace x → one x
       UnmarkedPlace → placeStates


similarInManner ∷ UnmarkableManner → NonEmpty Manner
similarInManner manner₁ =
  case manner₁ of
       MarkedManner x → one x
       UnmarkedManner → mannerStates

similarInAirstream ∷ UnmarkableAirstream → NonEmpty Airstream
similarInAirstream airstream₁ =
  case airstream₁ of
       MarkedAirstream x → one x
       UnmarkedAirstream → airstreamStates


similarInHeight ∷ UnmarkableHeight → NonEmpty Height
similarInHeight height₁ =
  case height₁ of
       MarkedHeight x → one x
       UnmarkedHeight → heightStates

similarInBackness ∷ UnmarkableBackness → NonEmpty Backness
similarInBackness backness₁ =
  case backness₁ of
       MarkedBackness x → one x
       UnmarkedBackness → backnessStates

similarInRounding ∷ UnmarkableRounding → NonEmpty Rounding
similarInRounding rounding₁ =
  case rounding₁ of
       MarkedRounding x → one x
       UnmarkedRounding → roundingStates

-- The following function returns whether an articulation is
-- considered impossible according to the IPA (pulmonic) consonants chart.
-- Does not work for other values.
impossible ∷ Phonet → Bool
impossible p = case p of
  (Consonant Voiced          Pharyngeal  Plosive            PulmonicEgressive)
    → True
  (Consonant VoicedAspirated Pharyngeal  Plosive            PulmonicEgressive)
    → True
  (Consonant Voiceless       Glottal     Plosive            PulmonicEgressive)
    → False  -- [ʔ] is not impossible.
  (Consonant _               Glottal     Fricative          PulmonicEgressive)
    → False  -- [h] and [ɦ] are not impossible.
  (Consonant _               Glottal     _                  PulmonicEgressive)
    → True   -- all other pulmonary egressive consonants are impossible..
  (Consonant _               Pharyngeal  Nasal              PulmonicEgressive)
    → True
  (Consonant _               Pharyngeal  LateralFricative   PulmonicEgressive)
    → True
  (Consonant _               Pharyngeal  LateralApproximant PulmonicEgressive)
    → True
  (Consonant _               Velar       Trill              PulmonicEgressive)
    → True
  (Consonant _               Velar       TapOrFlap          PulmonicEgressive)
    → True
  (Consonant _               Bilabial    LateralFricative   PulmonicEgressive)
    → True
  (Consonant _               Bilabial    LateralApproximant PulmonicEgressive)
    → True
  (Consonant _               LabioDental LateralFricative   PulmonicEgressive)
    → True
  (Consonant _               LabioDental LateralApproximant PulmonicEgressive)
    → True
  _
    → False -- Everything else is assumed to be possible.




-- | This is a list of the sounds of English. Just the basic ones.
-- | It is somewhat more complicated in reality, but for now this will
-- | suffice.
-- | This following sound inventory of English is from page 20 of
-- | (2013, Elizabeth C. Zsiga, The Sounds of Language)
englishPhonetInventory ∷ PhonetInventory
englishPhonetInventory = PhonetInventory (fromList
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
  Vowel  Close Back    Rounded   Voiced, -- "u"

  -- Near-close Vowels:
  Vowel NearClose Front Unrounded Voiced, -- "ɪ"
  Vowel NearClose Back  Rounded   Voiced, --  "ʊ"

  -- Close-mid Vowels:
  Vowel  CloseMid Front   Unrounded Voiced, -- "e"
  Vowel  CloseMid Back    Rounded   Voiced, -- "o"

  -- Mid Vowels:
  Vowel Mid Central Unrounded Voiced, -- "ə"


  -- Open-mid Vowels:
  Vowel  OpenMid Front   Unrounded Voiced, -- "ɛ"
  Vowel  OpenMid Central Unrounded Voiced, -- "ɜ"
  Vowel  OpenMid Back    Unrounded Voiced, --  "ʌ"
  Vowel  OpenMid Back    Rounded   Voiced, -- "ɔ"

  -- Near-open
  Vowel  NearOpen Front   Unrounded Voiced, -- "æ"
  Vowel  NearOpen Central Unrounded  Voiced, -- "ɐ"

  -- Open Vowels:
  Vowel  Open Back  Unrounded Voiced, -- "ɑ"
  Vowel  Open Back  Rounded   Voiced -- "ɒ"
  ]
  )
-- I added some English vowels. I did not choose any specific dialect.
-- I got all my information from the Wikipedia page titled
-- "English Phonology"
-- at the following URL: https://en.wikipedia.org/wiki/English_phonology#Vowels
-- on Monday, February 24, 2020.
-- To do: Get better information on English vowels from a more reliable source.
-- To do: model separate dialects of English or only one.




exponentials ∷ NonEmpty Text
exponentials = fromList ["ʰ" , "ʷ" , "ʲ" , "ˠ" , "ˤ" , "ⁿ" , "ˡ"]

{-|
Whether an IPA character is written above the base line
and to the right of the previous character,
like how exponents of a power are written
in mathematical notation.
|-}
isExponential ∷ Text → Bool
isExponential character = character ∈ exponentials
{-|
Whether a diacritic goes above
the character it is placed on.
|-}
isDiacriticAbove ∷ Text → Bool
isDiacriticAbove "̊" = True
isDiacriticAbove  _  = False

{-|
Whether a diacritic goes below
the character which it is placed on.
|-}
isDiacriticBelow ∷ Text → Bool
isDiacriticBelow "̥" = True
isDiacriticBelow  _  = False

{-|
When given a diacritic that goes above,
replaces it with one that goes below,
and has the same meaning.
otherwise does nothing.
  |-}
lowerDiacritic ∷ Text → Text
lowerDiacritic "̊" = "̥"
lowerDiacritic x  = x


{-|
When given a diacritic that goes below,
replaces it with one that goes below, and
has the same meaning;
otherwise it does nothing.
  |-}
raiseDiacritic ∷ Text → Text
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
ascenders ∷ NonEmpty Text
ascenders = fromList
  ["b", "t", "d", "k", "ʔ", "f", "θ", "ð", "ħ", "ʕ", "h", "ɦ", "ɬ", "l", "ʎ",
  "ʘ", "ɓ", "ǀ", "ɗ", "ǃ", "ǂ", "ɠ", "ʄ", "ǁ", "ʛ", "ɺ", "ʢ", "ʡ", "ɤ", "ʈ", "ɖ",
  "ɸ", "β", "ʃ", "ɮ", "ɭ", "ɧ"]


isAscender ∷ Text → Bool
isAscender character = character ∈ ascenders

descenders ∷ NonEmpty Text
descenders = fromList
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
isDescender ∷ Text → Bool
isDescender character = character ∈ descenders


{-|
Prevent placement of diacritics below a full-width
character,
when doing so would likely make the result
difficult to read, whenever there is another
diacritic with the same meaning, but can go above.
And vice-versa (above - below).

Only support the voiceless diacritic so far.
  |-}
preventProhibitedCombination ∷ Text → Text
preventProhibitedCombination ss
  | T.null ss = ""
  | T.length ss ≡ 1 = ss
  | otherwise =
     let firstCharacter = one (T.head ss) ∷ Text
         secondCharacter = one (T.index ss 1) ∷ Text
         rest = T.tail (T.tail ss)
     in
      if isAscender firstCharacter ∧ isDiacriticAbove secondCharacter
      then firstCharacter ⊕ lowerDiacritic secondCharacter ⊕ rest
      else if isDescender firstCharacter ∧ isDiacriticBelow secondCharacter
      then firstCharacter ⊕ raiseDiacritic secondCharacter ⊕ rest
      else ss






graphemesOfIPA ∷ NonEmpty Text
graphemesOfIPA = consonantsPulmonic
  ◇ consonantsNonPulmonic
  ◇ otherSymbols
  ◇ vowels
  ◇ suprasegmentals
  ◇ toneAndWordAccents
  ◇ diacriticsAndSuprasegmentals
-- See:
--www.internationalphoneticassociation.org/sites/default/files/IPA_Kiel_2015.pdf
-- For the source of this information..

-- CONSONANTS (PULMONIC)
consonantsPulmonic ∷ NonEmpty Text
consonantsPulmonic
   = plosivePulmonic
   ◇ nasalPulmonic
   ◇ trillPulmonic
   ◇ tapOrFlapPulmonic
   ◇ fricativePulmonic
   ◇ lateralFricativePulmonic
   ◇ approximantPulmonic
   ◇ lateralApproximantPulmonic


plosivePulmonic ∷ NonEmpty Text
plosivePulmonic            = fromList
                             [ "p", "b",                     "t", "d"
                             , "ʈ", "ɖ", "c", "ɟ", "k", "g", "q", "ɢ"
                             , "ʔ"
                             ]

nasalPulmonic ∷ NonEmpty Text
nasalPulmonic              = fromList ["m", "ɱ", "n", "ɳ", "ɲ", "ŋ", "ɴ"]

trillPulmonic ∷ NonEmpty Text
trillPulmonic              = fromList [ "ʙ", "r", "ʀ"]

tapOrFlapPulmonic ∷ NonEmpty Text
tapOrFlapPulmonic          = fromList [ "ⱱ", "ɾ", "ɽ"]

fricativePulmonic ∷ NonEmpty Text
fricativePulmonic
  = fromList
  [ "ɸ", "β", "f", "v", "θ", "ð", "s", "z", "ʃ", "ʒ"
  , "ʂ", "ʐ", "ç", "ʝ", "x", "ɣ", "χ", "ʁ", "ħ", "ʕ"
  , "h", "ɦ"
  ]

lateralFricativePulmonic ∷ NonEmpty Text
lateralFricativePulmonic   = fromList [ "ɬ", "ɮ" ]

approximantPulmonic ∷ NonEmpty Text
approximantPulmonic        = fromList [ "ʋ", "ɻ", "j", "ɰ" ]

lateralApproximantPulmonic ∷ NonEmpty Text
lateralApproximantPulmonic = fromList [ "l", "ɭ", "ʎ", "ʟ" ]




consonantsNonPulmonic ∷ NonEmpty Text
consonantsNonPulmonic = fromList
-- Clicks   Voiced implosives
 [ "ʘ",     "ɓ" -- Bilabial
 , "ǀ", {- Dental -}    "ɗ" -- Dental/alveolar
 , "ǃ", {-  (Post)alveolar -}  "ʄ"
 , "ǂ",  "ɠ"
 , "ǁ",  "ʛ"
 ]

otherSymbols ∷ NonEmpty Text
otherSymbols = fromList
  ["ʍ",  "ɕ"
  ,"w",  "ʑ"
  ,"ɥ",  "ɺ"
  ,"ʜ",  "ɧ"
  ,"ʢ"
  ,"ʡ"
  ]

vowels ∷ NonEmpty Text
vowels = fromList
  ["i", "y",   "ɨ", "ʉ",   "ɯ", "u"   -- Close
  ,"ɪ", "ʏ",            "ʊ"
  ,"e", "ø",   "ɘ", "ɵ",   "ɤ", "o"   -- Close-mid
  ,               "ə"
  ,"ɛ", "œ",   "ɜ", "ɞ",   "ʌ", "ɔ"   -- Open-mid
  , "æ",           "ɐ"
  , "a", "ɶ",              "ɑ", "ɒ"  -- Open
  ]

suprasegmentals ∷ NonEmpty Text
suprasegmentals = fromList
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


toneAndWordAccents ∷ NonEmpty Text
toneAndWordAccents = fromList
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

diacriticsAndSuprasegmentals ∷ NonEmpty Text
diacriticsAndSuprasegmentals = fromList
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

showIPA ∷ PhonetInventory → Text
showIPA (PhonetInventory phonetes) = sconcat (fmap constructIPA phonetes)



-- | This function will allow us to convert an IPA symbol
-- | to its analyzed form (its phonetic features)
analyzeIPA  ∷ Text → Maybe Phonet


-- Plosives:
analyzeIPA p = case p of
  "p"  → Just (Consonant  Voiceless Bilabial  Plosive PulmonicEgressive)
  "b"  → Just (Consonant  Voiced    Bilabial  Plosive PulmonicEgressive)
  "t"  → Just (Consonant  Voiceless Alveolar  Plosive PulmonicEgressive)
  "d"  → Just (Consonant  Voiced    Alveolar  Plosive PulmonicEgressive)
  "ʈ"  → Just (Consonant  Voiceless Retroflex Plosive PulmonicEgressive)
  "ɖ"  → Just (Consonant  Voiced    Retroflex Plosive PulmonicEgressive)
  "c"  → Just (Consonant  Voiceless Palatal   Plosive PulmonicEgressive)
  "ɟ"  → Just (Consonant  Voiced    Palatal   Plosive PulmonicEgressive)
  "k"  → Just (Consonant  Voiceless Velar     Plosive PulmonicEgressive)
  "g"  → Just (Consonant  Voiced    Velar     Plosive PulmonicEgressive)
  "q"  → Just (Consonant  Voiceless Uvular    Plosive PulmonicEgressive)
  "ɢ"  → Just (Consonant  Voiced    Uvular    Plosive PulmonicEgressive)
  "ʔ"  → Just (Consonant  Voiceless Glottal   Plosive PulmonicEgressive)

-- Nasals:
  "m"  → Just (Consonant  Voiced Bilabial    Nasal PulmonicEgressive)
  "ɱ"  → Just (Consonant  Voiced LabioDental Nasal PulmonicEgressive)
  "n"  → Just (Consonant  Voiced Alveolar    Nasal PulmonicEgressive)
  "ɳ"  → Just (Consonant  Voiced Retroflex   Nasal PulmonicEgressive)
  "ɲ"  → Just (Consonant  Voiced Palatal     Nasal PulmonicEgressive)
  "ŋ"  → Just (Consonant  Voiced Velar       Nasal PulmonicEgressive)
  "ɴ"  → Just (Consonant  Voiced Uvular      Nasal PulmonicEgressive)

-- Trills:
  "ʙ"  → Just (Consonant  Voiced Bilabial Trill PulmonicEgressive)
  "r"  → Just (Consonant  Voiced Alveolar Trill PulmonicEgressive)
  "ʀ"  → Just (Consonant  Voiced Uvular   Trill PulmonicEgressive)

-- Taps or flaps:
  "ⱱ"  → Just (Consonant  Voiced LabioDental TapOrFlap PulmonicEgressive)
  "ɾ"  → Just (Consonant  Voiced Alveolar    TapOrFlap PulmonicEgressive)
  "ɽ"  → Just (Consonant  Voiced Retroflex   TapOrFlap PulmonicEgressive)

-- Fricatives:
  "ɸ"  → Just (Consonant  Voiceless Bilabial     Fricative PulmonicEgressive)
  "β"  → Just (Consonant  Voiced    Bilabial     Fricative PulmonicEgressive)
  "f"  → Just (Consonant  Voiceless LabioDental  Fricative PulmonicEgressive)
  "v"  → Just (Consonant  Voiced    LabioDental  Fricative PulmonicEgressive)
  "θ"  → Just (Consonant  Voiceless Dental       Fricative PulmonicEgressive)
  "ð"  → Just (Consonant  Voiced    Dental       Fricative PulmonicEgressive)
  "s"  → Just (Consonant  Voiceless Alveolar     Fricative PulmonicEgressive)
  "z"  → Just (Consonant  Voiced    Alveolar     Fricative PulmonicEgressive)
  "ʃ"  → Just (Consonant  Voiceless PostAlveolar Fricative PulmonicEgressive)
  "ʒ"  → Just (Consonant  Voiced    PostAlveolar Fricative PulmonicEgressive)
  "ʂ"  → Just (Consonant  Voiceless Retroflex    Fricative PulmonicEgressive)
  "ʐ"  → Just (Consonant  Voiced    Retroflex    Fricative PulmonicEgressive)
  "ç"  → Just (Consonant  Voiceless Palatal      Fricative PulmonicEgressive)
  "ʝ"  → Just (Consonant  Voiced    Palatal      Fricative PulmonicEgressive)
  "x"  → Just (Consonant  Voiceless Velar        Fricative PulmonicEgressive)
  "ɣ"  → Just (Consonant  Voiced    Velar        Fricative PulmonicEgressive)
  "χ"  → Just (Consonant  Voiceless Uvular       Fricative PulmonicEgressive)
  "ʁ"  → Just (Consonant  Voiced    Uvular       Fricative PulmonicEgressive)
  "ħ"  → Just (Consonant  Voiceless Pharyngeal   Fricative PulmonicEgressive)
  "ʕ"  → Just (Consonant  Voiced    Pharyngeal   Fricative PulmonicEgressive)
  "h"  → Just (Consonant  Voiceless Glottal      Fricative PulmonicEgressive)
  "ɦ"  → Just (Consonant  Voiced    Glottal      Fricative PulmonicEgressive)


-- Lateral Fricatives:
  "ɬ" → Just (Consonant  Voiceless Alveolar LateralFricative PulmonicEgressive)
  "ɮ" → Just (Consonant  Voiced    Alveolar LateralFricative PulmonicEgressive)


-- Approximants:
  "ʋ"  → Just (Consonant  Voiced LabioDental  Approximant PulmonicEgressive)
  "ɹ"  → Just (Consonant  Voiced Alveolar     Approximant PulmonicEgressive)
  "ɻ"  → Just (Consonant  Voiced Retroflex    Approximant PulmonicEgressive)
  "j"  → Just (Consonant  Voiced Palatal      Approximant PulmonicEgressive)
  "ɰ"  → Just (Consonant  Voiced Velar        Approximant PulmonicEgressive)

-- Lateral Approximants:
  "l"  → Just (Consonant  Voiced Alveolar  LateralApproximant PulmonicEgressive)
  "ɭ"  → Just (Consonant  Voiced Retroflex LateralApproximant PulmonicEgressive)
  "ʎ"  → Just (Consonant  Voiced Palatal   LateralApproximant PulmonicEgressive)
  "ʟ"  → Just (Consonant  Voiced Velar     LateralApproximant PulmonicEgressive)



-- Affricates
  "t͡ʃ" → Just (Consonant  Voiceless PostAlveolar Affricate PulmonicEgressive)
  "d͡ʒ" → Just (Consonant  Voiced    PostAlveolar Affricate PulmonicEgressive)
-- We should probably enforce use of the tie-bar underneath, otherwise
-- it would not be deterministic to determine whether two graphemes here
-- represent affricates or a plosive followed by a fricative.




-- Under the Other Symbols part of the IPA chart:

  "w" → Just (Consonant Voiced    LabialVelar    Approximant PulmonicEgressive)
  "ʍ" → Just (Consonant Voiceless LabialVelar    Fricative   PulmonicEgressive)
  "ɥ" → Just (Consonant Voiced    LabialPalatal  Approximant PulmonicEgressive)
  "ʜ" → Just (Consonant Voiceless Epiglottal     Fricative   PulmonicEgressive)
  "ʢ" → Just (Consonant Voiced    Epiglottal     Fricative   PulmonicEgressive)
  "ʡ" → Just (Consonant Voiceless Epiglottal     Plosive     PulmonicEgressive)
  -- Is the epiglottal plosive voiceless? The IPA chart does not specify.
  "ɕ" → Just (Consonant Voiceless AlveoloPalatal Fricative   PulmonicEgressive)
  "ʑ" → Just (Consonant Voiced    AlveoloPalatal Fricative   PulmonicEgressive)
  "ɺ" → Just (Consonant Voiced    Alveolar       LateralFlap PulmonicEgressive)
  "ɧ" → Just (Consonant Voiceless (Places (PostAlveolar :| [Velar]))
              Fricative PulmonicEgressive)

-- Other Consonants:
  "ʘ" → Just (Consonant Voiceless Bilabial       Plosive  Click    )
  "ǀ" → Just (Consonant Voiceless  Dental         Plosive  Click    )
  "ǃ" → Just (Consonant Voiceless  Alveolar       Plosive  Click    )
  --  "ǃ" could also be PostAlveolar.
  "ǂ" → Just (Consonant Voiceless PalatoAlveolar Plosive  Click    )
  "ǁ" → Just (Consonant Voiceless  Alveolar       Lateral  Click    )
  "ɓ" → Just (Consonant Voiced     Bilabial      Plosive  Implosive)
  "ɗ" → Just (Consonant Voiced     Dental        Plosive  Implosive)
  -- "ɗ" could also be Alveolar
  "ʄ" → Just (Consonant Voiced     Palatal       Plosive  Implosive)
  "ɠ" → Just (Consonant Voiced     Velar         Plosive  Implosive)
  "ʛ" → Just (Consonant Voiced     Uvular        Plosive  Implosive)

-- Close Vowels:
  "i"  → Just (Vowel  Close Front   Unrounded Voiced)
  "y"  → Just (Vowel  Close Front   Rounded   Voiced)
  "ɨ"  → Just (Vowel  Close Central Unrounded Voiced)
  "ʉ"  → Just (Vowel  Close Central Rounded   Voiced)
  "ɯ"  → Just (Vowel  Close Back    Unrounded Voiced)
  "u"  → Just (Vowel  Close Back    Rounded   Voiced)

-- Near-close Vowels:
  "ɪ"  → Just (Vowel NearClose Front Unrounded Voiced)
  "ʏ"  → Just (Vowel NearClose Front Rounded   Voiced)
  "ʊ"  → Just (Vowel NearClose Back  Rounded   Voiced)

-- Close-mid Vowels:
  "e"  → Just (Vowel  CloseMid Front   Unrounded Voiced)
  "ø"  → Just (Vowel  CloseMid Front   Rounded   Voiced)
  "ɘ"  → Just (Vowel  CloseMid Central Unrounded Voiced)
  "ɵ"  → Just (Vowel  CloseMid Central Rounded   Voiced)
  "ɤ"  → Just (Vowel  CloseMid Back    Unrounded Voiced)
  "o"  → Just (Vowel  CloseMid Back    Rounded   Voiced)

-- Mid Vowels:
  "ə"  → Just (Vowel Mid Central Unrounded Voiced)


-- Open-mid Vowels:
  "ɛ"  → Just (Vowel  OpenMid Front   Unrounded Voiced)
  "œ"  → Just (Vowel  OpenMid Front   Rounded   Voiced)
  "ɜ"  → Just (Vowel  OpenMid Central Unrounded Voiced)
  "ɞ"  → Just (Vowel  OpenMid Central Rounded   Voiced)
  "ʌ"  → Just (Vowel  OpenMid Back    Unrounded Voiced)
  "ɔ"  → Just (Vowel  OpenMid Back    Rounded   Voiced)

-- Near-open
  "æ"  → Just (Vowel  NearOpen Front   Unrounded  Voiced)
  "ɐ"  → Just (Vowel  NearOpen Central Unrounded  Voiced)

-- Open Vowels:
  "a"  → Just (Vowel  Open Front Unrounded Voiced)
  "ɶ"  → Just (Vowel  Open Front Rounded   Voiced)
  "ɑ"  → Just (Vowel  Open Back  Unrounded Voiced)
  "ɒ"  → Just (Vowel  Open Back  Rounded   Voiced)

-- Handle Diacritics:
  ipaText →
    case [T.last ipaText] of
      "̥" →
        let fullGrapheme = analyzeIPA (T.init ipaText)
        in case fullGrapheme of
                Just (Consonant _ place manner airstream)
                  → Just (Consonant Voiceless place manner airstream)
                Just (Vowel height backness rounding _  )
                  → Just (Vowel height backness rounding Voiceless)
                _
                  → Nothing
      "̬" →
        let fullGrapheme = analyzeIPA (T.init ipaText)
        in case fullGrapheme of
                Just (Consonant _ place manner airstream)
                  → Just (Consonant Voiced place manner airstream)
                Just (Vowel height backness rounding _  )
                  → Just (Vowel height backness rounding Voiced)
                _
                  → Nothing

      "ʰ" →
        let fullGrapheme = analyzeIPA (T.init ipaText)
        in case fullGrapheme of
                Just (Consonant Voiced place manner airstream   )
                  → Just (Consonant VoicedAspirated    place manner airstream)
                Just (Consonant Voiceless place manner airstream)
                  → Just (Consonant VoicelessAspirated place manner airstream)
                Just (Vowel height backness rounding voicing    )
                  → Just (Vowel height backness rounding voicing             )
                anythingElse
                  → anythingElse
                -- (About the preceding line:) It is strange but we will just
                -- do nothing if they give us an aspirated vowel.
                -- since we have no way to represent it in the type system.
                -- to do: determine
                -- if the idea of an aspirated vowel makes sense
      _ → Nothing -- not recognized.


constructIPA ∷ Phonet → Text
constructIPA phoneme =
  fromMaybe "∅" (constructIPARecursive 3 0 phoneme)

constructIPARecursive ∷ Natural → Natural → Phonet → Maybe Text
-- Plosives:
constructIPARecursive recursionLimit recursionLevel p = case p of
  _ | recursionLevel ≡ recursionLimit → Nothing
  (Consonant  Voiceless  Bilabial       Plosive            PulmonicEgressive)
    → Just "p"
  (Consonant  Voiced     Bilabial       Plosive            PulmonicEgressive)
    → Just "b"
  (Consonant  Voiceless  Alveolar       Plosive            PulmonicEgressive)
    → Just "t"
  (Consonant  Voiced     Alveolar       Plosive            PulmonicEgressive)
    → Just "d"
  (Consonant  Voiceless  Retroflex      Plosive            PulmonicEgressive)
    → Just "ʈ"
  (Consonant  Voiced     Retroflex      Plosive            PulmonicEgressive)
    → Just "ɖ"
  (Consonant  Voiceless  Palatal        Plosive            PulmonicEgressive)
    → Just "c"
  (Consonant  Voiced     Palatal        Plosive            PulmonicEgressive)
    → Just "ɟ"
  (Consonant  Voiceless  Velar          Plosive            PulmonicEgressive)
    → Just "k"
  (Consonant  Voiced     Velar          Plosive            PulmonicEgressive)
    → Just "g"
  (Consonant  Voiceless  Uvular         Plosive            PulmonicEgressive)
    → Just "q"
  (Consonant  Voiced     Uvular         Plosive            PulmonicEgressive)
    → Just "ɢ"
  (Consonant  Voiceless  Glottal        Plosive            PulmonicEgressive)
    → Just "ʔ"  -- Nasals (next line):
  (Consonant  Voiced     Bilabial       Nasal              PulmonicEgressive)
    → Just "m"
  (Consonant  Voiced     LabioDental    Nasal              PulmonicEgressive)
    → Just "ɱ"
  (Consonant  Voiced     Alveolar       Nasal              PulmonicEgressive)
    → Just "n"
  (Consonant  Voiced     Retroflex      Nasal              PulmonicEgressive)
    → Just "ɳ"
  (Consonant  Voiced     Palatal        Nasal              PulmonicEgressive)
    → Just "ɲ"
  (Consonant  Voiced     Velar          Nasal              PulmonicEgressive)
    → Just "ŋ"
  (Consonant  Voiced     Uvular         Nasal              PulmonicEgressive)
    → Just "ɴ"  -- Trills (next line):
  (Consonant  Voiced     Bilabial       Trill              PulmonicEgressive)
    → Just "ʙ"
  (Consonant  Voiced     Alveolar       Trill              PulmonicEgressive)
    → Just "r"
  (Consonant  Voiced     Uvular         Trill              PulmonicEgressive)
    → Just "ʀ"  -- Taps or flaps (next line):
  (Consonant  Voiced     LabioDental    TapOrFlap          PulmonicEgressive)
    → Just "ⱱ"
  (Consonant  Voiced     Alveolar       TapOrFlap          PulmonicEgressive)
    → Just "ɾ"
  (Consonant  Voiced     Retroflex      TapOrFlap          PulmonicEgressive)
    → Just "ɽ"  -- Fricatives (next line):
  (Consonant  Voiceless  Bilabial       Fricative          PulmonicEgressive)
    → Just "ɸ"
  (Consonant  Voiced     Bilabial       Fricative          PulmonicEgressive)
    → Just "β"
  (Consonant  Voiceless  LabioDental    Fricative          PulmonicEgressive)
    → Just "f"
  (Consonant  Voiced     LabioDental    Fricative          PulmonicEgressive)
    → Just "v"
  (Consonant  Voiceless  Dental         Fricative          PulmonicEgressive)
    → Just "θ"
  (Consonant  Voiced     Dental         Fricative          PulmonicEgressive)
    → Just "ð"
  (Consonant  Voiceless  Alveolar       Fricative          PulmonicEgressive)
    → Just "s"
  (Consonant  Voiced     Alveolar       Fricative          PulmonicEgressive)
    → Just "z"
  (Consonant  Voiceless  PostAlveolar   Fricative          PulmonicEgressive)
    → Just "ʃ"
  (Consonant  Voiced     PostAlveolar   Fricative          PulmonicEgressive)
    → Just "ʒ"
  (Consonant  Voiceless  Retroflex      Fricative          PulmonicEgressive)
    → Just "ʂ"
  (Consonant  Voiced     Retroflex      Fricative          PulmonicEgressive)
    → Just "ʐ"
  (Consonant  Voiceless  Palatal        Fricative          PulmonicEgressive)
    → Just "ç"
  (Consonant  Voiced     Palatal        Fricative          PulmonicEgressive)
    → Just "ʝ"
  (Consonant  Voiceless  Velar          Fricative          PulmonicEgressive)
    → Just "x"
  (Consonant  Voiced     Velar          Fricative          PulmonicEgressive)
    → Just "ɣ"
  (Consonant  Voiceless  Uvular         Fricative          PulmonicEgressive)
    → Just "χ"
  (Consonant  Voiced     Uvular         Fricative          PulmonicEgressive)
    → Just "ʁ"
  (Consonant  Voiceless  Pharyngeal     Fricative          PulmonicEgressive)
    → Just "ħ"
  (Consonant  Voiced     Pharyngeal     Fricative          PulmonicEgressive)
    → Just "ʕ"
  (Consonant  Voiceless  Glottal        Fricative          PulmonicEgressive)
    → Just "h"
  (Consonant  Voiced     Glottal        Fricative          PulmonicEgressive)
    → Just "ɦ"  -- Lateral Fricatives (next line):
  (Consonant  Voiceless  Alveolar       LateralFricative   PulmonicEgressive)
    → Just "ɬ"
  (Consonant  Voiced     Alveolar       LateralFricative   PulmonicEgressive)
    → Just "ɮ" -- Approximants (next line):
  (Consonant  Voiced     LabioDental    Approximant        PulmonicEgressive)
    → Just "ʋ"
  (Consonant  Voiced     Alveolar       Approximant        PulmonicEgressive)
    → Just "ɹ"
  (Consonant  Voiced     Retroflex      Approximant        PulmonicEgressive)
    → Just "ɻ"
  (Consonant  Voiced     Palatal        Approximant        PulmonicEgressive)
    → Just "j"
  (Consonant  Voiced     Velar          Approximant        PulmonicEgressive)
    → Just "ɰ"  -- Lateral Approximants (next line):
  (Consonant  Voiced     Alveolar       LateralApproximant PulmonicEgressive)
    → Just "l"
  (Consonant  Voiced     Retroflex      LateralApproximant PulmonicEgressive)
    → Just "ɭ"
  (Consonant  Voiced     Palatal        LateralApproximant PulmonicEgressive)
    → Just "ʎ"
  (Consonant  Voiced     Velar          LateralApproximant PulmonicEgressive)
    → Just "ʟ" -- Affricates (next line)
  (Consonant  Voiceless  PostAlveolar   Affricate          PulmonicEgressive)
    → Just "t͡ʃ"
  (Consonant  Voiced     PostAlveolar   Affricate          PulmonicEgressive)
    → Just "d͡ʒ"
  (Consonant  Voiceless  Bilabial       Affricate          PulmonicEgressive)
    → Just "p͡ɸ"
  (Consonant  Voiceless  Alveolar       Affricate          PulmonicEgressive)
    → Just "t͜s"
  (Consonant  Voiced     Alveolar       Affricate          PulmonicEgressive)
    → Just "d͡z"
  (Consonant  Voiceless  Velar          Affricate          PulmonicEgressive)
    → Just "k͡x"
  (Consonant  Voiceless  Uvular         Affricate          PulmonicEgressive)
    → Just "q͡χ" -- Under the Other Symbols part of the IPA chart:
  (Consonant  Voiced     LabialVelar    Approximant        PulmonicEgressive)
    → Just "w"
  (Consonant  Voiceless  LabialVelar    Fricative          PulmonicEgressive)
    → Just "ʍ"
  (Consonant  Voiced     LabialPalatal  Approximant        PulmonicEgressive)
    → Just "ɥ"
  (Consonant  Voiceless  Epiglottal     Fricative          PulmonicEgressive)
    → Just "ʜ"
  (Consonant  Voiced     Epiglottal     Fricative          PulmonicEgressive)
    → Just "ʢ"
  (Consonant  Voiceless  Epiglottal     Plosive            PulmonicEgressive)
    → Just "ʡ" -- Is the epiglottal plosive voiceless? The IPA chart
               -- does not specify.
  (Consonant  Voiceless  AlveoloPalatal Fricative          PulmonicEgressive)
    → Just "ɕ"
  (Consonant  Voiced     AlveoloPalatal Fricative          PulmonicEgressive)
    → Just "ʑ"
  (Consonant  Voiced     Alveolar       LateralFlap        PulmonicEgressive)
    → Just "ɺ"
  (Consonant  Voiceless  (Places (PostAlveolar :| [Velar]))
                                        Fricative  PulmonicEgressive)
    → Just "ɧ" -- Other Consonants:
  (Consonant  Voiceless  Bilabial       Plosive            Click            )
    → Just "ʘ"
  (Consonant  Voiceless  Dental         Plosive            Click            )
    → Just "ǀ"
  (Consonant  Voiceless  Alveolar       Plosive            Click            )
    → Just "ǃ" -- Or it could be PostAlveolar.
  (Consonant  Voiceless  PalatoAlveolar Plosive            Click            )
    → Just "ǂ"
  (Consonant  Voiceless  Alveolar       Lateral            Click            )
    → Just "ǁ"
  (Consonant  Voiced     Bilabial       Plosive            Implosive        )
    → Just "ɓ"
  (Consonant  Voiced     Dental         Plosive            Implosive        )
    → Just "ɗ"  -- Or Alveolar
  (Consonant  Voiced     Palatal        Plosive            Implosive        )
    → Just "ʄ"
  (Consonant  Voiced     Velar          Plosive            Implosive        )
    → Just "ɠ"
  (Consonant  Voiced     Uvular         Plosive            Implosive        )
    → Just "ʛ" -- Close Vowels (next line):
  (Vowel      Close      Front          Unrounded          Voiced           )
    → Just "i"
  (Vowel      Close      Front          Rounded            Voiced           )
    → Just "y"
  (Vowel      Close      Central        Unrounded          Voiced           )
    → Just "ɨ"
  (Vowel      Close      Central        Rounded            Voiced           )
    → Just "ʉ"
  (Vowel      Close      Back           Unrounded          Voiced           )
    → Just "ɯ"
  (Vowel      Close      Back           Rounded            Voiced           )
    → Just "u" -- Near-close Vowels (next line):
  (Vowel      NearClose  Front          Unrounded          Voiced           )
    → Just "ɪ"
  (Vowel      NearClose  Front          Rounded            Voiced           )
    → Just "ʏ"
  (Vowel      NearClose  Back           Rounded            Voiced           )
    → Just "ʊ" -- Close-mid Vowels (next line):
  (Vowel      CloseMid   Front          Unrounded          Voiced           )
    → Just "e"
  (Vowel      CloseMid   Front          Rounded            Voiced           )
    → Just "ø"
  (Vowel      CloseMid   Central        Unrounded          Voiced           )
    → Just "ɘ"
  (Vowel      CloseMid   Central        Rounded            Voiced           )
    → Just "ɵ"
  (Vowel      CloseMid   Back           Unrounded          Voiced           )
    → Just "ɤ"
  (Vowel      CloseMid   Back           Rounded            Voiced           )
    → Just "o" -- Mid Vowels (next line):
  (Vowel      Mid        Central        Unrounded          Voiced           )
    → Just "ə" -- Open-mid Vowels (next line):
  (Vowel      OpenMid    Front          Unrounded          Voiced           )
    → Just "ɛ"
  (Vowel      OpenMid    Front          Rounded            Voiced           )
    → Just "œ"
  (Vowel      OpenMid    Central        Unrounded          Voiced           )
    → Just "ɜ"
  (Vowel      OpenMid    Central        Rounded            Voiced           )
    → Just "ɞ"
  (Vowel      OpenMid    Back           Unrounded          Voiced           )
    → Just "ʌ"
  (Vowel      OpenMid    Back           Rounded            Voiced           )
    → Just "ɔ" -- Near-open (next line)
  (Vowel      NearOpen   Front          Unrounded          Voiced           )
    → Just "æ"
  (Vowel      NearOpen   Central        Unrounded          Voiced           )
    → Just "ɐ" -- Open Vowels (next line):
  (Vowel      Open       Front          Unrounded          Voiced           )
    → Just "a"
  (Vowel      Open       Front          Rounded            Voiced           )
    → Just "ɶ"
  (Vowel      Open       Back           Unrounded          Voiced           )
    → Just "ɑ"
  (Vowel      Open       Back           Rounded            Voiced           )
    → Just "ɒ"




-- The following two lines are commented out, because I am unsure
-- about their place of articulation:
-- constructIPARecursive _ _ (Consonant  Voiceless LabialVelar? Affricate
--     PulmonicEgressive) = "k͡p"
-- constructIPARecursive _ _ (Consonant  Voiceless Palatal (or AlveoloPalatal?)
--     Affricate PulmonicEgressive) = "c͡ɕ"




  -- If it can represent it as a single character it will
  -- return the single character result (i.e. without diacritics),
  -- otherwise
  -- it will try to represent it in IPA with more than
  -- one character

  (Consonant  x PostAlveolar y z)
    | recursionLevel < recursionLimit →
    case constructIPARecursive recursionLimit
           (1 + recursionLevel) (Consonant x Alveolar y z) of
      Nothing → Nothing
      Just regularIPA → Just (regularIPA ⊕ "̠")
      -- Add the diacritic for "retracted"



-- If there isn't a symbol, and the consonant we want is voiceless,
-- Just take the symbol for a voiced consonant,
-- and then put that diacritic that means voiceless after.
-- (The following two definitions are intended to implement that)
-- Add the small circle diacritic to consonants to make them voiceless.
  (Consonant Voiceless x y z)
    | recursionLevel <  recursionLimit
    → case constructIPARecursive recursionLimit
             (1 + recursionLevel)  (Consonant Voiced x y z) of
           Nothing → Nothing
           Just regularIPA → Just (regularIPA ⊕ "̥")
           -- add diacritic for voiceless

-- Add the small circle diacritic to vowels to make them voiceless.
  (Vowel x y z Voiceless)
    | recursionLevel <  recursionLimit
    → case constructIPARecursive recursionLimit
             (1 + recursionLevel) (Vowel x y z Voiced) of
           Nothing → Nothing
           Just regularIPA → Just (regularIPA ⊕ "̥")

-- If there is no way to express a voiced consonant in a single
-- grapheme add a diacritic to the grapheme that represents
-- the voiceless counterpart.
  (Consonant Voiced x y z)
    | recursionLevel <  recursionLimit
    → case constructIPARecursive
             recursionLimit (1 + recursionLevel) (Consonant Voiceless x y z) of
           Nothing → Nothing
           Just regularIPA → Just (regularIPA ⊕ "̬")

  (Vowel x y z Voiced)
    | recursionLevel <  recursionLimit
    → case constructIPARecursive recursionLimit
             (1 + recursionLevel) (Vowel x y z Voiceless) of
           Nothing → Nothing
           Just regularIPA → Just (regularIPA ⊕ "̬")

  (Consonant VoicedAspirated _ _ PulmonicEgressive)
    | recursionLevel <  recursionLimit
    → let result = constructIPARecursive
                     recursionLimit (1 + recursionLevel) (deaspirate p)
      in case result of
           Nothing         → Nothing
           Just regularIPA → Just (regularIPA ⊕ "ʰ")

  (Consonant VoicelessAspirated _ _ PulmonicEgressive)
    | recursionLevel <  recursionLimit
    → let result = constructIPARecursive
                     recursionLimit (1 + recursionLevel) (deaspirate p)
      in case result of
           Nothing         → Nothing
           Just regularIPA → Just (regularIPA ⊕ "ʰ")

  (Consonant CreakyVoiced _ _ PulmonicEgressive)
    | recursionLevel <  recursionLimit
    → let result = constructIPARecursive
                     recursionLimit (1 + recursionLevel) (decreak p)
      in case result of
           Just regularIPA → Just (regularIPA ⊕ "̰")
           Nothing         → Nothing
  _                          → Nothing




deaspirate ∷ Phonet → Phonet
deaspirate (Consonant VoicedAspirated place manner airstream) =
  Consonant Voiced place manner airstream

deaspirate (Consonant VoicelessAspirated place₁ manner₁ airstream₁) =
  Consonant Voiceless place₁ manner₁ airstream₁

deaspirate x = x

decreak ∷ Phonet → Phonet
decreak (Consonant CreakyVoiced place manner airstream) =
  Consonant Voiced place manner airstream
decreak x = x

constructDeconstruct ∷ (Phonet → Phonet) → Text → Text
constructDeconstruct func x =
  let something = analyzeIPA x
  in case something of
       Nothing → "∅"
       Just phonet → constructIPA (func phonet)

voicedIPA ∷ Text → Text
voicedIPA = constructDeconstruct voicedPhonet

devoicedIPA ∷ Text → Text
devoicedIPA = constructDeconstruct devoicedPhonet

spirantizedIPA ∷ Text → Text
spirantizedIPA = constructDeconstruct spirantizedPhonet


{-|
Return an english description of a phoneme,
given a phoneme's representation in the
international phonetic alphabet.
  |-}
describeIPA ∷ Text → Text
describeIPA x =
  maybe noEnglishDescriptionFound_message showPhonet (analyzeIPA x)


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

binaryDifference ∷
          (Polarity → PhonemeFeature)
                    → [PhonemeFeature]
                    → [PhonemeFeature]
                    → (Maybe PhonemeFeature, Maybe PhonemeFeature)
binaryDifference feature list₁ list₂
  | relevantList₁ ≡ relevantList₂
  = (Nothing, Nothing)
  | otherwise
  = (relevantList₁ !!? 0, relevantList₂ !!? 0)
   where
   relevantList₁ = filter (relevantBinary feature) list₁
   relevantList₂ = filter (relevantBinary feature) list₂


unaryDifference ∷ PhonemeFeature
                → [PhonemeFeature]
                → [PhonemeFeature]
                → (Maybe PhonemeFeature, Maybe PhonemeFeature)
unaryDifference feature list₁ list₂
  | (feature ∈ list₁) ≡ (feature ∈ list₂) = (Nothing, Nothing)
  | feature ∈ list₁ ∧ feature ∉ list₂     = (Just feature, Nothing)
  | otherwise                             = (Nothing, Just feature)



-- | This function takes two lists of phoneme features
-- and returns how they differ. Any phonemic
-- feature present in one list, and absent in the other
-- will be represented; and any phonemic
-- feature that is positive in one list but absent
-- in the other will be represented.
difference ∷ [PhonemeFeature]
           → [PhonemeFeature]
           → [(Maybe PhonemeFeature, Maybe PhonemeFeature)]
difference list₁ list₂ =
  [ binaryDifference SyllabicFeature           list₁ list₂
  , binaryDifference ConsonantalFeature        list₁ list₂
  , binaryDifference SonorantFeature           list₁ list₂
  , binaryDifference ContinuantFeature         list₁ list₂
  , binaryDifference VoiceFeature              list₁ list₂
  , binaryDifference AdvancedTongueRootFeature list₁ list₂
  , unaryDifference  NasalFeature              list₁ list₂
  , unaryDifference  LateralFeature            list₁ list₂
  , unaryDifference  DelayedReleaseFeature     list₁ list₂
  , unaryDifference  SpreadGlottisFeature      list₁ list₂
  , unaryDifference  ConstrictedGlottisFeature list₁ list₂
  , unaryDifference  LabialFeature             list₁ list₂
  , unaryDifference  CoronalFeature            list₁ list₂
  , unaryDifference  DorsalFeature             list₁ list₂
  , unaryDifference  PharyngealFeature         list₁ list₂
  , unaryDifference  LaryngealFeature          list₁ list₂
  , binaryDifference RoundFeature              list₁ list₂
  , binaryDifference AnteriorFeature           list₁ list₂
  , binaryDifference DistributedFeature        list₁ list₂
  , binaryDifference StridentFeature           list₁ list₂
  , binaryDifference HighFeature               list₁ list₂
  , binaryDifference LowFeature                list₁ list₂
  , binaryDifference BackFeature               list₁ list₂
  ]

{-|
Vowels are [+syllabic]
Consonants (glides included) are [-syllabic].

(Source: page 258)
|-}
syllabic ∷ Phonet → Maybe PhonemeFeature
syllabic Vowel {}     = Just (SyllabicFeature Plus)
syllabic Consonant {} = Just (SyllabicFeature Minus)

{-|
Whether a segment is a glide.
|-}
isGlide ∷ Phonet → Bool
isGlide p = case p of
  (Consonant _ Palatal       Approximant PulmonicEgressive) → True
  (Consonant _ LabialVelar   Approximant PulmonicEgressive) → True
  (Consonant _ LabialPalatal Approximant PulmonicEgressive) → True
  (Consonant _ Velar         Approximant PulmonicEgressive) → True
  _                                                         → False

{-|
Vowels are [-consonantal].
Glides are [-consonantal].
Consonants (that are not glides) are [+consonantal].

(Source: page 258)
|-}
consonantal ∷ Phonet → Maybe PhonemeFeature
consonantal p = case p of
  Vowel {}      → Just (ConsonantalFeature Minus)
  Consonant {}
    | isGlide p → Just (ConsonantalFeature Minus)
    | otherwise → Just (ConsonantalFeature Plus)


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
sonorant p = case p of
  (Consonant _ _ Plosive     _) → Just (SonorantFeature Minus)
  (Consonant _ _ Affricate   _) → Just (SonorantFeature Minus)
  (Consonant _ _ Fricative   _) → Just (SonorantFeature Minus)
  (Consonant _ _ Nasal       _) → Just (SonorantFeature Plus)
  (Consonant _ _ Approximant _) → Just (SonorantFeature Plus)
  (Consonant _ _ Lateral     _) → Just (SonorantFeature Plus)
  Vowel {}                      → Just (SonorantFeature Plus)
  Consonant {}
            | isGlide p         → Just (SonorantFeature Plus)
            | otherwise         → Just (SonorantFeature Minus)

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
continuant p = case p of
  (Consonant _ _ Plosive            _) → Just (ContinuantFeature Minus)
  (Consonant _ _ Nasal              _) → Just (ContinuantFeature Minus)
  (Consonant _ _ Affricate          _) → Just (ContinuantFeature Minus)
  (Consonant _ _ Approximant        _) → Just (ContinuantFeature Plus)
  Vowel {}                             → Just (ContinuantFeature Plus)
  Consonant {}
    | isGlide p                        → Just (ContinuantFeature Plus)
    | otherwise                        → Nothing

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
lateral p = case p of
  (Consonant _ _ Lateral            _) → Just LateralFeature
  (Consonant _ _ LateralApproximant _) → Just LateralFeature
  (Consonant _ _ LateralFricative   _) → Just LateralFeature
  (Consonant _ _ LateralFlap        _) → Just LateralFeature
  _                                    → Nothing

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
labial p = case p of
  (Consonant _ Bilabial    _ _) → Just LabialFeature
  (Consonant _ LabioDental _ _) → Just LabialFeature
  _                             → Nothing


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
coronal p = case p of
  (Consonant _ Dental         _ _) → Just CoronalFeature
  (Consonant _ Alveolar       _ _) → Just CoronalFeature
  (Consonant _ AlveoloPalatal _ _) → Just CoronalFeature
  (Consonant _ Retroflex      _ _) → Just CoronalFeature
  (Consonant _ Palatal        _ _) → Just CoronalFeature
  (Consonant _ PostAlveolar   _ _) → Just CoronalFeature
  _                                → Nothing


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
dorsal p = case p of
  (Consonant _ Palatal        _ _) → Just DorsalFeature
  (Consonant _ Velar          _ _) → Just DorsalFeature
  (Consonant _ Uvular         _ _) → Just DorsalFeature
  _                                → Nothing


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
voice p = case p of
  (Consonant Voiceless Glottal Plosive PulmonicEgressive)
    → Just (VoiceFeature Minus) -- The voiceless glottal plosive is [-voice]
  (Consonant VoicedAspirated _ _ _)
    → Just (VoiceFeature Plus)
  (Consonant Voiced          _ _ _)
    → Just (VoiceFeature Plus)
  (Vowel _ _ _               Voiced)
    → Just (VoiceFeature Plus)
  _
    → Just (VoiceFeature Minus)

{-|
Voiceless aspirated plosives are [spread glottis].
Voiced aspirated plosives are [spread glottis].
All other segments are not defined for [spread glottis].
(Source: page 262)
|-}
spreadGlottis ∷ Phonet → Maybe PhonemeFeature
spreadGlottis p = case p of
  (Consonant VoicelessAspirated _ Plosive _) → Just SpreadGlottisFeature
  (Consonant VoicedAspirated    _ Plosive _) → Just SpreadGlottisFeature
  _                                          → Nothing


{-|
Ejectives have the feature [constricted glottis].
Glottal stop have the feature [constricted glottis].
Creaky voiced sonorants have the feature [constricted glottis].

(Source: page 262)
|-}
constrictedGlottis ∷ Phonet → Maybe PhonemeFeature
constrictedGlottis p = case p of
  (Consonant _ Glottal Plosive _) →
    Just ConstrictedGlottisFeature
  (Consonant CreakyVoiced _ _ _) →
    if sonorant p ≡ Just (SonorantFeature Plus)
      then Just ConstrictedGlottisFeature
      else Nothing
  (Vowel _ _ _ CreakyVoiced) →
    if sonorant p ≡ Just (SonorantFeature Plus)
      then Just ConstrictedGlottisFeature
      else Nothing
  _ →
    Nothing

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
anterior p = case p of
  (Consonant _ Dental            _ _) → Just (AnteriorFeature Plus)
  (Consonant _ Alveolar          _ _) → Just (AnteriorFeature Plus)
  (Consonant _ PostAlveolar      _ _) → Just (AnteriorFeature Minus)
  (Consonant _ Retroflex         _ _) → Just (AnteriorFeature Minus)
  (Consonant _ Palatal           _ _) → Just (AnteriorFeature Minus)
  (Consonant _ AlveoloPalatal    _ _) → Just (AnteriorFeature Minus)
  _                                   → Nothing

distributed ∷ Phonet → Maybe PhonemeFeature
distributed p = case p of
  (Consonant _ Dental         _ _) → Just (DistributedFeature Plus)
  (Consonant _ Alveolar       _ _) → Just (DistributedFeature Minus)
  (Consonant _ PostAlveolar   _ _) → Just (DistributedFeature Plus)
  (Consonant _ Retroflex      _ _) → Just (DistributedFeature Minus)
  (Consonant _ Palatal        _ _) → Just (DistributedFeature Plus)
  (Consonant _ AlveoloPalatal _ _) → Just (DistributedFeature Plus)
  _                                → Nothing


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
strident p = case p of
  (Consonant _ Alveolar     Fricative _) → Just (StridentFeature Plus)
  (Consonant _ Alveolar     Affricate _) → Just (StridentFeature Plus)
  (Consonant _ PostAlveolar Fricative _) → Just (StridentFeature Plus)
  (Consonant _ PostAlveolar Affricate _) → Just (StridentFeature Plus)
  (Consonant _ LabioDental  Fricative _) → Just (StridentFeature Plus)
  (Consonant _ LabioDental  Affricate _) → Just (StridentFeature Plus)
  (Consonant _ Uvular       Fricative _) → Just (StridentFeature Plus)
  (Consonant _ Uvular       Affricate _) → Just (StridentFeature Plus)
  (Consonant _ _            Fricative _) → Just (StridentFeature Minus)
  (Consonant _ _            Affricate _) → Just (StridentFeature Minus)
  _                                      → Nothing


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
high p = case p of
  (Consonant _ Palatal        _ _) → Just (HighFeature Plus)
  (Consonant _ AlveoloPalatal _ _) → Just (HighFeature Plus)
  (Consonant _ Velar          _ _) → Just (HighFeature Plus)
  (Consonant _ Uvular         _ _) → Just (HighFeature Minus)
  Consonant {}                     → Nothing
  (Vowel Close              _ _ _) → Just (HighFeature Plus)
  (Vowel NearClose          _ _ _) → Just (HighFeature Plus)
  Vowel {}                         → Just (HighFeature Minus)


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
low p = case p of
  (Consonant _ Uvular     _ _) → Just (LowFeature Plus)
  (Consonant _ Pharyngeal _ _) → Just (LowFeature Plus)
  (Consonant _ Glottal    _ _) → Just (LowFeature Plus)
  Consonant {}                 → Nothing
  (Vowel Open _           _ _) → Just (LowFeature Plus)
  (Vowel NearOpen _       _ _) → Just (LowFeature Plus)
  Vowel {}                     → Just (LowFeature Minus)


{-|
Back vowels are [+back].
Central vowels are [+back].
Front vowels are [-back].
All other segments are undefined for [+/-back].
|-}
back ∷ Phonet → Maybe PhonemeFeature
back p = case p of
  (Vowel _ Back    _ _) → Just (BackFeature Plus)
  (Vowel _ Central _ _) → Just (BackFeature Plus)
  (Vowel _ Front   _ _) → Just (BackFeature Minus)
  _                     → Nothing


{-|
Rounded vowels are [+round].
All other vowels are [-round].
All other segments are [-round].
|-}
lipRound ∷ Phonet → Maybe PhonemeFeature
lipRound p = case p of
  (Vowel _ _ Rounded _) → Just (RoundFeature Plus)
  Vowel {}              → Just (RoundFeature Minus)
  _                     → Just (RoundFeature Minus)

{-|
Advanced tongue root
|-}
atr ∷ Phonet → Maybe PhonemeFeature
atr p = case p of
  (Vowel  Close     Front   Unrounded Voiced)
    → Just (AdvancedTongueRootFeature Plus)
  (Vowel  CloseMid  Front   Unrounded Voiced)
    → Just (AdvancedTongueRootFeature Plus)
  (Vowel  Close     Back    Rounded   Voiced)
    → Just (AdvancedTongueRootFeature Plus)
  (Vowel  CloseMid  Front   Rounded   Voiced)
    → Just (AdvancedTongueRootFeature Plus)
  (Vowel  CloseMid  Back    Rounded   Voiced)
    → Just (AdvancedTongueRootFeature Plus)
  (Vowel  Close     Front   Rounded   Voiced)
    → Just (AdvancedTongueRootFeature Plus)
  (Vowel  NearOpen  Front   Unrounded Voiced)
    → Just (AdvancedTongueRootFeature Minus)
  (Vowel  Open      Back    Unrounded Voiced)
    → Just (AdvancedTongueRootFeature Minus)
  (Vowel  Close     Central Unrounded Voiced)
    → Just (AdvancedTongueRootFeature Minus)
  (Vowel  OpenMid   Back    Unrounded Voiced)
    → Just (AdvancedTongueRootFeature Minus)
  (Vowel  NearClose Front   Unrounded Voiced)
    → Just (AdvancedTongueRootFeature Minus)
  (Vowel  NearClose Back    Rounded   Voiced)
    → Just (AdvancedTongueRootFeature Minus)
  (Vowel  OpenMid   Front   Unrounded Voiced)
    → Just (AdvancedTongueRootFeature Minus)
  (Vowel  OpenMid   Back    Rounded   Voiced)
    → Just (AdvancedTongueRootFeature Minus)
  _
    → Nothing


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
    , lipRound             phonete
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

showFeatures ∷ [PhonemeFeature] → Text
showFeatures features =
  let featuresStrings ∷ [Text]
      featuresStrings = map showPhonemeFeature features
  in "[" ⊕ T.intercalate "; " featuresStrings ⊕ "]"

toTextFeatures ∷ Phonet → Text
toTextFeatures phonete =
  let features = analyzeFeatures phonete
  in showFeatures features


showPhonet ∷ Phonet → Text
showPhonet phonet =
  case phonet of
    Consonant v p m a → unwords [showVocalFolds v, showPlace p
                      , showManner m    , showAirstream a, consonant_Text]
    Vowel h b r v     → unwords [showVocalFolds v, showRounding r
                      , showHeight h    , showBackness b , vowel_Text]




showBackness ∷ Backness → Text
showBackness Front            = front_BacknessText
showBackness Central          = central_BacknessText
showBackness Back             = back_BacknessText




showHeight ∷ Height → Text
showHeight height =
  case height of
    Close     → close_HeightText
    NearClose → nearClose_HeightText
    CloseMid  → closeMid_HeightText
    Mid       → mid_HeightText
    OpenMid   → openMid_HeightText
    NearOpen  → nearOpen_HeightText
    Open      → open_HeightText


showRounding ∷ Rounding → Text
showRounding Rounded          = rounded_RoundingText
showRounding Unrounded        = unrounded_RoundingText




showPlace ∷ Place → Text
showPlace place₁ =
  case place₁ of
    Bilabial       → bilabial_PlaceText
    LabioDental    → labioDental_PlaceText
    Dental         → dental_PlaceText
    Alveolar       → alveolar_PlaceText
    PostAlveolar   → postAlveolar_PlaceText
    Retroflex      → retroflex_PlaceText
    Palatal        → palatal_PlaceText
    Velar          → velar_PlaceText
    Uvular         → uvular_PlaceText
    Pharyngeal     → pharyngeal_PlaceText
    Glottal        → glottal_PlaceText
    Epiglottal     → epiglottal_PlaceText
    LabialVelar    → labialVelar_PlaceText
    LabialPalatal  → labialPalatal_PlaceText
    AlveoloPalatal → alveoloPalatal_PlaceText
    PalatoAlveolar → palatoAlveolar_PlaceText
    Places ps      → unwords (toList (fmap showPlace ps))



showManner ∷ Manner → Text
showManner manner₁ =
  case manner₁ of
    Plosive            → plosive_MannerText
    Nasal              → nasal_MannerText
    Trill              → trill_MannerText
    TapOrFlap          → tapOrFlap_MannerText
    Approximant        → approximant_MannerText
    Fricative          → fricative_MannerText
    Affricate          → affricate_MannerText
    LateralFricative   → lateralFricative_MannerText
    LateralApproximant → lateralApproximant_MannerText
    LateralFlap        → lateralFlap_MannerText
    Lateral            → lateral_MannerText



showAirstream ∷ Airstream → Text
showAirstream airstream₁ =
  case airstream₁ of
    PulmonicEgressive → pulmonicEgressive_AirstreamText
    Click             → click_AirstreamText
    Implosive         → implosive_AirstreamText


showVocalFolds ∷ VocalFolds → Text
showVocalFolds vocalFolds₁ =
  case vocalFolds₁ of
    Voiced             → voiced_VocalFoldsText
    Voiceless          → voiceless_VocalFoldsText
    VoicedAspirated    → voicedAspirated_VocalFoldsText
    VoicelessAspirated → voicelessAspirated_VocalFoldsText
    CreakyVoiced       → creakyVoiced_VocalFoldsText

showPhonetInventory ∷ PhonetInventory → Text
showPhonetInventory (PhonetInventory phonetes)
  = sconcat (fmap showPhonet phonetes)


showPolarity ∷ Polarity → Text
showPolarity Plus = "+"
showPolarity Minus = "-"


showPhonemeFeature ∷ PhonemeFeature → Text
showPhonemeFeature pf =
  case pf of
    (SyllabicFeature p)           → showPolarity p ⊕ syllabic_PhonemeFeatureText
    (ConsonantalFeature p)        → showPolarity p ⊕ consonantal_PhonemeFeatureText
    (SonorantFeature p)           → showPolarity p ⊕ sonorant_PhonemeFeatureText
    (ContinuantFeature p)         → showPolarity p ⊕ continuant_PhonemeFeatureText
    (VoiceFeature p)              → showPolarity p ⊕ voice_PhonemeFeatureText
    (AdvancedTongueRootFeature p) → showPolarity p ⊕ atr_PhonemeFeatureText
    NasalFeature                  →                  nasal_PhonemeFeatureText
    LateralFeature                →                  lateral_PhonemeFeatureText
    DelayedReleaseFeature         →                  delayedRelease_PhonemeFeatureText
    SpreadGlottisFeature          →                  spreadGlottis_PhonemeFeatureText
    ConstrictedGlottisFeature     →                  constrictedGlottis_PhonemeFeatureText
    LabialFeature                 →                  labial_PhonemeFeatureText
    CoronalFeature                →                  coronal_PhonemeFeatureText
    DorsalFeature                 →                  dorsal_PhonemeFeatureText
    PharyngealFeature             →                  pharyngeal_PhonemeFeatureText
    LaryngealFeature              →                  laryngeal_PhonemeFeatureText
    (RoundFeature p)              → showPolarity p ⊕ round_PhonemeFeatureText
    (AnteriorFeature p)           → showPolarity p ⊕ anterior_PhonemeFeatureText
    (DistributedFeature p)        → showPolarity p ⊕ distributed_PhonemeFeatureText
    (StridentFeature p)           → showPolarity p ⊕ strident_PhonemeFeatureText
    (HighFeature p)               → showPolarity p ⊕ high_PhonemeFeatureText
    (LowFeature p)                → showPolarity p ⊕ low_PhonemeFeatureText
    (BackFeature p)               → showPolarity p ⊕ back_PhonemeFeatureText
