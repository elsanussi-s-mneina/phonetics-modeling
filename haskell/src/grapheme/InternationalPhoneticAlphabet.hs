module InternationalPhoneticAlphabet where

import Lib

import Prelude
  (
    Bool(False, True), Char, Eq, Int, String,
    concat, concatMap, div, elem, length, mod, otherwise, show,
    (.), (+), (*), (!!), (++), (==)
  )

type IPAText = String
-- For storing text meant to be interpreted as International phonetic alphabet



-- Grammar for combining IPA symbols
-- to express phonemes
--  I will have to do more work on this in order to cover all combinations.
--  I also am unsure what to do with combinations of symbols that lead to contradition
-- like putting a diacritic meaning voiceless with a diacritic meaning voiced.
-- 
-- 
--  types of symbols:
--  diacritics, ascenders, descenders, ascender-descenders, 
-- premodifier, postmodifier
-- diacritics take up no space, but combine with
-- a non-diacritic character
-- 
-- Note: I did not use the '|' symbol with its usual meaning in grammars, it means 'or' usually,
-- but in this case, it would be (visually) indistinguishable from certain IPA
-- symbols, so we don't use it..

-- (assume spaces are disjunctions in the right hand side of the following rules)
-- ascender → b t d k ʔ f θ ð ħ ʕ h ɦ ɬ l ʎ ʘ ɓ ǀ ɗ ǃ ǂ ɠ ʄ ǁ ʛ ɺ ʢ ʡ ɤ
-- midheight → c ɢ m n  ɴ ʙ r ʀ ⱱ ɾ v s z x ʁ ʋ ɹ ʟ ʍ ɕ w ʑ ʜ i ɨ ʉ ɯ u ɪ ʏ ʊ e ø ɘ ɵ o ə ɛ œ ɜ ɞ ʌ ɔ æ ɐ a ɶ ɑ ɒ
-- descender → p ɟ g q ɱ ɽ ʒ ʂ ʐ ç ʝ ɣ χ ɻ j ɰ ɥ y ɳ ɲ
-- ascender_descender → ʈ ɖ ɸ β ʃ ɮ ɭ ɧ
-- diacritic_below →  ̥ ̩
-- 
-- diacritic_above →  ̊  ̍  
-- (assume spaces are concatenation on the right hand side of the following rules)
-- composed_character → ascender diacritic_below 
-- composed_character → midheight diacritic_below
-- composed_character → descender diacritic_above
-- End of grammar
-- to do: a lot (fully sketch out the grammar, cover use of tie bars,
-- tone symbols, and aspiration symbols, and more.
--  Then implement it in functions in a programming language.


exponentials = ['ʰ' , 'ʷ' , 'ʲ' , 'ˠ' , 'ˤ' , 'ⁿ' , 'ˡ']

{-|
Whether an IPA character is written above the base line
and to the right of the previous character,
like how exponents of a power are written
in mathematical notation.
|-}
isExponential :: Char -> Bool
isExponential character = character `elem` exponentials
{-|
Whether a diacritic goes above
the character it is placed on.
|-}
isDiacriticAbove :: Char -> Bool
isDiacriticAbove '̊' = True
isDiacriticAbove  _  = False

{-|
Whether a diacritic goes below
the character which it is placed on.
|-}
isDiacriticBelow :: Char -> Bool
isDiacriticBelow '̥' = True
isDiacriticBelow  _  = False


{-|
Whether a character (but not a diacritic)
takes up space
below the imaginary horizontal line
on which it is written.

This could be useful later for determining
where to put diacritics so that
they are readable.
|-}

ascenders =
  ['b', 't', 'd', 'k', 'ʔ', 'f', 'θ', 'ð', 'ħ', 'ʕ', 'h', 'ɦ', 'ɬ', 'l', 'ʎ',
  'ʘ', 'ɓ', 'ǀ', 'ɗ', 'ǃ', 'ǂ', 'ɠ', 'ʄ', 'ǁ', 'ʛ', 'ɺ', 'ʢ', 'ʡ', 'ɤ', 'ʈ', 'ɖ',
  'ɸ', 'β', 'ʃ', 'ɮ', 'ɭ', 'ɧ']


isAscender :: Char -> Bool
isAscender character = character `elem` ascenders

descenders =
  ['p', 'ɟ', 'g', 'q', 'ɱ', 'ɽ', 'ʒ', 'ʂ', 'ʐ', 'ç', 'ʝ', 'ɣ', 'χ', 'ɻ', 'j',
   'ɰ', 'ɥ', 'y', 'ɳ', 'ɲ', 'ʈ', 'ɖ', 'ɸ', 'β', 'ʃ', 'ɮ', 'ɭ', 'ɧ']


{-|
Whether a character (but not a diacritic)
takes up space
below the imaginary horizontal line
on which it is written.

This could be useful later for determining
where to put diacritics so that
they are readable.
|-}
isDescender :: Char -> Bool
isDescender character = character `elem` descenders

graphemesOfIPA :: [Char]
graphemesOfIPA = consonantsPulmonic 
  ++ consonantsNonPulmonic
  ++ otherSymbols
  ++ vowels
  ++ suprasegmentals 
  ++ toneAndWordAccents
  ++ diacriticsAndSuprasegmentals
-- See: https://www.internationalphoneticassociation.org/sites/default/files/IPA_Kiel_2015.pdf
-- For the source of this information..

-- CONSONANTS (PULMONIC)
consonantsPulmonic :: [Char]
consonantsPulmonic = concat consonantsPulmonicTable



consonantsPulmonicTable :: [[Char]]
consonantsPulmonicTable =
 [[ 'p', 'b', ' ', ' ', ' ', ' ', 't', 'd', ' ', ' ', 'ʈ', 'ɖ', 'c', 'ɟ', 'k', 'g', 'q', 'ɢ', ' ', ' ', 'ʔ', ' '] -- Plosive
 ,[ ' ', 'm', ' ', 'ɱ', ' ', ' ', ' ', 'n', ' ', ' ', ' ', 'ɳ', ' ', 'ɲ', ' ', 'ŋ', ' ', 'ɴ', ' ', ' ', ' ', ' '] -- Nasal
 ,[ ' ', 'ʙ', ' ', ' ', ' ', ' ', ' ', 'r', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'ʀ', ' ', ' ', ' ', ' '] -- Trill
 ,[ ' ', ' ', ' ', 'ⱱ', ' ', ' ', ' ', 'ɾ', ' ', ' ', ' ', 'ɽ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '] -- Tap or Flap
 ,[ 'ɸ', 'β', 'f', 'v', 'θ', 'ð', 's', 'z', 'ʃ', 'ʒ', 'ʂ', 'ʐ', 'ç', 'ʝ', 'x', 'ɣ', 'χ', 'ʁ', 'ħ', 'ʕ', 'h', 'ɦ']  -- Fricative
 ,[ ' ', ' ', ' ', ' ', ' ', ' ', 'ɬ', 'ɮ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '] -- Lateral fricative
 ,[ ' ', ' ', ' ', 'ʋ', ' ', ' ', ' ', 'ɹ', ' ', ' ', ' ', 'ɻ', ' ', 'j', ' ', 'ɰ', ' ', ' ', ' ', ' ', ' ', ' '] -- Approximant
 ,[ ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'l', ' ', ' ', ' ', 'ɭ', ' ', 'ʎ', ' ', 'ʟ', ' ', ' ', ' ', ' ', ' ', ' '] -- Lateral approximant
 ]

consonantsNonPulmonic :: [Char]
consonantsNonPulmonic =
-- Clicks   Voiced implosives
 [ 'ʘ',     'ɓ' -- Bilabial
 , 'ǀ', {- Dental -}    'ɗ' -- Dental/alveolar
 , 'ǃ', {-  (Post)alveolar -}  'ʄ'
 , 'ǂ',  'ɠ'
 , 'ǁ',  'ʛ'
 ] 

otherSymbols :: [Char]
otherSymbols =
  ['ʍ',  'ɕ'
  ,'w',  'ʑ'
  ,'ɥ',  'ɺ'
  ,'ʜ',  'ɧ'
  ,'ʢ'
  ,'ʡ'
  ]

vowels =
  ['i', 'y',   'ɨ', 'ʉ',   'ɯ', 'u'   -- Close
  ,'ɪ', 'ʏ',            'ʊ'
  ,'e', 'ø',   'ɘ', 'ɵ',   'ɤ', 'o'   -- Close-mid
  ,               'ə'
  ,'ɛ', 'œ',   'ɜ', 'ɞ',   'ʌ', 'ɔ'   -- Open-mid
  , 'æ',           'ɐ'
  , 'a', 'ɶ',              'ɑ', 'ɒ'  -- Open
  ]     

suprasegmentals :: [Char]
suprasegmentals =
  [ 'ˈ'   -- Primary stress
  , 'ˌ'   -- Secondary stress
  , 'ː'   -- Long
  , 'ˑ'   -- Half long

  , '̆'    -- Extra short
  , '|'   -- Minor (foot) group 
  , '‖'   -- Major (intonation) group
  , '.'   -- Syllable break
  , '‿'   -- Linking (absence of a break
  ]


toneAndWordAccents :: [Char]
toneAndWordAccents =
{- Level -}
  [ '˥', '̋'  -- Extra high
  , '˦', '́'  -- High
  , '˧', '̄'  -- Mid
  , '˨', '̀'  -- Low
  , '˩', '̏'  -- Extra low
  ,      'ꜜ'  -- Downstep
  ,      'ꜛ'  -- Upstep

{- Countour -}
  , '̌' -- Rising
  , '̂' -- Falling
  , '᷄' -- High rising
  , '᷅' -- Low rising
  , '᷈' -- Rising-falling
  , '↗' -- Global rise
  , '↘' -- Global fall
  ]

diacriticsAndSuprasegmentals :: [Char]
diacriticsAndSuprasegmentals =
  [ 'ʰ'  -- Aspirated
  , 'ʷ'  -- Labialised
  , 'ʲ'  -- Palatalised
  , 'ˠ'  -- Velarised
  , 'ˤ'  -- Pharyngealised
  , 'ⁿ'  -- Pre/post nasalised
  , 'ˡ'  -- Lateral release

  , '˞'  -- Rhoticity
  , 'ʼ'  -- Ejective
  , '̚'   -- No audible release

  , '̩'   -- Syllabic
  , '̯'   -- Non-syllabic
  , '̰'   -- Creaky voiced
  , '̥'   -- Voiceless
  , '̬'   -- Voiced
  , '̤'   -- Breathy voiced
  , '̊'   -- Voiceless (diacritic placed above symbol with descender)
  , '̍'   -- Syllabic (diacritic placed above)
  , '̪'   -- Dental
  , '̺'   -- Apical
  , '̻'   -- Laminal
  , '̼'   -- Linguolabial
  , '.'  -- Closer variety/Fricative
  , '̃'   -- Nasalised
  , '̈'   -- Centralised
  , '̽'   -- Mid centralised
  , '̆'   -- Extra short
  , '̇'    -- Palatalization/Centralization
  ]

showIPA (PhonetInventory phonetes) = concatMap constructIPA phonetes




indexOf :: (Eq a) => [a] -> Int -> a -> Int
indexOf [] index target = -1
indexOf (elem:rest) index target = 
  if elem == target
    then index
    else indexOf rest (index + 1) target

analyzeMannerIPA :: Char -> (Manner, Int)
analyzeMannerIPA x
  | x `elem` (consonantsPulmonicTable !! 0) = (Plosive, 0)
  | x `elem` (consonantsPulmonicTable !! 1) = (Nasal, 1)
  | x `elem` (consonantsPulmonicTable !! 2) = (Trill, 2)
  | x `elem` (consonantsPulmonicTable !! 3) = (TapOrFlap, 3)
  | x `elem` (consonantsPulmonicTable !! 4) = (Fricative, 4)
  | x `elem` (consonantsPulmonicTable !! 5) = (LateralFricative, 5)
  | x `elem` (consonantsPulmonicTable !! 6) = (Approximant, 6)
  | x `elem` (consonantsPulmonicTable !! 7) = (LateralApproximant, 7)
  | otherwise = (LateralApproximant, 7) -- Not right, but will have to work for now. -- TODO: Fix this.

analyzePlaceIPA :: Int -> Place
analyzePlaceIPA colIndex = 
  let colNames = [Bilabial, LabioDental, Dental, Alveolar, PostAlveolar, Retroflex, Palatal, Velar, Uvular, Pharyngeal, Glottal]
  in colNames !! (colIndex `div` 2)

placeToHalfColIndex :: Place -> Int
placeToHalfColIndex place = 
  let colNames = [Bilabial, LabioDental, Dental, Alveolar, PostAlveolar, Retroflex, Palatal, Velar, Uvular, Pharyngeal, Glottal]
  in indexOf colNames 0 place

analyzeIPAv2 :: Char -> Phonet
analyzeIPAv2 x =
  let (manner, rowIndex) = analyzeMannerIPA x 
      colIndex = indexOf (consonantsPulmonicTable !! rowIndex) 0 x
      voicing  = colIndexToVoicing colIndex 
      place    = analyzePlaceIPA colIndex 
  in Consonant voicing place manner PulmonicEgressive

colIndexToVoicing :: Int -> VocalFolds
colIndexToVoicing colIndex = 
  if colIndex `mod` 2 == 0 then Voiceless else Voiced

voicingToColIndexOffset :: VocalFolds -> Int
voicingToColIndexOffset Voiceless          = 0
voicingToColIndexOffset Voiced             = 1
voicingToColIndexOffset VoicelessAspirated = 0
voicingToColIndexOffset VoicedAspirated    = 1
voicingToColIndexOffset CreakyVoiced       = 1
voicingToColIndexOffset UnmarkedVocalFolds = 0


mannerToRowIndex :: Manner -> Int
mannerToRowIndex manner = 
  let rowNames = [Plosive, Nasal, Trill, TapOrFlap, Fricative, LateralFricative, Approximant, LateralApproximant]
  in indexOf rowNames 0 manner

voicingAndPlaceToColIndex :: VocalFolds -> Place -> Int
voicingAndPlaceToColIndex voicing place = 
      (2 * placeToHalfColIndex place) + voicingToColIndexOffset voicing
  

-- | This function will allow us to convert an IPA symbol
-- | to its analyzed form (its phonetic features)
-- Currently, only the consonants (pulmonic) in the 2005 IPA chart are included.
analyzeIPA  :: IPAText -> Phonet

-- Affricates
analyzeIPA "t͡ʃ" = Consonant  Voiceless PostAlveolar Affricate PulmonicEgressive
analyzeIPA "d͡ʒ" = Consonant  Voiced    PostAlveolar Affricate PulmonicEgressive
-- We should probably enforce use of the tie-bar underneath, otherwise
-- it would not be deterministic to determine whether two graphemes here
-- represent affricates or a plosive followed by a fricative.




-- Under the Other Symbols part of the IPA chart:

analyzeIPA "w" = Consonant Voiced    LabialVelar    Approximant PulmonicEgressive
analyzeIPA "ʍ" = Consonant Voiceless LabialVelar    Fricative   PulmonicEgressive
analyzeIPA "ɥ" = Consonant Voiced    LabialPalatal  Approximant PulmonicEgressive
analyzeIPA "ʜ" = Consonant Voiceless Epiglottal     Fricative   PulmonicEgressive
analyzeIPA "ʢ" = Consonant Voiced    Epiglottal     Fricative   PulmonicEgressive
analyzeIPA "ʡ" = Consonant Voiceless Epiglottal     Plosive     PulmonicEgressive -- Is the epiglottal plosive voiceless? The IPA chart does not specify.
analyzeIPA "ɕ" = Consonant Voiceless AlveoloPalatal Fricative   PulmonicEgressive
analyzeIPA "ʑ" = Consonant Voiced    AlveoloPalatal Fricative   PulmonicEgressive
analyzeIPA "ɺ" = Consonant Voiced    Alveolar       LateralFlap PulmonicEgressive

-- We cannot handle the ɧ (simultaneous ʃ and x) because
-- we did not define our data types to handle it yet.
-- In any case, here is some pseudocode for it:
-- analyzeIPA "ɧ" = simultaneous (analyzeIPA "ʃ") (analyzeIPA "x")

-- Other Consonants:
analyzeIPA "ʘ" = Consonant UnmarkedVocalFolds Bilabial       UnmarkedManner Click
analyzeIPA "ǀ" = Consonant UnmarkedVocalFolds Dental         UnmarkedManner Click
analyzeIPA "ǃ" = Consonant UnmarkedVocalFolds Alveolar       UnmarkedManner Click -- Or it could be PostAlveolar.
analyzeIPA "ǂ" = Consonant UnmarkedVocalFolds PalatoAlveolar UnmarkedManner Click
analyzeIPA "ǁ" = Consonant UnmarkedVocalFolds Alveolar       Lateral        Click
analyzeIPA "ɓ" = Consonant Voiced             Bilabial       UnmarkedManner Implosive
analyzeIPA "ɗ" = Consonant Voiced             Dental         UnmarkedManner Implosive  -- Or Alveolar
analyzeIPA "ʄ" = Consonant Voiced             Palatal        UnmarkedManner Implosive
analyzeIPA "ɠ" = Consonant Voiced             Velar          UnmarkedManner Implosive
analyzeIPA "ʛ" = Consonant Voiced             Uvular         UnmarkedManner Implosive

-- Close Vowels:
analyzeIPA "i"  = Vowel  Close Front   Unrounded Voiced
analyzeIPA "y"  = Vowel  Close Front   Rounded   Voiced
analyzeIPA "ɨ"  = Vowel  Close Central Unrounded Voiced
analyzeIPA "ʉ"  = Vowel  Close Central Rounded   Voiced
analyzeIPA "ɯ"  = Vowel  Close Back    Unrounded Voiced
analyzeIPA "u"  = Vowel  Close Back    Rounded   Voiced

-- Near-close Vowels:
analyzeIPA "ɪ"  = Vowel NearClose Front Unrounded Voiced
analyzeIPA "ʏ"  = Vowel NearClose Front Rounded   Voiced
analyzeIPA "ʊ"  = Vowel NearClose Back  Rounded   Voiced

-- Close-mid Vowels:
analyzeIPA "e"  = Vowel  CloseMid Front   Unrounded Voiced
analyzeIPA "ø"  = Vowel  CloseMid Front   Rounded   Voiced
analyzeIPA "ɘ"  = Vowel  CloseMid Central Unrounded Voiced
analyzeIPA "ɵ"  = Vowel  CloseMid Central Rounded   Voiced
analyzeIPA "ɤ"  = Vowel  CloseMid Back    Unrounded Voiced
analyzeIPA "o"  = Vowel  CloseMid Back    Rounded   Voiced

-- Mid Vowels:
analyzeIPA "ə"  = Vowel Mid Central UnmarkedRounding Voiced


-- Open-mid Vowels:
analyzeIPA "ɛ"  = Vowel  OpenMid Front   Unrounded Voiced
analyzeIPA "œ"  = Vowel  OpenMid Front   Rounded   Voiced
analyzeIPA "ɜ"  = Vowel  OpenMid Central Unrounded Voiced
analyzeIPA "ɞ"  = Vowel  OpenMid Central Rounded   Voiced
analyzeIPA "ʌ"  = Vowel  OpenMid Back    Unrounded Voiced
analyzeIPA "ɔ"  = Vowel  OpenMid Back    Rounded   Voiced

-- Near-open
analyzeIPA "æ"  = Vowel  NearOpen Front   Unrounded Voiced
analyzeIPA "ɐ"  = Vowel  NearOpen Central UnmarkedRounding  Voiced

-- Open Vowels:
analyzeIPA "a"  = Vowel  Open Front Unrounded Voiced
analyzeIPA "ɶ"  = Vowel  Open Front Rounded   Voiced
analyzeIPA "ɑ"  = Vowel  Open Back  Unrounded Voiced
analyzeIPA "ɒ"  = Vowel  Open Back  Rounded   Voiced


analyzeIPA x 
   | length x == 1
      = analyzeIPAv2 (x !! 0)

-- Handle Diacritics:
analyzeIPA [firstChar, '̥'] =
  let fullGrapheme = analyzeIPA [firstChar]
  in case fullGrapheme of
          Consonant _ place manner airstream    -> Consonant Voiceless place manner airstream
          Vowel height backness rounding _      -> Vowel height backness rounding Voiceless

analyzeIPA [firstChar, '̬'] =
  let fullGrapheme = analyzeIPA [firstChar]
  in case fullGrapheme of
          Consonant _ place manner airstream    -> Consonant Voiced place manner airstream
          Vowel height backness rounding _      -> Vowel height backness rounding Voiced

analyzeIPA [firstChar, 'ʰ'] =
  let fullGrapheme = analyzeIPA [firstChar]
  in case fullGrapheme of
          Consonant Voiced place manner airstream    -> Consonant VoicedAspirated place manner airstream
          Consonant Voiceless place manner airstream -> Consonant VoicelessAspirated place manner airstream
          Vowel height backness rounding voice       -> Vowel height backness rounding voice
          -- (About the preceding line:) It is strange but we will just do nothing if they give us an aspirated vowel.
          -- since we have no way to represent it in the type system. to do: determine
          -- if the idea of an aspirated vowel makes sense

analyzeIPA _ = (Consonant UnmarkedVocalFolds UnmarkedPlace UnmarkedManner UnmarkedAirstream) -- Not recognized.

constructIPA :: Phonet -> IPAText
-- Affricates
constructIPA (Consonant  Voiceless PostAlveolar  Affricate PulmonicEgressive) = "t͡ʃ"
constructIPA (Consonant  Voiced    PostAlveolar  Affricate PulmonicEgressive) = "d͡ʒ"
constructIPA (Consonant  Voiceless Bilabial      Affricate PulmonicEgressive) = "p͡ɸ"
constructIPA (Consonant  Voiceless Alveolar      Affricate PulmonicEgressive) = "t͜s"
constructIPA (Consonant  Voiced    Alveolar      Affricate PulmonicEgressive) = "d͡z"
constructIPA (Consonant  Voiceless Velar         Affricate PulmonicEgressive) = "k͡x"
constructIPA (Consonant  Voiceless Uvular        Affricate PulmonicEgressive) = "q͡χ"
-- The following two lines are commented out, because I am unsure about their place of articulation:
-- constructIPA (Consonant  Voiceless LabialVelar? Affricate PulmonicEgressive) = "k͡p"
-- constructIPA (Consonant  Voiceless Palatal (or AlveolaPalatal?) Affricate PulmonicEgressive) = "c͡ɕ"



constructIPA phoneDescription =
  -- If it can represent it as a single character it will
  -- return the single character result (i.e. without diacritics),
  -- otherwise
  -- it will try to represent it in IPA with more than
  -- one character
  let simpleResult = constructIPA1 phoneDescription
  in if simpleResult == " "
       then constructIPA2 phoneDescription
       else simpleResult



-- Note to Software Developer: the reason there are three
-- functions for constructing the IPA is to prevent
-- infinite recursion. The reason is that
-- if we only had one function, it would -- for some
-- cases never halt if it could not find a character
-- to place a diacritic on.

-- | Given an analysis construct an IPA symbol
-- | This function will allow us to convert an analyzed form
-- | to its IPA symbol.
-- | Note this only returns one character without diacritics.
constructIPA1  ::  Phonet -> IPAText

-- Under the Other Symbols part of the IPA chart:

constructIPA1 (Consonant Voiced    LabialVelar   Approximant PulmonicEgressive) = "w"
constructIPA1 (Consonant Voiceless LabialVelar   Fricative   PulmonicEgressive) = "ʍ"
constructIPA1 (Consonant Voiced    LabialPalatal Approximant PulmonicEgressive) = "ɥ"
constructIPA1 (Consonant Voiceless Epiglottal    Fricative   PulmonicEgressive) = "ʜ"
constructIPA1 (Consonant Voiced    Epiglottal    Fricative   PulmonicEgressive) = "ʢ"
constructIPA1 (Consonant Voiceless Epiglottal    Plosive     PulmonicEgressive) = "ʡ"
-- Is the epiglottal plosive voiceless? The IPA chart does not specify.

constructIPA1 (Consonant Voiceless AlveoloPalatal Fricative   PulmonicEgressive) = "ɕ"
constructIPA1 (Consonant Voiced    AlveoloPalatal Fricative   PulmonicEgressive) = "ʑ"
constructIPA1 (Consonant Voiced    Alveolar       LateralFlap PulmonicEgressive) = "ɺ"

-- We cannot handle the ɧ (simultaneous ʃ and x) because
-- we did not define our data types to handle it yet.
-- constructIPA (simultaneous (analyzeIPA "ʃ") (analyzeIPA "x")) = "ɧ"

-- Other Consonants:
constructIPA1 (Consonant UnmarkedVocalFolds Bilabial       UnmarkedManner Click    ) = "ʘ"
constructIPA1 (Consonant UnmarkedVocalFolds Dental         UnmarkedManner Click    ) = "ǀ"
constructIPA1 (Consonant UnmarkedVocalFolds Alveolar       UnmarkedManner Click    ) = "ǃ" -- Or it could be PostAlveolar.
constructIPA1 (Consonant UnmarkedVocalFolds PalatoAlveolar UnmarkedManner Click    ) = "ǂ"
constructIPA1 (Consonant UnmarkedVocalFolds Alveolar       Lateral        Click    ) = "ǁ"
constructIPA1 (Consonant Voiced             Bilabial       UnmarkedManner Implosive) = "ɓ"
constructIPA1 (Consonant Voiced             Dental         UnmarkedManner Implosive) = "ɗ"  -- Or Alveolar
constructIPA1 (Consonant Voiced             Palatal        UnmarkedManner Implosive) = "ʄ"
constructIPA1 (Consonant Voiced             Velar          UnmarkedManner Implosive) = "ɠ"
constructIPA1 (Consonant Voiced             Uvular         UnmarkedManner Implosive) = "ʛ"



constructIPA1 c@(Consonant Voiced place manner PulmonicEgressive) = 
  constructUnaspiratedPulmonicEgressive c

constructIPA1 c@(Consonant VoicedAspirated place manner PulmonicEgressive) = 
  constructUnaspiratedPulmonicEgressive (deaspirate c) ++ "ʰ"


constructIPA1 c@(Consonant Voiceless place manner PulmonicEgressive) = 
  constructUnaspiratedPulmonicEgressive c

constructIPA1 c@(Consonant VoicelessAspirated place manner PulmonicEgressive) = 
  constructUnaspiratedPulmonicEgressive (deaspirate c) ++ "ʰ"

-- Close Vowels:
constructIPA1 (Vowel  Close Front   Unrounded Voiced) = "i"
constructIPA1 (Vowel  Close Front   Rounded   Voiced) = "y"
constructIPA1 (Vowel  Close Central Unrounded Voiced) = "ɨ"
constructIPA1 (Vowel  Close Central Rounded   Voiced) = "ʉ"
constructIPA1 (Vowel  Close Back    Unrounded Voiced) = "ɯ"
constructIPA1 (Vowel  Close Back    Rounded   Voiced) = "u"

-- Near-close Vowels:
constructIPA1 (Vowel NearClose Front Unrounded Voiced) = "ɪ"
constructIPA1 (Vowel NearClose Front Rounded   Voiced) = "ʏ"
constructIPA1 (Vowel NearClose Back  Rounded   Voiced) = "ʊ"

-- Close-mid Vowels:
constructIPA1 (Vowel  CloseMid Front   Unrounded Voiced) = "e"
constructIPA1 (Vowel  CloseMid Front   Rounded   Voiced) = "ø"
constructIPA1 (Vowel  CloseMid Central Unrounded Voiced) = "ɘ"
constructIPA1 (Vowel  CloseMid Central Rounded   Voiced) = "ɵ"
constructIPA1 (Vowel  CloseMid Back    Unrounded Voiced) = "ɤ"
constructIPA1 (Vowel  CloseMid Back    Rounded   Voiced) = "o"

-- Mid Vowels:
constructIPA1 (Vowel Mid Central UnmarkedRounding Voiced) = "ə"


-- Open-mid Vowels:
constructIPA1 (Vowel  OpenMid Front   Unrounded Voiced) = "ɛ"
constructIPA1 (Vowel  OpenMid Front   Rounded   Voiced) = "œ"
constructIPA1 (Vowel  OpenMid Central Unrounded Voiced) = "ɜ"
constructIPA1 (Vowel  OpenMid Central Rounded   Voiced) = "ɞ"
constructIPA1 (Vowel  OpenMid Back    Unrounded Voiced) = "ʌ"
constructIPA1 (Vowel  OpenMid Back    Rounded   Voiced) = "ɔ"

-- Near-open
constructIPA1 (Vowel  NearOpen Front   Unrounded Voiced) = "æ"
constructIPA1 (Vowel  NearOpen Central UnmarkedRounding  Voiced) = "ɐ"

-- Open Vowels:
constructIPA1 (Vowel  Open Front Unrounded Voiced) = "a"
constructIPA1 (Vowel  Open Front Rounded   Voiced) = "ɶ"
constructIPA1 (Vowel  Open Back  Unrounded Voiced) = "ɑ"
constructIPA1 (Vowel  Open Back  Rounded   Voiced) = "ɒ"
constructIPA1 _ = " "




constructIPA2 :: Phonet -> IPAText

constructIPA2 (Consonant  x PostAlveolar y z) =
    constructIPA1 (Consonant x Alveolar y z) ++ "̠"  -- Add the diacritic for "retracted"


-- If there isn't a symbol, and the consonant we want is voiceless,
-- Just take the symbol for a voiced consonant,
-- and then put that diacritic that means voiceless after.
-- (The following two definitions are intended to implement that)
-- Add the small circle diacritic to consonants to make them voiceless.
constructIPA2 (Consonant Voiceless x y z) =
  constructIPA1 (Consonant Voiced x y z) ++ "̥" -- add diacritic for voiceless

-- Add the small circle diacritic to vowels to make them voiceless.
constructIPA2 (Vowel x y z Voiceless) =
    constructIPA1 (Vowel x y z Voiced) ++ "̥"

-- If there is no way to express a voiced consonant in a single
-- grapheme add a diacritic to the grapheme that represents
-- the voiceless counterpart.
constructIPA2 (Consonant Voiced x y z) =
   constructIPA1 (Consonant Voiceless x y z) ++ "̬"

constructIPA2 (Vowel x y z Voiced) =
  constructIPA1 (Vowel x y z Voiceless) ++ "̬"






constructIPA2 _ = "∅" -- This return value ( a symbol representing the empty set)
-- is not a full answer. It really means we don't have an answer.
-- We are only using it here so that we can ignore values we have not programmed
-- yet. We just want it to show that we do not have it.


constructUnaspiratedPulmonicEgressive (Consonant voicing place manner PulmonicEgressive) =
   let rowIndex = mannerToRowIndex manner
       colIndex = voicingAndPlaceToColIndex voicing place
   in  [(consonantsPulmonicTable !! rowIndex) !! colIndex]
   
constructUnaspiratedPulmonicEgressive _ = ""  -- Kind of senseless for non-consonants, so just do nothing.

deaspirate (Consonant VoicedAspirated place manner airstream) =
  (Consonant Voiced place manner airstream)

deaspirate (Consonant VoicelessAspirated place manner airstream) =
  (Consonant Voiceless place manner airstream)
  
deaspirate x = x


voicedIPA :: IPAText -> IPAText
voicedIPA = constructIPA . voicedPhonet . analyzeIPA

devoicedIPA :: IPAText -> IPAText
devoicedIPA = constructIPA . devoicedPhonet . analyzeIPA


spirantizedIPA :: IPAText -> IPAText
spirantizedIPA = constructIPA . spirantizedPhonet . analyzeIPA

{-|
Return an english description of a phoneme,
given a phoneme's representation in the
internatinal phonetica alphabet.
  |-}
describeIPA :: IPAText -> String
describeIPA = show . analyzeIPA