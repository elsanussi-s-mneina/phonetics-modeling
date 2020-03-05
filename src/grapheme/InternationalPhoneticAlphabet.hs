module InternationalPhoneticAlphabet where

import Lib

type IPAText = String
  -- For storing text meant to be interpreted as International phonetic alphabet


graphemesOfIPA :: [Char]
graphemesOfIPA = consonantsPulmonic 
  ++ consonantsNonPulmonic
  ++ otherSymbols
  ++ vowels
  ++ suprasegmentals 
  ++ toneAndWordAccents
  ++ diacriticsAndSupersegmentals
-- See: https://www.internationalphoneticassociation.org/sites/default/files/IPA_Kiel_2015.pdf
-- For the source of this information..

-- CONSONANTS (PULMONIC)
consonantsPulmonic :: [Char]
consonantsPulmonic = 
  [ 'p', 'b',                     't', 'd',           'ʈ', 'ɖ', 'c', 'ɟ', 'k', 'g', 'q', 'ɢ', 'ʔ'                -- Plosive
  ,      'm',      'ɱ',                'n',                'ɳ',      'ɲ',      'ɴ'                               -- Nasal
  ,      'ʙ',                          'r',                          'ʀ'                                         -- Trill
  ,                'ⱱ',                'ɾ',                'ɽ'                                                   -- Tap or Flap
  , 'ɸ', 'β', 'f', 'v', 'θ', 'ð', 's', 'z', 'ʃ', 'ʒ', 'ʂ', 'ʐ', 'ç', 'ʝ', 'x', 'ɣ', 'χ', 'ʁ', 'ħ', 'ʕ', 'h', 'ɦ'   -- Fricative
  ,                     'ɬ', 'ɮ'                                                                                 -- Lateral fricative
  ,                'ʋ',      'ɹ',      'ɻ',     'j',       'ɰ'                                                   -- Approximant
  ,                          'l',      'ɭ',     'ʎ',                     'ʟ'                                     -- Lateral approximant
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

diacriticsAndSupersegmentals :: [Char]
diacriticsAndSupersegmentals = 
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


-- | This function will allow us to convert an IPA symbol
-- | to its analyzed form (its phonetic features)
-- Currently, only the consonants (pulmonic) in the 2005 IPA chart are included.
analyzeIPA  :: IPAText -> Phonet
-- Plosives:
analyzeIPA "p"  = Consonant  Voiceless Bilabial  Plosive PulmonicEgressive
analyzeIPA "b"  = Consonant  Voiced    Bilabial  Plosive PulmonicEgressive
analyzeIPA "t"  = Consonant  Voiceless Alveolar  Plosive PulmonicEgressive
analyzeIPA "d"  = Consonant  Voiced    Alveolar  Plosive PulmonicEgressive
analyzeIPA "ʈ"  = Consonant  Voiceless Retroflex Plosive PulmonicEgressive
analyzeIPA "ɖ"  = Consonant  Voiced    Retroflex Plosive PulmonicEgressive
analyzeIPA "c"  = Consonant  Voiceless Palatal   Plosive PulmonicEgressive
analyzeIPA "ɟ"  = Consonant  Voiced    Palatal   Plosive PulmonicEgressive
analyzeIPA "k"  = Consonant  Voiceless Velar     Plosive PulmonicEgressive
analyzeIPA "g"  = Consonant  Voiced    Velar     Plosive PulmonicEgressive
analyzeIPA "q"  = Consonant  Voiceless Uvular    Plosive PulmonicEgressive
analyzeIPA "ɢ"  = Consonant  Voiced    Uvular    Plosive PulmonicEgressive
analyzeIPA "ʔ"  = Consonant  Voiceless Glottal   Plosive PulmonicEgressive

-- Nasals:
analyzeIPA "m"  = Consonant  Voiced Bilabial    Nasal PulmonicEgressive
analyzeIPA "ɱ"  = Consonant  Voiced LabioDental Nasal PulmonicEgressive
analyzeIPA "n"  = Consonant  Voiced Alveolar    Nasal PulmonicEgressive
analyzeIPA "ɳ"  = Consonant  Voiced Retroflex   Nasal PulmonicEgressive
analyzeIPA "ɲ"  = Consonant  Voiced Palatal     Nasal PulmonicEgressive
analyzeIPA "ŋ"  = Consonant  Voiced Velar       Nasal PulmonicEgressive
analyzeIPA "ɴ"  = Consonant  Voiced Uvular      Nasal PulmonicEgressive

-- Trills:
analyzeIPA "ʙ"  = Consonant  Voiced Bilabial Trill PulmonicEgressive
analyzeIPA "r"  = Consonant  Voiced Alveolar Trill PulmonicEgressive
analyzeIPA "ʀ"  = Consonant  Voiced Uvular   Trill PulmonicEgressive

-- Taps or flaps:
analyzeIPA "ⱱ"  = Consonant  Voiced LabioDental TapOrFlap PulmonicEgressive
analyzeIPA "ɾ"  = Consonant  Voiced Alveolar    TapOrFlap PulmonicEgressive
analyzeIPA "ɽ"  = Consonant  Voiced Retroflex   TapOrFlap PulmonicEgressive

-- Fricatives:
analyzeIPA "ɸ"  = Consonant  Voiceless Bilabial     Fricative PulmonicEgressive
analyzeIPA "β"  = Consonant  Voiced    Bilabial     Fricative PulmonicEgressive
analyzeIPA "f"  = Consonant  Voiceless LabioDental  Fricative PulmonicEgressive
analyzeIPA "v"  = Consonant  Voiced    LabioDental  Fricative PulmonicEgressive
analyzeIPA "θ"  = Consonant  Voiceless Dental       Fricative PulmonicEgressive
analyzeIPA "ð"  = Consonant  Voiced    Dental       Fricative PulmonicEgressive
analyzeIPA "s"  = Consonant  Voiceless Alveolar     Fricative PulmonicEgressive
analyzeIPA "z"  = Consonant  Voiced    Alveolar     Fricative PulmonicEgressive
analyzeIPA "ʃ"  = Consonant  Voiceless PostAlveolar Fricative PulmonicEgressive
analyzeIPA "ʒ"  = Consonant  Voiced    PostAlveolar Fricative PulmonicEgressive
analyzeIPA "ʂ"  = Consonant  Voiceless Retroflex    Fricative PulmonicEgressive
analyzeIPA "ʐ"  = Consonant  Voiced    Retroflex    Fricative PulmonicEgressive
analyzeIPA "ç"  = Consonant  Voiceless Palatal      Fricative PulmonicEgressive
analyzeIPA "ʝ"  = Consonant  Voiced    Palatal      Fricative PulmonicEgressive
analyzeIPA "x"  = Consonant  Voiceless Velar        Fricative PulmonicEgressive
analyzeIPA "ɣ"  = Consonant  Voiced    Velar        Fricative PulmonicEgressive
analyzeIPA "χ"  = Consonant  Voiceless Uvular       Fricative PulmonicEgressive
analyzeIPA "ʁ"  = Consonant  Voiced    Uvular       Fricative PulmonicEgressive
analyzeIPA "ħ"  = Consonant  Voiceless Pharyngeal   Fricative PulmonicEgressive
analyzeIPA "ʕ"  = Consonant  Voiced    Pharyngeal   Fricative PulmonicEgressive
analyzeIPA "h"  = Consonant  Voiceless Glottal      Fricative PulmonicEgressive
analyzeIPA "ɦ"  = Consonant  Voiced    Glottal      Fricative PulmonicEgressive

-- Affricates
analyzeIPA "t͡ʃ" = Consonant  Voiceless PostAlveolar Affricate PulmonicEgressive
analyzeIPA "d͡ʒ" = Consonant  Voiced    PostAlveolar Affricate PulmonicEgressive
-- We should probably enforce use of the tie-bar underneath, otherwise
-- it would not be deterministic to determine whether two graphemes here
-- represent affricates or a plosive followed by a fricative.

-- Lateral Fricatives:
analyzeIPA "ɬ" = Consonant  Voiceless Alveolar LateralFricative PulmonicEgressive
analyzeIPA "ɮ" = Consonant  Voiced    Alveolar LateralFricative PulmonicEgressive


-- Approximants:
analyzeIPA "ʋ"  = Consonant  Voiced LabioDental  Approximant PulmonicEgressive
analyzeIPA "ɹ"  = Consonant  Voiced PostAlveolar Approximant PulmonicEgressive
analyzeIPA "ɻ"  = Consonant  Voiced Retroflex    Approximant PulmonicEgressive
analyzeIPA "j"  = Consonant  Voiced Palatal      Approximant PulmonicEgressive
analyzeIPA "ɰ"  = Consonant  Voiced Velar        Approximant PulmonicEgressive

-- Lateral Approximants:
analyzeIPA "l"  = Consonant  Voiced Alveolar  LateralApproximant PulmonicEgressive
analyzeIPA "ɭ"  = Consonant  Voiced Retroflex LateralApproximant PulmonicEgressive
analyzeIPA "ʎ"  = Consonant  Voiced Palatal   LateralApproximant PulmonicEgressive
analyzeIPA "ʟ"  = Consonant  Voiced Velar     LateralApproximant PulmonicEgressive


-- Under the Other Symbols part of the IPA chart:

analyzeIPA "w" = Consonant Voiced    LabialVelar Approximant PulmonicEgressive

analyzeIPA "ʍ" = Consonant Voiceless LabialVelar   Fricative   PulmonicEgressive
analyzeIPA "ɥ" = Consonant Voiced    LabialPalatal Approximant PulmonicEgressive
analyzeIPA "ʜ" = Consonant Voiceless Epiglottal    Fricative   PulmonicEgressive
analyzeIPA "ʢ" = Consonant Voiced    Epiglottal    Fricative   PulmonicEgressive
analyzeIPA "ʡ" = Consonant Voiceless Epiglottal    Plosive    PulmonicEgressive
-- Is the epiglottal plosive voiceless? The IPA chart does not specify.

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


-- Handle Diacritics:
analyzeIPA [firstChar, '̥'] =
  let fullGrapheme = analyzeIPA [firstChar]
  in case fullGrapheme of
          Consonant _ place manner airstream    -> Consonant Voiceless place manner airstream
          Vowel height backness rounding _      -> Vowel height backness rounding Voiceless

analyzeIPA [firstChar, '̼'] =
  let fullGrapheme = analyzeIPA [firstChar]
  in case fullGrapheme of
          Consonant _ place manner airstream    -> Consonant Voiced place manner airstream
          Vowel height backness rounding _      -> Vowel height backness rounding Voiced


constructIPA :: Phonet -> IPAText
constructIPA phoneDescription =
  -- If it can represent it as a single character it will
  -- return the single character result (i.e. without diacritics),
  -- otherwise
  -- it will try to represent it in IPA with more than
  -- one character
  let simpleResult = constructIPA1 phoneDescription
  in if simpleResult == "∅"
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
-- Plosives:
constructIPA1 (Consonant  Voiced    Bilabial  Plosive PulmonicEgressive) = "b"
constructIPA1 (Consonant  Voiceless Bilabial  Plosive PulmonicEgressive) = "p"
constructIPA1 (Consonant  Voiceless Alveolar  Plosive PulmonicEgressive) = "t"
constructIPA1 (Consonant  Voiced    Alveolar  Plosive PulmonicEgressive) = "d"
constructIPA1 (Consonant  Voiceless Retroflex Plosive PulmonicEgressive) = "ʈ"
constructIPA1 (Consonant  Voiced    Retroflex Plosive PulmonicEgressive) = "ɖ"
constructIPA1 (Consonant  Voiceless Palatal   Plosive PulmonicEgressive) = "c"
constructIPA1 (Consonant  Voiced    Palatal   Plosive PulmonicEgressive) = "ɟ"
constructIPA1 (Consonant  Voiceless Velar     Plosive PulmonicEgressive) = "k"
constructIPA1 (Consonant  Voiced    Velar     Plosive PulmonicEgressive) = "g"
constructIPA1 (Consonant  Voiceless Uvular    Plosive PulmonicEgressive) = "q"
constructIPA1 (Consonant  Voiced    Uvular    Plosive PulmonicEgressive) = "ɢ"
constructIPA1 (Consonant  Voiceless Glottal   Plosive PulmonicEgressive) = "ʔ"

-- Nasals:
constructIPA1 (Consonant  Voiced    Bilabial    Nasal PulmonicEgressive) = "m"
constructIPA1 (Consonant  Voiced    LabioDental Nasal PulmonicEgressive) = "ɱ"
constructIPA1 (Consonant  Voiced    Alveolar    Nasal PulmonicEgressive) = "n"
constructIPA1 (Consonant  Voiced    Retroflex   Nasal PulmonicEgressive) = "ɳ"
constructIPA1 (Consonant  Voiced    Palatal     Nasal PulmonicEgressive) = "ɲ"
constructIPA1 (Consonant  Voiced    Velar       Nasal PulmonicEgressive) = "ŋ"
constructIPA1 (Consonant  Voiced    Uvular      Nasal PulmonicEgressive) = "ɴ"

-- Trills:
constructIPA1 (Consonant  Voiced   Bilabial    Trill     PulmonicEgressive) = "ʙ"
constructIPA1 (Consonant  Voiced   Alveolar    Trill     PulmonicEgressive) = "r"
constructIPA1 (Consonant  Voiced   Uvular      Trill     PulmonicEgressive) = "ʀ" -- Taps or flaps:
constructIPA1 (Consonant  Voiced   LabioDental TapOrFlap PulmonicEgressive) = "ⱱ"
constructIPA1 (Consonant  Voiced   Alveolar    TapOrFlap PulmonicEgressive) = "ɾ"
constructIPA1 (Consonant  Voiced   Retroflex   TapOrFlap PulmonicEgressive) = "ɽ"

-- Fricatives:
constructIPA1 (Consonant  Voiceless Bilabial       Fricative  PulmonicEgressive) = "ɸ"
constructIPA1 (Consonant  Voiced    Bilabial       Fricative  PulmonicEgressive) = "β"
constructIPA1 (Consonant  Voiceless LabioDental    Fricative  PulmonicEgressive) = "f"
constructIPA1 (Consonant  Voiced    LabioDental    Fricative  PulmonicEgressive) = "v"
constructIPA1 (Consonant  Voiceless Dental         Fricative  PulmonicEgressive) = "θ"
constructIPA1 (Consonant  Voiced    Dental         Fricative  PulmonicEgressive) = "ð"
constructIPA1 (Consonant  Voiceless Alveolar       Fricative  PulmonicEgressive) = "s"
constructIPA1 (Consonant  Voiced    Alveolar       Fricative  PulmonicEgressive) = "z"
constructIPA1 (Consonant  Voiceless PostAlveolar   Fricative  PulmonicEgressive) = "ʃ"
constructIPA1 (Consonant  Voiced    PostAlveolar   Fricative  PulmonicEgressive) = "ʒ"
constructIPA1 (Consonant  Voiceless Retroflex      Fricative  PulmonicEgressive) = "ʂ"
constructIPA1 (Consonant  Voiced    Retroflex      Fricative  PulmonicEgressive) = "ʐ"
constructIPA1 (Consonant  Voiceless Palatal        Fricative  PulmonicEgressive) = "ç"
constructIPA1 (Consonant  Voiced    Palatal        Fricative  PulmonicEgressive) = "ʝ"
constructIPA1 (Consonant  Voiceless Velar          Fricative  PulmonicEgressive) = "x"
constructIPA1 (Consonant  Voiced    Velar          Fricative  PulmonicEgressive) = "ɣ"
constructIPA1 (Consonant  Voiceless Uvular         Fricative  PulmonicEgressive) = "χ"
constructIPA1 (Consonant  Voiced    Uvular         Fricative  PulmonicEgressive) = "ʁ"
constructIPA1 (Consonant  Voiceless Pharyngeal     Fricative  PulmonicEgressive) = "ħ"
constructIPA1 (Consonant  Voiced    Pharyngeal     Fricative  PulmonicEgressive) = "ʕ"
constructIPA1 (Consonant  Voiceless Glottal        Fricative  PulmonicEgressive) = "h"
constructIPA1 (Consonant  Voiced    Glottal        Fricative  PulmonicEgressive) = "ɦ"



-- Lateral Fricatives:
constructIPA1 (Consonant  Voiceless Alveolar LateralFricative PulmonicEgressive) = "ɬ"
constructIPA1 (Consonant  Voiced    Alveolar LateralFricative PulmonicEgressive) = "ɮ"


-- Approximants:
constructIPA1 (Consonant  Voiced  LabioDental  Approximant PulmonicEgressive) = "ʋ"
constructIPA1 (Consonant  Voiced  Alveolar     Approximant PulmonicEgressive) = "ɹ"

constructIPA1 (Consonant  Voiced  Retroflex    Approximant PulmonicEgressive) = "ɻ"
constructIPA1 (Consonant  Voiced  Palatal      Approximant PulmonicEgressive) = "j"
constructIPA1 (Consonant  Voiced  Velar        Approximant PulmonicEgressive) = "ɰ"

-- Lateral Approximants:
constructIPA1 (Consonant  Voiced Alveolar   LateralApproximant PulmonicEgressive) = "l"
constructIPA1 (Consonant  Voiced Retroflex  LateralApproximant PulmonicEgressive) = "ɭ"
constructIPA1 (Consonant  Voiced Palatal    LateralApproximant PulmonicEgressive) = "ʎ"
constructIPA1 (Consonant  Voiced Velar      LateralApproximant PulmonicEgressive) = "ʟ"


-- Under the Other Symbols part of the IPA chart:

constructIPA1 (Consonant  Voiced LabialVelar Approximant PulmonicEgressive) = "w"
constructIPA1 (Consonant Voiceless LabialVelar Fricative PulmonicEgressive) = "ʍ"
constructIPA1 (Consonant Voiced LabialPalatal Approximant PulmonicEgressive) = "ɥ"
constructIPA1 (Consonant Voiceless Epiglottal Fricative PulmonicEgressive) = "ʜ"
constructIPA1 (Consonant Voiced Epiglottal Fricative PulmonicEgressive) = "ʢ"
constructIPA1 (Consonant Voiceless Epiglottal Plosive PulmonicEgressive) = "ʡ"
-- Is the epiglottal plosive voiceless? The IPA chart does not specify.

constructIPA1 (Consonant Voiceless AlveoloPalatal Fricative   PulmonicEgressive) = "ɕ"
constructIPA1 (Consonant Voiced    AlveoloPalatal Fricative   PulmonicEgressive) = "ʑ"
constructIPA1 (Consonant Voiced    Alveolar       LateralFlap PulmonicEgressive) = "ɺ"

-- We cannot handle the ɧ (simultaneous ʃ and x) because
-- we did not define our data types to handle it yet.
-- constructIPA (simultaneous (analyzeIPA "ʃ") (analyzeIPA "x")) = "ɧ"

-- Other Consonants:
constructIPA1 (Consonant UnmarkedVocalFolds Bilabial UnmarkedManner Click) = "ʘ"
constructIPA1 (Consonant UnmarkedVocalFolds Dental UnmarkedManner  Click) = "ǀ"
constructIPA1 (Consonant UnmarkedVocalFolds Alveolar UnmarkedManner Click) = "ǃ" -- Or it could be PostAlveolar.
constructIPA1 (Consonant UnmarkedVocalFolds PalatoAlveolar UnmarkedManner Click) = "ǂ"
constructIPA1 (Consonant UnmarkedVocalFolds Alveolar Lateral Click) = "ǁ"
constructIPA1 (Consonant Voiced Bilabial UnmarkedManner Implosive) = "ɓ"
constructIPA1 (Consonant Voiced Dental UnmarkedManner Implosive) = "ɗ"  -- Or Alveolar
constructIPA1 (Consonant Voiced Palatal UnmarkedManner Implosive) = "ʄ"
constructIPA1 (Consonant Voiced Velar UnmarkedManner Implosive) = "ɠ"
constructIPA1 (Consonant Voiced Uvular UnmarkedManner Implosive) = "ʛ"


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
constructIPA1 _ = "∅"


constructIPA2 :: Phonet -> IPAText
-- Affricates
constructIPA2 (Consonant  Voiceless PostAlveolar  Affricate PulmonicEgressive) = "t͡ʃ"
constructIPA2 (Consonant  Voiced    PostAlveolar  Affricate PulmonicEgressive) = "d͡ʒ"
constructIPA2 (Consonant  Voiceless Bilabial Affricate PulmonicEgressive) = "p͡ɸ"
-- constructIPA2 (Consonant  Voiceless LabialVelar? Affricate PulmonicEgressive) = "k͡p"
constructIPA2 (Consonant  Voiceless Alveolar Affricate PulmonicEgressive) = "t͜s"
constructIPA2 (Consonant  Voiced Alveolar Affricate PulmonicEgressive) = "d͡z"
constructIPA2 (Consonant  Voiceless Velar Affricate PulmonicEgressive) = "k͡x"
-- constructIPA2 (Consonant  Voiceless Palatal (or AlveolaPalatal?) Affricate PulmonicEgressive) = "c͡ɕ"
constructIPA2 (Consonant Voiceless Uvular Affricate PulmonicEgressive) = "q͡χ"

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


voicedIPA :: IPAText -> IPAText
voicedIPA = constructIPA . voicedPhonet . analyzeIPA

devoicedIPA :: IPAText -> IPAText
devoicedIPA = constructIPA . devoicedPhonet . analyzeIPA


spirantizedIPA :: IPAText -> IPAText
spirantizedIPA = constructIPA . spirantizedPhonet . analyzeIPA
