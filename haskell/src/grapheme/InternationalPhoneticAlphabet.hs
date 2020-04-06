module InternationalPhoneticAlphabet (IPAText, describeIPA, constructIPA, analyzeIPA, voicedIPA, devoicedIPA, showIPA, spirantizedIPA,
  diacriticsAndSuprasegmentals, toneAndWordAccents, suprasegmentals, vowels, otherSymbols, consonantsNonPulmonic, consonantsPulmonic, graphemesOfIPA) where

import Lib (Phonet(Consonant, Vowel), VocalFolds(Voiced, Voiceless, VoicelessAspirated, VoicedAspirated, CreakyVoiced, UnmarkedVocalFolds), 


            Place(
                Bilabial
              , LabioDental
              , Dental
              , Alveolar
              , PostAlveolar
              , Retroflex
              , Palatal
              , Velar
              , Uvular
              , Pharyngeal
              , Glottal
              , Epiglottal
              , LabialVelar
              , LabialPalatal
              , AlveoloPalatal
              , PalatoAlveolar 
              , Places
              , UnmarkedPlace),
              Manner(
                Plosive
              , Nasal
              , Trill
              , TapOrFlap
              , Approximant
              , Fricative
              , Affricate
              , LateralFricative
              , LateralApproximant
              , LateralFlap
              , Lateral 
              , UnmarkedManner), Airstream(PulmonicEgressive, Click, Implosive, UnmarkedAirstream),
               Height(Close, NearClose, CloseMid, Mid, OpenMid, NearOpen, Open) -- , -- UnmarkedHeight), 
              , Backness(Front, Central, Back), Rounding(Rounded, Unrounded, UnmarkedRounding), PhonetInventory(PhonetInventory), spirantizedPhonet, devoicedPhonet,
            voicedPhonet
            )

import Prelude
  (
    Eq, Int, String,
    concat, concatMap, init, last, show,
    (.), (+), (*), (!!), (++), (==)
  )

type IPAText = String
-- For storing text meant to be interpreted as International phonetic alphabet




graphemesOfIPA :: [IPAText]
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
consonantsPulmonic :: [IPAText]
consonantsPulmonic = concat consonantsPulmonicTable

plosivePulmonic :: [String]
plosivePulmonic            = [ "p", "b", " ", " ", " ", " ", "t", "d", " ", " ", "ʈ", "ɖ", "c", "ɟ", "k", "g", "q", "ɢ", " ", " ", "ʔ", " "] -- Plosive

nasalPulmonic :: [String]
nasalPulmonic              = [ " ", "m", " ", "ɱ", " ", " ", " ", "n", " ", " ", " ", "ɳ", " ", "ɲ", " ", "ŋ", " ", "ɴ", " ", " ", " ", " "] -- Nasal

trillPulmonic :: [String]
trillPulmonic              = [ " ", "ʙ", " ", " ", " ", " ", " ", "r", " ", " ", " ", " ", " ", " ", " ", " ", " ", "ʀ", " ", " ", " ", " "] -- Trill

tapOrFlapPulmonic :: [String]
tapOrFlapPulmonic          = [ " ", " ", " ", "ⱱ", " ", " ", " ", "ɾ", " ", " ", " ", "ɽ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "] -- Tap or Flap

fricativePulmonic :: [String]
fricativePulmonic          = [ "ɸ", "β", "f", "v", "θ", "ð", "s", "z", "ʃ", "ʒ", "ʂ", "ʐ", "ç", "ʝ", "x", "ɣ", "χ", "ʁ", "ħ", "ʕ", "h", "ɦ"]  -- Fricative

lateralFricativePulmonic :: [String]
lateralFricativePulmonic   = [ " ", " ", " ", " ", " ", " ", "ɬ", "ɮ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "] -- Lateral fricative

approximantPulmonic :: [String]
approximantPulmonic        = [ " ", " ", " ", "ʋ", " ", " ", " ", "ɹ", " ", " ", " ", "ɻ", " ", "j", " ", "ɰ", " ", " ", " ", " ", " ", " "] -- Approximant

lateralApproximantPulmonic :: [String]
lateralApproximantPulmonic = [ " ", " ", " ", " ", " ", " ", " ", "l", " ", " ", " ", "ɭ", " ", "ʎ", " ", "ʟ", " ", " ", " ", " ", " ", " "] -- Lateral approximant




consonantsPulmonicTable :: [[IPAText]]
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


consonantsNonPulmonic :: [IPAText]
consonantsNonPulmonic =
-- Clicks   Voiced implosives
 [ "ʘ",     "ɓ" -- Bilabial
 , "ǀ", {- Dental -}    "ɗ" -- Dental/alveolar
 , "ǃ", {-  (Post)alveolar -}  "ʄ"
 , "ǂ",  "ɠ"
 , "ǁ",  "ʛ"
 ]

otherSymbols :: [IPAText]
otherSymbols =
  ["ʍ",  "ɕ"
  ,"w",  "ʑ"
  ,"ɥ",  "ɺ"
  ,"ʜ",  "ɧ"
  ,"ʢ"
  ,"ʡ"
  ]

vowels :: [IPAText]
vowels =
  ["i", "y",   "ɨ", "ʉ",   "ɯ", "u"   -- Close
  ,"ɪ", "ʏ",            "ʊ"
  ,"e", "ø",   "ɘ", "ɵ",   "ɤ", "o"   -- Close-mid
  ,               "ə"
  ,"ɛ", "œ",   "ɜ", "ɞ",   "ʌ", "ɔ"   -- Open-mid
  , "æ",           "ɐ"
  , "a", "ɶ",              "ɑ", "ɒ"  -- Open
  ]

suprasegmentals :: [IPAText]
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


toneAndWordAccents :: [IPAText]
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

diacriticsAndSuprasegmentals :: [IPAText]
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
  , "̆"   -- Extra short
  , "̇"    -- Palatalization/Centralization
  ]

showIPA :: PhonetInventory -> IPAText
showIPA (PhonetInventory phonetes) = concatMap constructIPA phonetes




indexOf :: (Eq a) => [a] -> Int -> a -> Int
indexOf [] _ _ = -1
indexOf (element:rest) index target =
  if element == target
    then index
    else indexOf rest (index + 1) target


placeToHalfColIndex :: Place -> Int
placeToHalfColIndex place1 =
  let colNames = [Bilabial, LabioDental, Dental, Alveolar, PostAlveolar, Retroflex, Palatal, Velar, Uvular, Pharyngeal, Glottal]
  in indexOf colNames 0 place1


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


-- | This function will allow us to convert an IPA symbol
-- | to its analyzed form (its phonetic features)
-- Currently, only the consonants (pulmonic) in the 2005 IPA chart are included.
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


-- Lateral Fricatives:
analyzeIPA "ɬ" = Consonant  Voiceless Alveolar LateralFricative PulmonicEgressive
analyzeIPA "ɮ" = Consonant  Voiced    Alveolar LateralFricative PulmonicEgressive


-- Approximants:
analyzeIPA "ʋ"  = Consonant  Voiced LabioDental  Approximant PulmonicEgressive
analyzeIPA "ɹ"  = Consonant  Voiced Alveolar     Approximant PulmonicEgressive
analyzeIPA "ɻ"  = Consonant  Voiced Retroflex    Approximant PulmonicEgressive
analyzeIPA "j"  = Consonant  Voiced Palatal      Approximant PulmonicEgressive
analyzeIPA "ɰ"  = Consonant  Voiced Velar        Approximant PulmonicEgressive

-- Lateral Approximants:
analyzeIPA "l"  = Consonant  Voiced Alveolar  LateralApproximant PulmonicEgressive
analyzeIPA "ɭ"  = Consonant  Voiced Retroflex LateralApproximant PulmonicEgressive
analyzeIPA "ʎ"  = Consonant  Voiced Palatal   LateralApproximant PulmonicEgressive
analyzeIPA "ʟ"  = Consonant  Voiced Velar     LateralApproximant PulmonicEgressive



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

analyzeIPA "ɧ" = Consonant Voiceless (Places [PostAlveolar, Velar]) Fricative PulmonicEgressive

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
analyzeIPA ipaText =
  case [last ipaText] of
    "̥" ->
      let fullGrapheme = analyzeIPA (init ipaText)
      in case fullGrapheme of
              Consonant _ place manner airstream    -> Consonant Voiceless place manner airstream
              Vowel height backness rounding _      -> Vowel height backness rounding Voiceless
    "̬" ->
      let fullGrapheme = analyzeIPA (init ipaText)
      in case fullGrapheme of
              Consonant _ place manner airstream    -> Consonant Voiced place manner airstream
              Vowel height backness rounding _      -> Vowel height backness rounding Voiced

    "ʰ" ->
      let fullGrapheme = analyzeIPA (init ipaText)
      in case fullGrapheme of
              Consonant Voiced place manner airstream    -> Consonant VoicedAspirated place manner airstream
              Consonant Voiceless place manner airstream -> Consonant VoicelessAspirated place manner airstream
              Vowel height backness rounding voice       -> Vowel height backness rounding voice
              anythingElse                               -> anythingElse
              -- (About the preceding line:) It is strange but we will just do nothing if they give us an aspirated vowel.
              -- since we have no way to represent it in the type system. to do: determine
              -- if the idea of an aspirated vowel makes sense
    _ -> Consonant UnmarkedVocalFolds UnmarkedPlace UnmarkedManner UnmarkedAirstream -- Not recognized.

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

constructIPA1 (Consonant Voiceless (Places [PostAlveolar, Velar]) Fricative PulmonicEgressive) = "ɧ"

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



constructIPA1 c@(Consonant Voiced _ _ PulmonicEgressive) =
  constructUnaspiratedPulmonicEgressive c

constructIPA1 c@(Consonant VoicedAspirated _ _ PulmonicEgressive) =
  constructUnaspiratedPulmonicEgressive (deaspirate c) ++ "ʰ"


constructIPA1 c@(Consonant Voiceless _ _ PulmonicEgressive) =
  constructUnaspiratedPulmonicEgressive c

constructIPA1 c@(Consonant VoicelessAspirated _ _ PulmonicEgressive) =
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

constructUnaspiratedPulmonicEgressive :: Phonet -> IPAText




constructUnaspiratedPulmonicEgressive (Consonant voicing place manner PulmonicEgressive) =
   let rowIndex = mannerToRowIndex manner
       colIndex = voicingAndPlaceToColIndex voicing place
   in  (consonantsPulmonicTable !! rowIndex) !! colIndex

constructUnaspiratedPulmonicEgressive _ = ""  -- Kind of senseless for non-consonants, so just do nothing.

deaspirate :: Phonet -> Phonet
deaspirate (Consonant VoicedAspirated place manner airstream) =
  (Consonant Voiced place manner airstream)

deaspirate (Consonant VoicelessAspirated place1 manner1 airstream1) =
  (Consonant Voiceless place1 manner1 airstream1)

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
international phonetic alphabet.
  |-}
describeIPA :: IPAText -> String
describeIPA = show . analyzeIPA
