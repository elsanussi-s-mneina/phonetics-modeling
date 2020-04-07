module InternationalPhoneticAlphabet (IPAText, describeIPA, constructIPA, analyzeIPA, voicedIPA, devoicedIPA, showIPA, spirantizedIPA,
  diacriticsAndSuprasegmentals, toneAndWordAccents, suprasegmentals, vowels, otherSymbols, consonantsNonPulmonic, consonantsPulmonic, graphemesOfIPA) where

import Lib (Phonet(Consonant, Vowel), VocalFolds(Voiced, Voiceless, VoicelessAspirated, VoicedAspirated, CreakyVoiced), 
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
              ),
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
              ), Airstream(PulmonicEgressive, Click, Implosive)
              , Height(Close, NearClose, CloseMid, Mid, OpenMid, NearOpen, Open)
              , Backness(Front, Central, Back), Rounding(Rounded, Unrounded), PhonetInventory(PhonetInventory), spirantizedPhonet, devoicedPhonet,
            voicedPhonet
            )

import Prelude
  (
    Int, Maybe(Just, Nothing), String,
    concat, concatMap, init, last, show,
    (.), (<), ($), (+), (++), (==)
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
  , "̇"    -- Palatalization/Centralization
  ]

showIPA :: PhonetInventory -> IPAText
showIPA (PhonetInventory phonetes) = concatMap constructIPA phonetes



-- | This function will allow us to convert an IPA symbol
-- | to its analyzed form (its phonetic features)
-- Currently, only the consonants (pulmonic) in the 2005 IPA chart are included.
analyzeIPA  :: IPAText -> Maybe Phonet


-- | This function will allow us to convert an IPA symbol
-- | to its analyzed form (its phonetic features)
-- Currently, only the consonants (pulmonic) in the 2005 IPA chart are included.
-- Plosives:
analyzeIPA "p"  = Just $ Consonant  Voiceless Bilabial  Plosive PulmonicEgressive
analyzeIPA "b"  = Just $ Consonant  Voiced    Bilabial  Plosive PulmonicEgressive
analyzeIPA "t"  = Just $ Consonant  Voiceless Alveolar  Plosive PulmonicEgressive
analyzeIPA "d"  = Just $ Consonant  Voiced    Alveolar  Plosive PulmonicEgressive
analyzeIPA "ʈ"  = Just $ Consonant  Voiceless Retroflex Plosive PulmonicEgressive
analyzeIPA "ɖ"  = Just $ Consonant  Voiced    Retroflex Plosive PulmonicEgressive
analyzeIPA "c"  = Just $ Consonant  Voiceless Palatal   Plosive PulmonicEgressive
analyzeIPA "ɟ"  = Just $ Consonant  Voiced    Palatal   Plosive PulmonicEgressive
analyzeIPA "k"  = Just $ Consonant  Voiceless Velar     Plosive PulmonicEgressive
analyzeIPA "g"  = Just $ Consonant  Voiced    Velar     Plosive PulmonicEgressive
analyzeIPA "q"  = Just $ Consonant  Voiceless Uvular    Plosive PulmonicEgressive
analyzeIPA "ɢ"  = Just $ Consonant  Voiced    Uvular    Plosive PulmonicEgressive
analyzeIPA "ʔ"  = Just $ Consonant  Voiceless Glottal   Plosive PulmonicEgressive

-- Nasals:
analyzeIPA "m"  = Just $ Consonant  Voiced Bilabial    Nasal PulmonicEgressive
analyzeIPA "ɱ"  = Just $ Consonant  Voiced LabioDental Nasal PulmonicEgressive
analyzeIPA "n"  = Just $ Consonant  Voiced Alveolar    Nasal PulmonicEgressive
analyzeIPA "ɳ"  = Just $ Consonant  Voiced Retroflex   Nasal PulmonicEgressive
analyzeIPA "ɲ"  = Just $ Consonant  Voiced Palatal     Nasal PulmonicEgressive
analyzeIPA "ŋ"  = Just $ Consonant  Voiced Velar       Nasal PulmonicEgressive
analyzeIPA "ɴ"  = Just $ Consonant  Voiced Uvular      Nasal PulmonicEgressive

-- Trills:
analyzeIPA "ʙ"  = Just $ Consonant  Voiced Bilabial Trill PulmonicEgressive
analyzeIPA "r"  = Just $ Consonant  Voiced Alveolar Trill PulmonicEgressive
analyzeIPA "ʀ"  = Just $ Consonant  Voiced Uvular   Trill PulmonicEgressive

-- Taps or flaps:
analyzeIPA "ⱱ"  = Just $ Consonant  Voiced LabioDental TapOrFlap PulmonicEgressive
analyzeIPA "ɾ"  = Just $ Consonant  Voiced Alveolar    TapOrFlap PulmonicEgressive
analyzeIPA "ɽ"  = Just $ Consonant  Voiced Retroflex   TapOrFlap PulmonicEgressive

-- Fricatives:
analyzeIPA "ɸ"  = Just $ Consonant  Voiceless Bilabial     Fricative PulmonicEgressive
analyzeIPA "β"  = Just $ Consonant  Voiced    Bilabial     Fricative PulmonicEgressive
analyzeIPA "f"  = Just $ Consonant  Voiceless LabioDental  Fricative PulmonicEgressive
analyzeIPA "v"  = Just $ Consonant  Voiced    LabioDental  Fricative PulmonicEgressive
analyzeIPA "θ"  = Just $ Consonant  Voiceless Dental       Fricative PulmonicEgressive
analyzeIPA "ð"  = Just $ Consonant  Voiced    Dental       Fricative PulmonicEgressive
analyzeIPA "s"  = Just $ Consonant  Voiceless Alveolar     Fricative PulmonicEgressive
analyzeIPA "z"  = Just $ Consonant  Voiced    Alveolar     Fricative PulmonicEgressive
analyzeIPA "ʃ"  = Just $ Consonant  Voiceless PostAlveolar Fricative PulmonicEgressive
analyzeIPA "ʒ"  = Just $ Consonant  Voiced    PostAlveolar Fricative PulmonicEgressive
analyzeIPA "ʂ"  = Just $ Consonant  Voiceless Retroflex    Fricative PulmonicEgressive
analyzeIPA "ʐ"  = Just $ Consonant  Voiced    Retroflex    Fricative PulmonicEgressive
analyzeIPA "ç"  = Just $ Consonant  Voiceless Palatal      Fricative PulmonicEgressive
analyzeIPA "ʝ"  = Just $ Consonant  Voiced    Palatal      Fricative PulmonicEgressive
analyzeIPA "x"  = Just $ Consonant  Voiceless Velar        Fricative PulmonicEgressive
analyzeIPA "ɣ"  = Just $ Consonant  Voiced    Velar        Fricative PulmonicEgressive
analyzeIPA "χ"  = Just $ Consonant  Voiceless Uvular       Fricative PulmonicEgressive
analyzeIPA "ʁ"  = Just $ Consonant  Voiced    Uvular       Fricative PulmonicEgressive
analyzeIPA "ħ"  = Just $ Consonant  Voiceless Pharyngeal   Fricative PulmonicEgressive
analyzeIPA "ʕ"  = Just $ Consonant  Voiced    Pharyngeal   Fricative PulmonicEgressive
analyzeIPA "h"  = Just $ Consonant  Voiceless Glottal      Fricative PulmonicEgressive
analyzeIPA "ɦ"  = Just $ Consonant  Voiced    Glottal      Fricative PulmonicEgressive


-- Lateral Fricatives:
analyzeIPA "ɬ" = Just $ Consonant  Voiceless Alveolar LateralFricative PulmonicEgressive
analyzeIPA "ɮ" = Just $ Consonant  Voiced    Alveolar LateralFricative PulmonicEgressive


-- Approximants:
analyzeIPA "ʋ"  = Just $ Consonant  Voiced LabioDental  Approximant PulmonicEgressive
analyzeIPA "ɹ"  = Just $ Consonant  Voiced Alveolar     Approximant PulmonicEgressive
analyzeIPA "ɻ"  = Just $ Consonant  Voiced Retroflex    Approximant PulmonicEgressive
analyzeIPA "j"  = Just $ Consonant  Voiced Palatal      Approximant PulmonicEgressive
analyzeIPA "ɰ"  = Just $ Consonant  Voiced Velar        Approximant PulmonicEgressive

-- Lateral Approximants:
analyzeIPA "l"  = Just $ Consonant  Voiced Alveolar  LateralApproximant PulmonicEgressive
analyzeIPA "ɭ"  = Just $ Consonant  Voiced Retroflex LateralApproximant PulmonicEgressive
analyzeIPA "ʎ"  = Just $ Consonant  Voiced Palatal   LateralApproximant PulmonicEgressive
analyzeIPA "ʟ"  = Just $ Consonant  Voiced Velar     LateralApproximant PulmonicEgressive



-- Affricates
analyzeIPA "t͡ʃ" = Just $ Consonant  Voiceless PostAlveolar Affricate PulmonicEgressive
analyzeIPA "d͡ʒ" = Just $ Consonant  Voiced    PostAlveolar Affricate PulmonicEgressive
-- We should probably enforce use of the tie-bar underneath, otherwise
-- it would not be deterministic to determine whether two graphemes here
-- represent affricates or a plosive followed by a fricative.




-- Under the Other Symbols part of the IPA chart:

analyzeIPA "w" = Just $ Consonant Voiced    LabialVelar    Approximant PulmonicEgressive
analyzeIPA "ʍ" = Just $ Consonant Voiceless LabialVelar    Fricative   PulmonicEgressive
analyzeIPA "ɥ" = Just $ Consonant Voiced    LabialPalatal  Approximant PulmonicEgressive
analyzeIPA "ʜ" = Just $ Consonant Voiceless Epiglottal     Fricative   PulmonicEgressive
analyzeIPA "ʢ" = Just $ Consonant Voiced    Epiglottal     Fricative   PulmonicEgressive
analyzeIPA "ʡ" = Just $ Consonant Voiceless Epiglottal     Plosive     PulmonicEgressive -- Is the epiglottal plosive voiceless? The IPA chart does not specify.
analyzeIPA "ɕ" = Just $ Consonant Voiceless AlveoloPalatal Fricative   PulmonicEgressive
analyzeIPA "ʑ" = Just $ Consonant Voiced    AlveoloPalatal Fricative   PulmonicEgressive
analyzeIPA "ɺ" = Just $ Consonant Voiced    Alveolar       LateralFlap PulmonicEgressive

analyzeIPA "ɧ" = Just $ Consonant Voiceless (Places [PostAlveolar, Velar]) Fricative PulmonicEgressive

-- Other Consonants:
analyzeIPA "ʘ" = Just $ Consonant Voiceless          Bilabial       Plosive        Click
analyzeIPA "ǀ" = Just $ Consonant Voiceless          Dental         Plosive        Click
analyzeIPA "ǃ" = Just $ Consonant Voiceless          Alveolar       Plosive        Click -- Or it could be PostAlveolar.
analyzeIPA "ǂ" = Just $ Consonant Voiceless          PalatoAlveolar Plosive        Click
analyzeIPA "ǁ" = Just $ Consonant Voiceless          Alveolar       Lateral        Click
analyzeIPA "ɓ" = Just $ Consonant Voiced             Bilabial       Plosive        Implosive
analyzeIPA "ɗ" = Just $ Consonant Voiced             Dental         Plosive        Implosive  -- Or Alveolar
analyzeIPA "ʄ" = Just $ Consonant Voiced             Palatal        Plosive        Implosive
analyzeIPA "ɠ" = Just $ Consonant Voiced             Velar          Plosive        Implosive
analyzeIPA "ʛ" = Just $ Consonant Voiced             Uvular         Plosive        Implosive

-- Close Vowels:
analyzeIPA "i"  = Just $ Vowel  Close Front   Unrounded Voiced
analyzeIPA "y"  = Just $ Vowel  Close Front   Rounded   Voiced
analyzeIPA "ɨ"  = Just $ Vowel  Close Central Unrounded Voiced
analyzeIPA "ʉ"  = Just $ Vowel  Close Central Rounded   Voiced
analyzeIPA "ɯ"  = Just $ Vowel  Close Back    Unrounded Voiced
analyzeIPA "u"  = Just $ Vowel  Close Back    Rounded   Voiced

-- Near-close Vowels:
analyzeIPA "ɪ"  = Just $ Vowel NearClose Front Unrounded Voiced
analyzeIPA "ʏ"  = Just $ Vowel NearClose Front Rounded   Voiced
analyzeIPA "ʊ"  = Just $ Vowel NearClose Back  Rounded   Voiced

-- Close-mid Vowels:
analyzeIPA "e"  = Just $ Vowel  CloseMid Front   Unrounded Voiced
analyzeIPA "ø"  = Just $ Vowel  CloseMid Front   Rounded   Voiced
analyzeIPA "ɘ"  = Just $ Vowel  CloseMid Central Unrounded Voiced
analyzeIPA "ɵ"  = Just $ Vowel  CloseMid Central Rounded   Voiced
analyzeIPA "ɤ"  = Just $ Vowel  CloseMid Back    Unrounded Voiced
analyzeIPA "o"  = Just $ Vowel  CloseMid Back    Rounded   Voiced

-- Mid Vowels:
analyzeIPA "ə"  = Just $ Vowel Mid Central Unrounded Voiced


-- Open-mid Vowels:
analyzeIPA "ɛ"  = Just $ Vowel  OpenMid Front   Unrounded Voiced
analyzeIPA "œ"  = Just $ Vowel  OpenMid Front   Rounded   Voiced
analyzeIPA "ɜ"  = Just $ Vowel  OpenMid Central Unrounded Voiced
analyzeIPA "ɞ"  = Just $ Vowel  OpenMid Central Rounded   Voiced
analyzeIPA "ʌ"  = Just $ Vowel  OpenMid Back    Unrounded Voiced
analyzeIPA "ɔ"  = Just $ Vowel  OpenMid Back    Rounded   Voiced

-- Near-open
analyzeIPA "æ"  = Just $ Vowel  NearOpen Front   Unrounded Voiced
analyzeIPA "ɐ"  = Just $ Vowel  NearOpen Central Unrounded  Voiced

-- Open Vowels:
analyzeIPA "a"  = Just $ Vowel  Open Front Unrounded Voiced
analyzeIPA "ɶ"  = Just $ Vowel  Open Front Rounded   Voiced
analyzeIPA "ɑ"  = Just $ Vowel  Open Back  Unrounded Voiced
analyzeIPA "ɒ"  = Just $ Vowel  Open Back  Rounded   Voiced



-- Handle Diacritics:
analyzeIPA ipaText =
  case [last ipaText] of
    "̥" ->
      let fullGrapheme = analyzeIPA (init ipaText)
      in case fullGrapheme of
              Just (Consonant _ place manner airstream)    -> Just $ Consonant Voiceless place manner airstream
              Just (Vowel height backness rounding _  )    -> Just $ Vowel height backness rounding Voiceless
              _                                            -> Nothing
    "̬" ->
      let fullGrapheme = analyzeIPA (init ipaText)
      in case fullGrapheme of
              Just (Consonant _ place manner airstream)    -> Just $ Consonant Voiced place manner airstream
              Just (Vowel height backness rounding _  )    -> Just $ Vowel height backness rounding Voiced
              _                                            -> Nothing

    "ʰ" ->
      let fullGrapheme = analyzeIPA (init ipaText)
      in case fullGrapheme of
              Just (Consonant Voiced place manner airstream   ) -> Just $ Consonant VoicedAspirated place manner airstream
              Just (Consonant Voiceless place manner airstream) -> Just $ Consonant VoicelessAspirated place manner airstream
              Just (Vowel height backness rounding voice      ) -> Just $ Vowel height backness rounding voice
              anythingElse                                      -> anythingElse
              -- (About the preceding line:) It is strange but we will just do nothing if they give us an aspirated vowel.
              -- since we have no way to represent it in the type system. to do: determine
              -- if the idea of an aspirated vowel makes sense
    _ -> Nothing -- Not recognized.


constructIPA :: Phonet -> IPAText
constructIPA phoneme = 
  case constructIPARecursive 3 0 phoneme of
    Just graphemes -> graphemes
    Nothing        -> "∅"

constructIPARecursive :: Int -> Int -> Phonet -> Maybe IPAText
constructIPARecursive recursionLimit recursionLevel _
  | recursionLevel == recursionLimit = Nothing

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
           Nothing -> Nothing
           Just regularIPA -> Just (regularIPA ++ "̠")  -- Add the diacritic for "retracted"



-- If there isn't a symbol, and the consonant we want is voiceless,
-- Just take the symbol for a voiced consonant,
-- and then put that diacritic that means voiceless after.
-- (The following two definitions are intended to implement that)
-- Add the small circle diacritic to consonants to make them voiceless.
constructIPARecursive recursionLimit recursionLevel  (Consonant Voiceless x y z)
  | recursionLevel <  recursionLimit
    = case constructIPARecursive recursionLimit (1 + recursionLevel)  (Consonant Voiced x y z) of 
           Nothing -> Nothing
           Just regularIPA -> Just (regularIPA ++ "̥") -- add diacritic for voiceless

-- Add the small circle diacritic to vowels to make them voiceless.
constructIPARecursive recursionLimit recursionLevel (Vowel x y z Voiceless)
  | recursionLevel <  recursionLimit
    = case constructIPARecursive recursionLimit (1 + recursionLevel) (Vowel x y z Voiced) of
           Nothing -> Nothing
           Just regularIPA -> Just (regularIPA ++ "̥")

-- If there is no way to express a voiced consonant in a single
-- grapheme add a diacritic to the grapheme that represents
-- the voiceless counterpart.
constructIPARecursive recursionLimit recursionLevel  (Consonant Voiced x y z)
  | recursionLevel <  recursionLimit
    = case constructIPARecursive recursionLimit (1 + recursionLevel) (Consonant Voiceless x y z) of 
           Nothing -> Nothing
           Just regularIPA -> Just (regularIPA ++ "̬")

constructIPARecursive recursionLimit recursionLevel  (Vowel x y z Voiced)
  | recursionLevel <  recursionLimit
    = case constructIPARecursive recursionLimit (1 + recursionLevel) (Vowel x y z Voiceless) of 
           Nothing -> Nothing 
           Just regularIPA -> Just (regularIPA ++ "̬")

constructIPARecursive recursionLimit recursionLevel  c@(Consonant VoicedAspirated _ _ PulmonicEgressive)
  | recursionLevel <  recursionLimit
    = case constructIPARecursive recursionLimit (1 + recursionLevel) (deaspirate c) of
           Nothing         -> Nothing
           Just regularIPA -> Just (regularIPA ++ "ʰ")

constructIPARecursive recursionLimit recursionLevel  c@(Consonant VoicelessAspirated _ _ PulmonicEgressive)
  | recursionLevel <  recursionLimit
    = case constructIPARecursive recursionLimit (1 + recursionLevel) (deaspirate c) of 
           Nothing         -> Nothing
           Just regularIPA -> Just (regularIPA ++ "ʰ")

constructIPARecursive recursionLimit recursionLevel  c@(Consonant CreakyVoiced _ _ PulmonicEgressive)
  | recursionLevel <  recursionLimit
    = case constructIPARecursive recursionLimit (1 + recursionLevel) (deaspirate c) of
           Nothing         -> Nothing
           Just regularIPA -> Just (regularIPA ++ "̰")


constructIPARecursive _ _ _
    = Nothing




deaspirate :: Phonet -> Phonet
deaspirate (Consonant VoicedAspirated place manner airstream) =
  (Consonant Voiced place manner airstream)

deaspirate (Consonant VoicelessAspirated place1 manner1 airstream1) =
  (Consonant Voiceless place1 manner1 airstream1)

deaspirate x = x


constructDeconstruct :: (Phonet -> Phonet) -> IPAText -> IPAText
constructDeconstruct func x = 
  let something = analyzeIPA x 
  in case something of 
       Nothing -> "∅" 
       Just phonet -> constructIPA (func phonet)

voicedIPA :: IPAText -> IPAText
voicedIPA = constructDeconstruct voicedPhonet

devoicedIPA :: IPAText -> IPAText
devoicedIPA = constructDeconstruct devoicedPhonet

spirantizedIPA :: IPAText -> IPAText
spirantizedIPA = constructDeconstruct spirantizedPhonet


{-|
Return an english description of a phoneme,
given a phoneme's representation in the
international phonetic alphabet.
  |-}
describeIPA :: IPAText -> String
describeIPA = show . analyzeIPA
