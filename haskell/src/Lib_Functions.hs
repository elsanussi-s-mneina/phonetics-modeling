{-# LANGUAGE OverloadedStrings #-}

module Lib_Functions where

import           EnglishUSText
import           Lib_Types
import           Prelude       ()
import           Relude        (Bool (False, True),
                                Maybe (Just, Nothing), NonEmpty (),
                                Text, elem, fmap,
                                one,
                                sconcat, toList, unwords,
                                (==))

equivalentInPlace :: Place -> Place -> Bool
Bilabial `equivalentInPlace` Bilabial = True
LabioDental `equivalentInPlace` LabioDental = True
Dental `equivalentInPlace` Dental = True
Alveolar `equivalentInPlace` Alveolar = True
PostAlveolar `equivalentInPlace` PostAlveolar = True
Retroflex `equivalentInPlace` Retroflex = True
Palatal `equivalentInPlace` Palatal = True
Velar `equivalentInPlace` Velar = True
Uvular `equivalentInPlace` Uvular = True
Pharyngeal `equivalentInPlace` Pharyngeal = True
Glottal `equivalentInPlace` Glottal = True
Epiglottal `equivalentInPlace` Epiglottal = True
x `equivalentInPlace` Places pList = x `elem` pList
Places x `equivalentInPlace` y = y `equivalentInPlace` Places x
_ `equivalentInPlace` _ = False


-- | Given a place of articulation,
--   returns the place of articulation that is
--   the next more retracted.
retractedPlace :: Place -> Place
retractedPlace place =
  case place of
    Bilabial     -> LabioDental
    LabioDental  -> Dental
    Dental       -> Alveolar
    Alveolar     -> PostAlveolar
    PostAlveolar -> Retroflex
    Retroflex    -> Palatal
    Palatal      -> Velar
    Velar        -> Uvular
    Uvular       -> Pharyngeal
    Pharyngeal   -> Glottal
    Glottal      -> Epiglottal
    same         -> same


-- | Gives the English description of a phone.
englishDescription :: Phonet -> Text
englishDescription = showPhonet

-- | A function that given an IPA symbol will convert it to the voiced
--   equivalent.
voicedPhonet :: Phonet -> Phonet
voicedPhonet p = case p of
  (Consonant VoicelessAspirated x y z) -> Consonant VoicedAspirated x y z
  (Consonant Voiceless x y z)          -> Consonant Voiced x y z
  (Consonant Voiced x y z)             -> Consonant Voiced x y z
  (Consonant VoicedAspirated x y z)    -> Consonant VoicedAspirated x y z
  (Consonant _ x y z)                  -> Consonant Voiced x y z
  (Vowel x y z _)                      -> Vowel x y z Voiced

-- | A function that given an IPA symbol will convert it to the voiceless
--   equivalent.
devoicedPhonet :: Phonet -> Phonet
devoicedPhonet p = case p of
  (Consonant Voiced x y z)             -> Consonant Voiceless x y z
  (Consonant CreakyVoiced x y z)       -> Consonant Voiceless x y z
  (Consonant Voiceless x y z)          -> Consonant Voiceless x y z
  (Consonant VoicedAspirated x y z)    -> Consonant VoicelessAspirated x y z
  (Consonant VoicelessAspirated x y z) -> Consonant VoicelessAspirated x y z
  (Vowel x y z _)                      -> Vowel x y z Voiceless

spirantizedPhonet :: Phonet -> Phonet
-- The following is inelegant, but there is no other way in the system,
-- right now. The part that is inelegant is that,
-- a [t] which is considered alveolar, when spirantized becomes [θ]
-- which is dental.
-- So the following line implements this
-- change in place of articulation.
spirantizedPhonet p = case p of
  (Consonant x Alveolar Plosive z) -> Consonant x Dental Fricative z
  (Consonant x place_1 Plosive z)  -> Consonant x place_1 Fricative z
  other                            -> other

unmarkDifferences :: Phonet -> Phonet -> UnmarkablePhonet
unmarkDifferences p_1 p_2 = case (p_1, p_2) of
  ( Consonant voice_1 place_1 manner_1 airstream_1,
    Consonant voice_2 place_2 manner_2 airstream_2
    ) ->
      let voice' = unmarkVoice voice_1 voice_2
          place' = unmarkPlace place_1 place_2
          manner' = unmarkManner manner_1 manner_2
          airstream' = unmarkAirstream airstream_1 airstream_2
       in UnmarkableConsonant voice' place' manner' airstream'
  ( Vowel height_1 backness_1 rounding_1 voice_1,
    Vowel height_2 backness_2 rounding_2 voice_2
    ) ->
      let voice' = unmarkVoice voice_1 voice_2
          height' = unmarkHeight height_1 height_2
          backness' = unmarkBackness backness_1 backness_2
          rounding' = unmarkRounding rounding_1 rounding_2
       in UnmarkableVowel height' backness' rounding' voice'
  ( Vowel _ _ _ voice_1,
    Consonant voice_2 _ _ _
    ) ->
      let voice' = unmarkVoice voice_1 voice_2
       in UnmarkableVowel UnmarkedHeight UnmarkedBackness UnmarkedRounding voice'
  ( Consonant {},
    Vowel {}
    ) ->
      unmarkDifferences p_2 p_1 -- Change the order of arguments
  where
    unmarkVoice voice_1 voice_2 =
      if voice_1 == voice_2
        then MarkedVocalFolds voice_1
        else UnmarkedVocalFolds

    unmarkPlace place_1 place_2 =
      if place_1 `equivalentInPlace` place_2
        then MarkedPlace place_1
        else UnmarkedPlace

    unmarkManner manner_1 manner_2 =
      if manner_1 == manner_2
        then MarkedManner manner_1
        else UnmarkedManner

    unmarkAirstream airstream_1 airstream_2 =
      if airstream_1 == airstream_2
        then MarkedAirstream airstream_1
        else UnmarkedAirstream

    unmarkHeight height_1 height_2 =
      if height_1 == height_2
        then MarkedHeight height_1
        else UnmarkedHeight

    unmarkBackness backness_1 backness_2 =
      if backness_1 == backness_2
        then MarkedBackness backness_1
        else UnmarkedBackness

    unmarkRounding rounding_1 rounding_2 =
      if rounding_1 == rounding_2
        then MarkedRounding rounding_1
        else UnmarkedRounding

-- This function
-- takes any unmarked attributes in the phoneme definition,
-- and returns a list with all possible phonemes that have that attribute.
similarPhonemesTo :: UnmarkablePhonet -> [Phonet]
similarPhonemesTo (UnmarkableConsonant voice_1 place_1 manner_1 airstream_1) =
  let voice' = toList (similarInVoice voice_1)
      place' = toList (similarInPlace place_1)
      manner' = toList (similarInManner manner_1)
      airstream' = toList (similarInAirstream airstream_1)
   in [Consonant v p m a | p <- place', v <- voice', m <- manner', a <- airstream']
similarPhonemesTo (UnmarkableVowel height_1 backness_1 rounding_1 voice_1) =
  let voice' = toList (similarInVoice voice_1)
      height' = toList (similarInHeight height_1)
      backness' = toList (similarInBackness backness_1)
      rounding' = toList (similarInRounding rounding_1)
   in [Vowel h b r v | h <- height', b <- backness', r <- rounding', v <- voice']

similarInVoice :: UnmarkableVocalFolds -> NonEmpty VocalFolds
similarInVoice voice_1 =
  case voice_1 of
    MarkedVocalFolds x -> one x
    UnmarkedVocalFolds -> vocalFoldStates

similarInPlace :: UnmarkablePlace -> NonEmpty Place
similarInPlace place_1 =
  case place_1 of
    MarkedPlace x -> one x
    UnmarkedPlace -> placeStates

similarInManner :: UnmarkableManner -> NonEmpty Manner
similarInManner manner_1 =
  case manner_1 of
    MarkedManner x -> one x
    UnmarkedManner -> mannerStates

similarInAirstream :: UnmarkableAirstream -> NonEmpty Airstream
similarInAirstream airstream_1 =
  case airstream_1 of
    MarkedAirstream x -> one x
    UnmarkedAirstream -> airstreamStates

similarInHeight :: UnmarkableHeight -> NonEmpty Height
similarInHeight height_1 =
  case height_1 of
    MarkedHeight x -> one x
    UnmarkedHeight -> heightStates

similarInBackness :: UnmarkableBackness -> NonEmpty Backness
similarInBackness backness_1 =
  case backness_1 of
    MarkedBackness x -> one x
    UnmarkedBackness -> backnessStates

similarInRounding :: UnmarkableRounding -> NonEmpty Rounding
similarInRounding rounding_1 =
  case rounding_1 of
    MarkedRounding x -> one x
    UnmarkedRounding -> roundingStates

-- The following function returns whether an articulation is
-- considered impossible according to the IPA (pulmonic) consonants chart.
-- Does not work for other values.
impossible :: Phonet -> Bool
impossible p = case p of
  (Consonant Voiced Pharyngeal Plosive PulmonicEgressive) ->
    True
  (Consonant VoicedAspirated Pharyngeal Plosive PulmonicEgressive) ->
    True
  (Consonant Voiceless Glottal Plosive PulmonicEgressive) ->
    False -- [ʔ] is not impossible.
  (Consonant _ Glottal Fricative PulmonicEgressive) ->
    False -- [h] and [ɦ] are not impossible.
  (Consonant _ Glottal _ PulmonicEgressive) ->
    True -- all other pulmonary egressive consonants are impossible..
  (Consonant _ Pharyngeal Nasal PulmonicEgressive) ->
    True
  (Consonant _ Pharyngeal LateralFricative PulmonicEgressive) ->
    True
  (Consonant _ Pharyngeal LateralApproximant PulmonicEgressive) ->
    True
  (Consonant _ Velar Trill PulmonicEgressive) ->
    True
  (Consonant _ Velar TapOrFlap PulmonicEgressive) ->
    True
  (Consonant _ Bilabial LateralFricative PulmonicEgressive) ->
    True
  (Consonant _ Bilabial LateralApproximant PulmonicEgressive) ->
    True
  (Consonant _ LabioDental LateralFricative PulmonicEgressive) ->
    True
  (Consonant _ LabioDental LateralApproximant PulmonicEgressive) ->
    True
  _ ->
    False -- Everything else is assumed to be possible.

retractPhonet :: Maybe Phonet -> Maybe Phonet
retractPhonet (Just (Consonant v p m a)) = Just (Consonant v (retractedPlace p) m a)
retractPhonet _ = Nothing

deaspirate :: Phonet -> Phonet
deaspirate (Consonant VoicedAspirated place manner airstream) =
  Consonant Voiced place manner airstream
deaspirate (Consonant VoicelessAspirated place_1 manner_1 airstream_1) =
  Consonant Voiceless place_1 manner_1 airstream_1
deaspirate x = x

decreak :: Phonet -> Phonet
decreak (Consonant CreakyVoiced place manner airstream) =
  Consonant Voiced place manner airstream
decreak x = x

showPhonet :: Phonet -> Text
showPhonet phonet =
  case phonet of
    Consonant v p m a ->
      unwords
        [ showVocalFolds v,
          showPlace p,
          showManner m,
          showAirstream a,
          consonantUIText
        ]
    Vowel h b r v ->
      unwords
        [ showVocalFolds v,
          showRounding r,
          showHeight h,
          showBackness b,
          vowelUIText
        ]

showBackness :: Backness -> Text
showBackness Front   = frontBacknessUIText
showBackness Central = centralBacknessUIText
showBackness Back    = backBacknessUIText

showHeight :: Height -> Text
showHeight height =
  case height of
    Close     -> closeHeightUIText
    NearClose -> nearCloseHeightUIText
    CloseMid  -> closeMidHeightUIText
    Mid       -> midHeightUIText
    OpenMid   -> openMidHeightUIText
    NearOpen  -> nearOpenHeightUIText
    Open      -> openHeightUIText

showRounding :: Rounding -> Text
showRounding Rounded   = roundedRoundingUIText
showRounding Unrounded = unroundedRoundingUIText

showPlace :: Place -> Text
showPlace place_1 =
  case place_1 of
    Bilabial       -> bilabialPlaceUIText
    LabioDental    -> labioDentalPlaceUIText
    Dental         -> dentalPlaceUIText
    Alveolar       -> alveolarPlaceUIText
    PostAlveolar   -> postAlveolarPlaceUIText
    Retroflex      -> retroflexPlaceUIText
    Palatal        -> palatalPlaceUIText
    Velar          -> velarPlaceUIText
    Uvular         -> uvularPlaceUIText
    Pharyngeal     -> pharyngealPlaceUIText
    Glottal        -> glottalPlaceUIText
    Epiglottal     -> epiglottalPlaceUIText
    LabialVelar    -> labialVelarPlaceUIText
    LabialPalatal  -> labialPalatalPlaceUIText
    AlveoloPalatal -> alveoloPalatalPlaceUIText
    PalatoAlveolar -> palatoAlveolarPlaceUIText
    Places ps      -> unwords (toList (fmap showPlace ps))

showManner :: Manner -> Text
showManner manner_1 =
  case manner_1 of
    Plosive            -> plosiveMannerUIText
    Nasal              -> nasalMannerUIText
    Trill              -> trillMannerUIText
    TapOrFlap          -> tapOrFlapMannerUIText
    Approximant        -> approximantMannerUIText
    Fricative          -> fricativeMannerUIText
    Affricate          -> affricateMannerUIText
    LateralFricative   -> lateralFricativeMannerUIText
    LateralApproximant -> lateralApproximantMannerUIText
    LateralFlap        -> lateralFlapMannerUIText
    Lateral            -> lateralMannerUIText

showAirstream :: Airstream -> Text
showAirstream airstream_1 =
  case airstream_1 of
    PulmonicEgressive -> pulmonicEgressiveAirstreamUIText
    Click             -> clickAirstreamUIText
    Implosive         -> implosiveAirstreamUIText

showVocalFolds :: VocalFolds -> Text
showVocalFolds vocalFolds_1 =
  case vocalFolds_1 of
    Voiced             -> voicedVocalFoldsUIText
    Voiceless          -> voicelessVocalFoldsUIText
    VoicedAspirated    -> voicedAspiratedVocalFoldsUIText
    VoicelessAspirated -> voicelessAspiratedVocalFoldsUIText
    CreakyVoiced       -> creakyVoicedVocalFoldsUIText

showPhonetInventory :: PhonetInventory -> Text
showPhonetInventory (PhonetInventory phonetes) =
  sconcat (fmap showPhonet phonetes)
