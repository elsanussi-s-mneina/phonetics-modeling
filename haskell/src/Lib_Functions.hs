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
import qualified Data.Text as T


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
  (Consonant VoicelessAspirated w x y z) -> Consonant VoicedAspirated w x y z
  (Consonant Voiceless w x y z)          -> Consonant Voiced w x y z
  (Consonant Voiced w x y z)             -> Consonant Voiced w x y z
  (Consonant VoicedAspirated w x y z)    -> Consonant VoicedAspirated w x y z
  (Consonant _ w x y z)                  -> Consonant Voiced w x y z
  (Vowel x y z _ vl)                      -> Vowel x y z Voiced vl

-- | A function that given an IPA symbol will convert it to the voiceless
--   equivalent.
devoicedPhonet :: Phonet -> Phonet
devoicedPhonet p = case p of
  (Consonant Voiced w x y z)             -> Consonant Voiceless w x y z
  (Consonant CreakyVoiced w x y z)       -> Consonant Voiceless w x y z
  (Consonant Voiceless w x y z)          -> Consonant Voiceless w x y z
  (Consonant VoicedAspirated w x y z)    -> Consonant VoicelessAspirated w x y z
  (Consonant VoicelessAspirated w x y z) -> Consonant VoicelessAspirated w x y z
  (Vowel x y z _ vl)                      -> Vowel x y z Voiceless vl

spirantizedPhonet :: Phonet -> Phonet
-- The following is inelegant, but there is no other way in the system,
-- right now. The part that is inelegant is that,
-- a [t] which is considered alveolar, when spirantized becomes [θ]
-- which is dental.
-- So the following line implements this
-- change in place of articulation.
spirantizedPhonet p = case p of
  (Consonant x Alveolar Plosive z sa) -> Consonant x Dental Fricative z sa
  (Consonant x place_1 Plosive z sa)  -> Consonant x place_1 Fricative z sa
  other                            -> other

unmarkDifferences :: Phonet -> Phonet -> UnmarkablePhonet
unmarkDifferences p_1 p_2 = case (p_1, p_2) of
  ( Consonant voice_1 place_1 manner_1 airstream_1 secondary_1,
    Consonant voice_2 place_2 manner_2 airstream_2 secondary_2
    ) ->
      let voice' = unmarkVoice voice_1 voice_2
          place' = unmarkPlace place_1 place_2
          manner' = unmarkManner manner_1 manner_2
          airstream' = unmarkAirstream airstream_1 airstream_2
          secondary' = unmarkSecondaryArticulation secondary_1 secondary_2
       in UnmarkableConsonant voice' place' manner' airstream' secondary'
  ( Vowel height_1 backness_1 rounding_1 voice_1 vowel_length_1,
    Vowel height_2 backness_2 rounding_2 voice_2 vowel_length_2
    ) ->
      let voice' = unmarkVoice voice_1 voice_2
          height' = unmarkHeight height_1 height_2
          backness' = unmarkBackness backness_1 backness_2
          rounding' = unmarkRounding rounding_1 rounding_2
          vowel_length' = unmarkVowelLength vowel_length_1 vowel_length_2
       in UnmarkableVowel height' backness' rounding' voice' vowel_length'
  ( Vowel _ _ _ voice_1 _,
    Consonant voice_2 _ _ _ _
    ) ->
      let voice' = unmarkVoice voice_1 voice_2
       in UnmarkableVowel UnmarkedHeight UnmarkedBackness UnmarkedRounding voice' UnmarkedVowelLength
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

    unmarkSecondaryArticulation secondary_1 secondary_2 =
      if secondary_1 == secondary_2
        then MarkedSecondaryArticulation secondary_1
        else UnmarkedSecondaryArticulation

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

    unmarkVowelLength vowel_length_1 vowel_length_2 =
      if vowel_length_1 == vowel_length_2
        then MarkedVowelLength vowel_length_1
        else UnmarkedVowelLength

-- This function
-- takes any unmarked attributes in the phoneme definition,
-- and returns a list with all possible phonemes that have that attribute.
similarPhonemesTo :: UnmarkablePhonet -> [Phonet]
similarPhonemesTo (UnmarkableConsonant voice_1 place_1 manner_1 airstream_1 secondary_1) =
  let voice' = toList (similarInVoice voice_1)
      place' = toList (similarInPlace place_1)
      manner' = toList (similarInManner manner_1)
      airstream' = toList (similarInAirstream airstream_1)
      secondary' = toList (similarInSecondaryArticulation secondary_1)
   in [Consonant v p m a sa | p <- place', v <- voice', m <- manner', a <- airstream', sa <- secondary']
similarPhonemesTo (UnmarkableVowel height_1 backness_1 rounding_1 voice_1 vowel_length_1) =
  let voice' = toList (similarInVoice voice_1)
      height' = toList (similarInHeight height_1)
      backness' = toList (similarInBackness backness_1)
      rounding' = toList (similarInRounding rounding_1)
      vowel_length' = toList (similarInLength vowel_length_1)
   in [Vowel h b r v l | h <- height', b <- backness', r <- rounding', v <- voice', l <- vowel_length']

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

similarInSecondaryArticulation :: UnmarkableSecondaryArticulation -> NonEmpty SecondaryArticulation
similarInSecondaryArticulation secondaryArticulation_1 =
  case secondaryArticulation_1 of
    MarkedSecondaryArticulation x -> one x
    UnmarkedSecondaryArticulation -> secondaryArticulationStates

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

similarInLength :: UnmarkableVowelLength -> NonEmpty VowelLength
similarInLength vowel_length_1 =
  case vowel_length_1 of
    MarkedVowelLength x -> one x
    UnmarkedVowelLength -> vowelLengthStates

-- The following function returns whether an articulation is
-- considered impossible according to the IPA (pulmonic) consonants chart.
-- Does not work for other values.
impossible :: Phonet -> Bool
impossible p = case p of
  (Consonant Voiced Pharyngeal Plosive PulmonicEgressive _) ->
    True
  (Consonant VoicedAspirated Pharyngeal Plosive PulmonicEgressive _) ->
    True
  (Consonant Voiceless Glottal Plosive PulmonicEgressive _) ->
    False -- [ʔ] is not impossible.
  (Consonant _ Glottal Fricative PulmonicEgressive _) ->
    False -- [h] and [ɦ] are not impossible.
  (Consonant _ Glottal _ PulmonicEgressive _) ->
    True -- all other pulmonary egressive consonants are impossible..
  (Consonant _ Pharyngeal Nasal PulmonicEgressive _) ->
    True
  (Consonant _ Pharyngeal LateralFricative PulmonicEgressive _) ->
    True
  (Consonant _ Pharyngeal LateralApproximant PulmonicEgressive _) ->
    True
  (Consonant _ Velar Trill PulmonicEgressive _) ->
    True
  (Consonant _ Velar TapOrFlap PulmonicEgressive _) ->
    True
  (Consonant _ Bilabial LateralFricative PulmonicEgressive _) ->
    True
  (Consonant _ Bilabial LateralApproximant PulmonicEgressive _) ->
    True
  (Consonant _ LabioDental LateralFricative PulmonicEgressive _) ->
    True
  (Consonant _ LabioDental LateralApproximant PulmonicEgressive _) ->
    True
  _ ->
    False -- Everything else is assumed to be possible.

retractPhonet :: Maybe Phonet -> Maybe Phonet
retractPhonet (Just (Consonant v p m a sa)) = Just (Consonant v (retractedPlace p) m a sa)
retractPhonet _ = Nothing

deaspirate :: Phonet -> Phonet
deaspirate (Consonant VoicedAspirated place manner airstream secondary) =
  Consonant Voiced place manner airstream secondary
deaspirate (Consonant VoicelessAspirated place_1 manner_1 airstream_1 secondary_1) =
  Consonant Voiceless place_1 manner_1 airstream_1 secondary_1
deaspirate x = x

decreak :: Phonet -> Phonet
decreak (Consonant CreakyVoiced place manner airstream secondary) =
  Consonant Voiced place manner airstream secondary
decreak x = x

-- | Replaces two consecutive spaces with one wherever they occur in a text
removeExtraTwoSpaces :: Text -> Text
removeExtraTwoSpaces x = T.replace "  " " " x


showPhonet :: Phonet -> Text
showPhonet phonet =
  case phonet of
    Consonant v p m a sa ->
      removeExtraTwoSpaces (
        unwords
          [ showVocalFolds v,
            showPlace p,
            showManner m,
            showAirstream a,
            showSecondaryArticulation sa,
            consonantUIText
          ]
        )
    Vowel h b r v l ->
      removeExtraTwoSpaces (
        unwords
          [ showVocalFolds v,
            showRounding r,
            showHeight h,
            showBackness b,
            showVowelLength l,
            vowelUIText
          ]
        )
-- | Provide user-readable text for the backness of
--   a vowel.
--
--   e.g. "central"
showBackness :: Backness -> Text
showBackness Front   = frontBacknessUIText
showBackness Central = centralBacknessUIText
showBackness Back    = backBacknessUIText

-- | Provide user readable text for reprsenting
--   height of a vowel.
--   
--   e.g. "mid"
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

-- | Provide user readable text for representing
--   lip rounding.
--   e.g. "rounded"
showRounding :: Rounding -> Text
showRounding Rounded   = roundedRoundingUIText
showRounding Unrounded = unroundedRoundingUIText

-- | Provide user readable text for representing
--   vowel length.
--   e.g. "half-long"
showVowelLength :: VowelLength -> Text
showVowelLength NormalLength = ""
showVowelLength ExtraShort = extraShortUIText
showVowelLength HalfLong = halfLongUIText
showVowelLength Long = longUIText

-- | Provide user readable text for representing
--   the place of articulation.
--   e.g. "bilabial"
--   Convert place to a string that the user can read.
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

-- | Provide user-readable text for representing
--   the manner of articulation.
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

-- | user-readable representation of a Airstream configuration.
--   Converts an Airstream object to user-readable text.
--   e.g. "pulmonic egressive"
showAirstream :: Airstream -> Text
showAirstream airstream_1 =
  case airstream_1 of
    PulmonicEgressive -> pulmonicEgressiveAirstreamUIText
    Click             -> clickAirstreamUIText
    Implosive         -> implosiveAirstreamUIText

-- | user-readable text representation of a vocal fold configuration:
--   e.g. "voiced", "creaky voiced"
showVocalFolds :: VocalFolds -> Text
showVocalFolds vocalFolds_1 =
  case vocalFolds_1 of
    Voiced             -> voicedVocalFoldsUIText
    Voiceless          -> voicelessVocalFoldsUIText
    VoicedAspirated    -> voicedAspiratedVocalFoldsUIText
    VoicelessAspirated -> voicelessAspiratedVocalFoldsUIText
    CreakyVoiced       -> creakyVoicedVocalFoldsUIText

-- | user-readable text representation of a secondary articulation:
--   e.g. "pharyngealized"
showSecondaryArticulation :: SecondaryArticulation -> Text
showSecondaryArticulation secondary_articulation =
  case secondary_articulation of
    Labialized -> labializedUIText
    Palatalized -> palatalizedUIText
    Velarized -> velarizedUIText
    Pharyngealized -> pharyngealizedUIText
    Normal -> ""


showPhonetInventory :: PhonetInventory -> Text
showPhonetInventory (PhonetInventory phonetes) =
  sconcat (fmap showPhonet phonetes)
