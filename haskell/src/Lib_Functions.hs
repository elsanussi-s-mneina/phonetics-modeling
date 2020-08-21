{-# LANGUAGE OverloadedStrings #-}


module Lib_Functions where

import           EnglishUSText
import           Lib_Types
import           Prelude (Bool (False, True),
                          Maybe (Just, Nothing),
                          elem, fmap,
                          (==), (||))
import Data.Semigroup (Semigroup(sconcat))
import GHC.Exts (IsList (toList))
import Data.Text (Text, unwords)
import qualified Data.Text as T

import Lib_PseudoLens

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
retractedPlace placeValue =
  case placeValue of
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
voicedPhonet p =
  if isAspirated p
  then toVoicedAspirated p
  else toVoiced p

-- | A function that given an IPA symbol will convert it to the voiceless
--   equivalent.
devoicedPhonet :: Phonet -> Phonet
devoicedPhonet p =
  if isAspirated p
  then toVoicelessAspirated p
  else toVoiceless p

-- | whether a phoneme is aspirated,
--   (regardless of whether or not it is voiced)
isAspirated :: Phonet -> Bool
isAspirated p =
  let vf = vocalFolds p
  in vf == VoicelessAspirated || vf == VoicedAspirated

-- | Make a phoneme spirantized. That is,
--  change its manner of articulation to fricative.
spirantizedPhonet :: Phonet -> Phonet
-- The following is inelegant, but there is no other way in the system,
-- right now. The part that is inelegant is that,
-- a [t] which is considered alveolar, when spirantized becomes [θ]
-- which is dental.
-- So the following line implements this
-- change in place of articulation.
spirantizedPhonet p =
  if place p == Just Alveolar
    then withManner Fricative (withPlace Dental p)
    else withManner Fricative p

-- | The following function returns whether an articulation is
--   considered impossible according to the IPA (pulmonic) consonants chart.
--   Does not work for other values.
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
    True -- all other glottal pulmonic egressive consonants are impossible..
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
deaspirate p =
  let vf = vocalFolds p
  in case vf of
       VoicedAspirated -> withVocalFolds Voiced p
       VoicelessAspirated -> withVocalFolds Voiceless p
       _ -> p

aspirate :: Phonet -> Phonet
aspirate p =
  let vf = vocalFolds p
  in case vf of
       Voiced -> withVocalFolds VoicedAspirated p
       Voiceless -> withVocalFolds VoicelessAspirated p
       _ -> p


decreak :: Phonet -> Phonet
decreak p =
  if vocalFolds p == CreakyVoiced
    then toVoiced p
    else p

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
            showSecondaryArticulation sa,
            showPlace p,
            showManner m,
            showAirstream a,
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
    Places ps      -> T.unwords (toList (fmap showPlace ps))

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
