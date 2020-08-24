module ShowFunctions where

import Prelude (fmap)
import Data.Text (Text, empty, pack, replace, unwords, concat)
import EnglishUSText
import Lib_Types

-- | Replaces two consecutive spaces with one wherever they occur in a text
removeExtraTwoSpaces :: Text -> Text
removeExtraTwoSpaces x = replace (pack "  ") (pack " ") x

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
showVowelLength NormalLength = empty
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
    Places ps      -> unwords (fmap showPlace ps)

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
    Labialized     -> labializedUIText
    Palatalized    -> palatalizedUIText
    Velarized      -> velarizedUIText
    Pharyngealized -> pharyngealizedUIText
    Normal         -> empty

showPhonetInventory :: PhonetInventory -> Text
showPhonetInventory (PhonetInventory phonetes) =
  concat (fmap showPhonet phonetes)
