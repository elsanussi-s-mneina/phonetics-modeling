{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}

module UserInterfaceText where

import           Relude  (Text, Eq, IO, (<>), ($), map, putTextLn)
import         EnglishUSText
import         qualified  DiagnosticText
import qualified Data.Text as T

data UITextTicket
  = ApplicationTitle 
  | Menu
  | UserInputViewEnglishPhonemeInventory
  | UserInputMakeAPhonemeVoiced
  | UserInputMakeAPhonemeUnvoiced
  | UserInputDescribeAPhonemeInEnglish
  | UserInputDescribeAPhonemeInSPE
  | UserInputChunkIPAByPhoneme
  | Prompt
  | SorryUnableToCalculate
  | TypeAPhoneme
  | PhonemeToDevoiceMessage
  | PhonemeToVoiceMessage
  | PhonemeToDescribeMessage
  | PhonemeToCalculateSPEMessage
  | PleaseReadReadmeMessage
  | ProgramTerminatedNormallyMessage
  | UserSelectedMessage
  | UnrecognizedSelectionMessage
  | NoAnalysisFoundMessage
  | IpaTextToDivideMessage
  | NoEnglishDescriptionFoundMessage
  | ConsonantUIText
  | VowelUIText
  | FrontBacknessUIText
  | CentralBacknessUIText
  | BackBacknessUIText
  | CloseHeightUIText
  | NearCloseHeightUIText
  | CloseMidHeightUIText
  | MidHeightUIText
  | OpenMidHeightUIText
  | NearOpenHeightUIText
  | OpenHeightUIText
  | RoundedRoundingUIText
  | UnroundedRoundingUIText
  | BilabialPlaceUIText
  | LabioDentalPlaceUIText
  | DentalPlaceUIText
  | AlveolarPlaceUIText
  | PostAlveolarPlaceUIText
  | RetroflexPlaceUIText
  | PalatalPlaceUIText
  | VelarPlaceUIText
  | UvularPlaceUIText
  | PharyngealPlaceUIText
  | GlottalPlaceUIText
  | EpiglottalPlaceUIText
  | LabialVelarPlaceUIText
  | LabialPalatalPlaceUIText
  | AlveoloPalatalPlaceUIText
  | PalatoAlveolarPlaceUIText
  | PlosiveMannerUIText
  | NasalMannerUIText
  | TrillMannerUIText
  | TapOrFlapMannerUIText
  | ApproximantMannerUIText
  | FricativeMannerUIText
  | AffricateMannerUIText
  | LateralFricativeMannerUIText
  | LateralApproximantMannerUIText
  | LateralFlapMannerUIText
  | LateralMannerUIText
  | PulmonicEgressiveAirstreamUIText
  | ClickAirstreamUIText
  | ImplosiveAirstreamUIText
  | VoicedVocalFoldsUIText
  | VoicelessVocalFoldsUIText
  | VoicedAspiratedVocalFoldsUIText
  | VoicelessAspiratedVocalFoldsUIText
  | CreakyVoicedVocalFoldsUIText
  | SyllabicPhonemeFeatureUIText
  | ConsonantalPhonemeFeatureUIText
  | SonorantPhonemeFeatureUIText
  | ContinuantPhonemeFeatureUIText
  | VoicePhonemeFeatureUIText
  | AtrPhonemeFeatureUIText
  | NasalPhonemeFeatureUIText
  | LateralPhonemeFeatureUIText
  | DelayedReleasePhonemeFeatureUIText
  | SpreadGlottisPhonemeFeatureUIText
  | ConstrictedGlottisPhonemeFeatureUIText
  | LabialPhonemeFeatureUIText
  | CoronalPhonemeFeatureUIText
  | DorsalPhonemeFeatureUIText
  | PharyngealPhonemeFeatureUIText
  | LaryngealPhonemeFeatureUIText
  | RoundPhonemeFeatureUIText
  | AnteriorPhonemeFeatureUIText
  | DistributedPhonemeFeatureUIText
  | StridentPhonemeFeatureUIText
  | HighPhonemeFeatureUIText
  | LowPhonemeFeatureUIText
  | BackPhonemeFeatureUIText
  | ShowPhonemeInventoryUIText
  | MakeAPhonemeVoicedUIText
  | QuitUIText
  | MakeAPhonemeUnvoicedUIText
  | DescribePhonemeUIText
  | GetFeaturesOfPhonemeUIText
  | SplitTranscriptionUIText
  | ResultHeader
  | VoicedPhonemeHeader
  | UnvoicedPhonemeHeader
  | PhonemeDescriptionHeader
  | FeaturesHeader
  | PhonemesSplitHeader
  | EnglishPhonemeInventoryHeader
  | InputHeader
  | BeforeServerStartMessage

allUITextTicketList :: [Text]
allUITextTicketList
  =
  [ "ApplicationTitle"
  , "Menu"
  , "UserInputViewEnglishPhonemeInventory"
  , "UserInputMakeAPhonemeVoiced"
  , "UserInputMakeAPhonemeUnvoiced"
  , "UserInputDescribeAPhonemeInEnglish"
  , "UserInputDescribeAPhonemeInSPE"
  , "UserInputChunkIPAByPhoneme"
  , "Prompt"
  , "SorryUnableToCalculate"
  , "TypeAPhoneme"
  , "PhonemeToDevoiceMessage"
  , "PhonemeToVoiceMessage"
  , "PhonemeToDescribeMessage"
  , "PhonemeToCalculateSPEMessage"
  , "PleaseReadReadmeMessage"
  , "ProgramTerminatedNormallyMessage"
  , "UserSelectedMessage"
  , "UnrecognizedSelectionMessage"
  , "NoAnalysisFoundMessage"
  , "IpaTextToDivideMessage"
  , "NoEnglishDescriptionFoundMessage"
  , "ConsonantUIText"
  , "VowelUIText"
  , "FrontBacknessUIText"
  , "CentralBacknessUIText"
  , "BackBacknessUIText"
  , "CloseHeightUIText"
  , "NearCloseHeightUIText"
  , "CloseMidHeightUIText"
  , "MidHeightUIText"
  , "OpenMidHeightUIText"
  , "NearOpenHeightUIText"
  , "OpenHeightUIText"
  , "RoundedRoundingUIText"
  , "UnroundedRoundingUIText"
  , "BilabialPlaceUIText"
  , "LabioDentalPlaceUIText"
  , "DentalPlaceUIText"
  , "AlveolarPlaceUIText"
  , "PostAlveolarPlaceUIText"
  , "RetroflexPlaceUIText"
  , "PalatalPlaceUIText"
  , "VelarPlaceUIText"
  , "UvularPlaceUIText"
  , "PharyngealPlaceUIText"
  , "GlottalPlaceUIText"
  , "EpiglottalPlaceUIText"
  , "LabialVelarPlaceUIText"
  , "LabialPalatalPlaceUIText"
  , "AlveoloPalatalPlaceUIText"
  , "PalatoAlveolarPlaceUIText"
  , "PlosiveMannerUIText"
  , "NasalMannerUIText"
  , "TrillMannerUIText"
  , "TapOrFlapMannerUIText"
  , "ApproximantMannerUIText"
  , "FricativeMannerUIText"
  , "AffricateMannerUIText"
  , "LateralFricativeMannerUIText"
  , "LateralApproximantMannerUIText"
  , "LateralFlapMannerUIText"
  , "LateralMannerUIText"
  , "PulmonicEgressiveAirstreamUIText"
  , "ClickAirstreamUIText"
  , "ImplosiveAirstreamUIText"
  , "VoicedVocalFoldsUIText"
  , "VoicelessVocalFoldsUIText"
  , "VoicedAspiratedVocalFoldsUIText"
  , "VoicelessAspiratedVocalFoldsUIText"
  , "CreakyVoicedVocalFoldsUIText"
  , "SyllabicPhonemeFeatureUIText"
  , "ConsonantalPhonemeFeatureUIText"
  , "SonorantPhonemeFeatureUIText"
  , "ContinuantPhonemeFeatureUIText"
  , "VoicePhonemeFeatureUIText"
  , "AtrPhonemeFeatureUIText"
  , "NasalPhonemeFeatureUIText"
  , "LateralPhonemeFeatureUIText"
  , "DelayedReleasePhonemeFeatureUIText"
  , "SpreadGlottisPhonemeFeatureUIText"
  , "ConstrictedGlottisPhonemeFeatureUIText"
  , "LabialPhonemeFeatureUIText"
  , "CoronalPhonemeFeatureUIText"
  , "DorsalPhonemeFeatureUIText"
  , "PharyngealPhonemeFeatureUIText"
  , "LaryngealPhonemeFeatureUIText"
  , "RoundPhonemeFeatureUIText"
  , "AnteriorPhonemeFeatureUIText"
  , "DistributedPhonemeFeatureUIText"
  , "StridentPhonemeFeatureUIText"
  , "HighPhonemeFeatureUIText"
  , "LowPhonemeFeatureUIText"
  , "BackPhonemeFeatureUIText"
  , "ShowPhonemeInventoryUIText"
  , "MakeAPhonemeVoicedUIText"
  , "QuitUIText"
  , "MakeAPhonemeUnvoicedUIText"
  , "DescribePhonemeUIText"
  , "GetFeaturesOfPhonemeUIText"
  , "SplitTranscriptionUIText"
  , "ResultHeader"
  , "VoicedPhonemeHeader"
  , "UnvoicedPhonemeHeader"
  , "PhonemeDescriptionHeader"
  , "FeaturesHeader"
  , "PhonemesSplitHeader"
  , "EnglishPhonemeInventoryHeader"
  , "InputHeader"
  , "BeforeServerStartMessage"
  ]

data NatLanguage
  = English
  | French
  | Mandarin
  | Arabic
  deriving stock Eq

languageList :: [Text]
languageList
  =
  [ "English"
  , "French"
  , "Mandarin"
  , "Arabic"
  ]

i18n :: NatLanguage -> UITextTicket -> Text
i18n lang t =
   case t of
        ApplicationTitle
          -> case lang of
               English   -> EnglishUSText.applicationTitle
               _         -> DiagnosticText.applicationTitle
        Menu                                      -> menu
        UserInputViewEnglishPhonemeInventory      -> userInputViewEnglishPhonemeInventory
        UserInputMakeAPhonemeVoiced               -> userInputMakeAPhonemeVoiced
        UserInputMakeAPhonemeUnvoiced             -> userInputMakeAPhonemeUnvoiced
        UserInputDescribeAPhonemeInEnglish        -> userInputDescribeAPhonemeInEnglish
        UserInputDescribeAPhonemeInSPE            -> userInputDescribeAPhonemeInSPE
        UserInputChunkIPAByPhoneme                -> userInputChunkIPAByPhoneme
        Prompt                                    -> prompt
        SorryUnableToCalculate                    -> sorryUnableToCalculate
        TypeAPhoneme                              -> typeAPhoneme
        PhonemeToDevoiceMessage                   -> phonemeToDevoiceMessage
        PhonemeToVoiceMessage                     -> phonemeToVoiceMessage
        PhonemeToDescribeMessage                  -> phonemeToDescribeMessage
        PhonemeToCalculateSPEMessage              -> phonemeToCalculateSPEMessage
        PleaseReadReadmeMessage                   -> pleaseReadReadmeMessage
        ProgramTerminatedNormallyMessage          -> programTerminatedNormallyMessage
        UserSelectedMessage                       -> userSelectedMessage
        UnrecognizedSelectionMessage              -> unrecognizedSelectionMessage
        NoAnalysisFoundMessage                    -> noAnalysisFoundMessage
        IpaTextToDivideMessage                    -> ipaTextToDivideMessage
        NoEnglishDescriptionFoundMessage          -> noEnglishDescriptionFoundMessage
        ConsonantUIText                           -> consonantUIText
        VowelUIText                               -> vowelUIText
        FrontBacknessUIText                       -> frontBacknessUIText
        CentralBacknessUIText                     -> centralBacknessUIText
        BackBacknessUIText                        -> backBacknessUIText
        CloseHeightUIText                         -> closeHeightUIText
        NearCloseHeightUIText                     -> nearCloseHeightUIText
        CloseMidHeightUIText                      -> closeMidHeightUIText
        MidHeightUIText                           -> midHeightUIText
        OpenMidHeightUIText                       -> openMidHeightUIText
        NearOpenHeightUIText                      -> nearOpenHeightUIText
        OpenHeightUIText                          -> openHeightUIText
        RoundedRoundingUIText                     -> roundedRoundingUIText
        UnroundedRoundingUIText                   -> unroundedRoundingUIText
        BilabialPlaceUIText                       -> bilabialPlaceUIText
        LabioDentalPlaceUIText                    -> labioDentalPlaceUIText
        DentalPlaceUIText                         -> dentalPlaceUIText
        AlveolarPlaceUIText                       -> alveolarPlaceUIText
        PostAlveolarPlaceUIText                   -> postAlveolarPlaceUIText
        RetroflexPlaceUIText                      -> retroflexPlaceUIText
        PalatalPlaceUIText                        -> palatalPlaceUIText
        VelarPlaceUIText                          -> velarPlaceUIText
        UvularPlaceUIText                         -> uvularPlaceUIText
        PharyngealPlaceUIText                     -> pharyngealPlaceUIText
        GlottalPlaceUIText                        -> glottalPlaceUIText
        EpiglottalPlaceUIText                     -> epiglottalPlaceUIText
        LabialVelarPlaceUIText                    -> labialVelarPlaceUIText
        LabialPalatalPlaceUIText                  -> labialPalatalPlaceUIText
        AlveoloPalatalPlaceUIText                 -> alveoloPalatalPlaceUIText
        PalatoAlveolarPlaceUIText                 -> palatoAlveolarPlaceUIText
        PlosiveMannerUIText                       -> plosiveMannerUIText
        NasalMannerUIText                         -> nasalMannerUIText
        TrillMannerUIText                         -> trillMannerUIText
        TapOrFlapMannerUIText                     -> tapOrFlapMannerUIText
        ApproximantMannerUIText                   -> approximantMannerUIText
        FricativeMannerUIText                     -> fricativeMannerUIText
        AffricateMannerUIText                     -> affricateMannerUIText
        LateralFricativeMannerUIText              -> lateralFricativeMannerUIText
        LateralApproximantMannerUIText            -> lateralApproximantMannerUIText
        LateralFlapMannerUIText                   -> lateralFlapMannerUIText
        LateralMannerUIText                       -> lateralMannerUIText
        PulmonicEgressiveAirstreamUIText          -> pulmonicEgressiveAirstreamUIText
        ClickAirstreamUIText                      -> clickAirstreamUIText
        ImplosiveAirstreamUIText                  -> implosiveAirstreamUIText
        VoicedVocalFoldsUIText                    -> voicedVocalFoldsUIText
        VoicelessVocalFoldsUIText                 -> voicelessVocalFoldsUIText
        VoicedAspiratedVocalFoldsUIText           -> voicedAspiratedVocalFoldsUIText
        VoicelessAspiratedVocalFoldsUIText        -> voicelessAspiratedVocalFoldsUIText
        CreakyVoicedVocalFoldsUIText              -> creakyVoicedVocalFoldsUIText
        SyllabicPhonemeFeatureUIText              -> syllabicPhonemeFeatureUIText
        ConsonantalPhonemeFeatureUIText           -> consonantalPhonemeFeatureUIText
        SonorantPhonemeFeatureUIText              -> sonorantPhonemeFeatureUIText
        ContinuantPhonemeFeatureUIText            -> continuantPhonemeFeatureUIText
        VoicePhonemeFeatureUIText                 -> voicePhonemeFeatureUIText
        AtrPhonemeFeatureUIText                   -> atrPhonemeFeatureUIText
        NasalPhonemeFeatureUIText                 -> nasalPhonemeFeatureUIText
        LateralPhonemeFeatureUIText               -> lateralPhonemeFeatureUIText
        DelayedReleasePhonemeFeatureUIText        -> delayedReleasePhonemeFeatureUIText
        SpreadGlottisPhonemeFeatureUIText         -> spreadGlottisPhonemeFeatureUIText
        ConstrictedGlottisPhonemeFeatureUIText    -> constrictedGlottisPhonemeFeatureUIText
        LabialPhonemeFeatureUIText                -> labialPhonemeFeatureUIText
        CoronalPhonemeFeatureUIText               -> coronalPhonemeFeatureUIText
        DorsalPhonemeFeatureUIText                -> dorsalPhonemeFeatureUIText
        PharyngealPhonemeFeatureUIText            -> pharyngealPhonemeFeatureUIText
        LaryngealPhonemeFeatureUIText             -> laryngealPhonemeFeatureUIText
        RoundPhonemeFeatureUIText                 -> roundPhonemeFeatureUIText
        AnteriorPhonemeFeatureUIText              -> anteriorPhonemeFeatureUIText
        DistributedPhonemeFeatureUIText           -> distributedPhonemeFeatureUIText
        StridentPhonemeFeatureUIText              -> stridentPhonemeFeatureUIText
        HighPhonemeFeatureUIText                  -> highPhonemeFeatureUIText
        LowPhonemeFeatureUIText                   -> lowPhonemeFeatureUIText
        BackPhonemeFeatureUIText                  -> backPhonemeFeatureUIText
        ShowPhonemeInventoryUIText                -> showPhonemeInventoryUIText
        MakeAPhonemeVoicedUIText                  -> makeAPhonemeVoicedUIText
        QuitUIText                                -> quitUIText
        MakeAPhonemeUnvoicedUIText                -> makeAPhonemeUnvoicedUIText
        DescribePhonemeUIText                     -> describePhonemeUIText
        GetFeaturesOfPhonemeUIText                -> getFeaturesOfPhonemeUIText
        SplitTranscriptionUIText                  -> splitTranscriptionUIText
        ResultHeader                              -> resultHeader
        VoicedPhonemeHeader                       -> voicedPhonemeHeader
        UnvoicedPhonemeHeader                     -> unvoicedPhonemeHeader
        PhonemeDescriptionHeader                  -> phonemeDescriptionHeader
        FeaturesHeader                            -> featuresHeader
        PhonemesSplitHeader                       -> phonemesSplitHeader
        EnglishPhonemeInventoryHeader             -> englishPhonemeInventoryHeader
        InputHeader                               -> inputHeader
        BeforeServerStartMessage                  -> beforeServerStartMessage


il8nFunctionGen :: [Text] -> [Text] -> Text
il8nFunctionGen natLangIdentifiers textTokenIdentifiers =
  "i18n :: NatLanguage -> UITextTicket -> Text\n\
  \i18n lang t =\n\
  \   case t of\n"
  <>  T.concat (map (writeCaseForTextToken natLangIdentifiers) textTokenIdentifiers)
  where
    writeCaseForTextToken :: [Text] -> Text -> Text
    writeCaseForTextToken natLanguages textToken =
       "     "<> textToken <>"\n       -> case lang of\n" <>
         (T.concat $ map (\natLang ->
                          "               " <> natLang <> "   -> " <> natLang <> "Text.applicationTitle\n")
                         natLanguages)


il8nGenMain :: IO ()
il8nGenMain =
  let generated = il8nFunctionGen languageList allUITextTicketList
  in putTextLn generated


