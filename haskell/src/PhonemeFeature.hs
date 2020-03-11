module PhonemeFeature where

import Lib

import Data.List (intercalate)
-- Part for features:
-- Go to Section 12.2 of the textbook.
data Polarity = Plus | Minus
                deriving Eq

data PhonemeFeature = SyllabicFeature Polarity
                    | ConsonantalFeature Polarity
                    | SonorantFeature Polarity
                    | ContinuantFeature Polarity
                    | VoiceFeature Polarity
                    | AdvancedTongueRootFeature Polarity
                    | NasalFeature
                    | LateralFeature
                    | DelayedReleaseFeature
                    | SpreadGlottisFeature
                    | ConstrictedGlottisFeature
                    | LabialFeature
                    | CoronalFeature
                    | DorsalFeature
                    | PharyngealFeature
                    | LaryngealFeature
                    | RoundFeature Polarity
                    | AnteriorFeature Polarity
                    | DistributedFeature Polarity
                    | StridentFeature Polarity
                    | HighFeature Polarity
                    | LowFeature Polarity
                    | BackFeature Polarity
                    deriving Eq

instance Show Polarity where
  show Plus = "+"
  show Minus = "-"

instance Show PhonemeFeature where
  show (SyllabicFeature polarity)           = show polarity ++ " syllabic"
  show (ConsonantalFeature polarity)        = show polarity ++ " consonantal"
  show (SonorantFeature polarity)           = show polarity ++ " sonorant"
  show (ContinuantFeature polarity)         = show polarity ++ " continuant"
  show (VoiceFeature polarity)              = show polarity ++ " voice"
  show (AdvancedTongueRootFeature polarity) = show polarity ++ " ATR"
  show NasalFeature                         = "nasal"
  show LateralFeature                       = "lateral"
  show DelayedReleaseFeature                = "DR"
  show SpreadGlottisFeature                 = "SG"
  show ConstrictedGlottisFeature            = "CG"
  show LabialFeature                        = "labial"
  show CoronalFeature                       = "coronal"
  show DorsalFeature                        = "dorsal"
  show PharyngealFeature                    = "pharyngeal"
  show LaryngealFeature                     = "laryngeal"
  show (RoundFeature polarity)              = show polarity ++ " round"
  show (AnteriorFeature polarity)           = show polarity ++ " anterior"
  show (DistributedFeature polarity)        = show polarity ++ " distributed"
  show (StridentFeature polarity)           = show polarity ++ " strident"
  show (HighFeature polarity)               = show polarity ++ " high"
  show (LowFeature polarity)                = show polarity ++ " low"
  show (BackFeature polarity)               = show polarity ++ " back"

isUnary :: PhonemeFeature -> Bool
isUnary NasalFeature = True
isUnary LateralFeature = True
isUnary DelayedReleaseFeature = True
isUnary SpreadGlottisFeature = True
isUnary ConstrictedGlottisFeature = True
isUnary LabialFeature = True
isUnary CoronalFeature = True
isUnary DorsalFeature = True
isUnary PharyngealFeature = True
isUnary LaryngealFeature = True
isUnary _ = False


analyzeFeatures :: Phonet -> [PhonemeFeature] 
analyzeFeatures phonete =
    concat [syllabicFL phonete
           , consonantalFL phonete
           , sonorantFL phonete
           , continuantFL phonete
           , voiceFL phonete
           , atrFL phonete
           , nasalFL phonete
           , lateralFL phonete
           , delayedReleaseFL phonete
           , spreadGlottisFL phonete
           , constrictedGlottisFL phonete
           , labialFL phonete
           , coronalFL phonete
           , dorsalFL phonete
           , pharyngealFL phonete
           , laryngealFL phonete
           , roundFL phonete
           , anteriorFL phonete
           , distributedFL phonete
           , stridentFL phonete
           , highFL phonete
           , lowFL phonete
           , backFL phonete
           ]
  

difference :: [PhonemeFeature] 
           -> [PhonemeFeature] 
           -> [(Maybe PhonemeFeature, Maybe PhonemeFeature)]
difference list1 list2 = 
  [ differenceOfBinaryFeature SyllabicFeature list1 list2
  , differenceOfBinaryFeature ConsonantalFeature list1 list2
  , differenceOfBinaryFeature SonorantFeature list1 list2
  , differenceOfBinaryFeature ContinuantFeature list1 list2
  , differenceOfBinaryFeature VoiceFeature list1 list2
  , differenceOfBinaryFeature AdvancedTongueRootFeature list1 list2
  , differenceOfUnaryFeature NasalFeature list1 list2
  , differenceOfUnaryFeature LateralFeature list1 list2
  , differenceOfUnaryFeature DelayedReleaseFeature list1 list2
  , differenceOfUnaryFeature SpreadGlottisFeature list1 list2
  , differenceOfUnaryFeature ConstrictedGlottisFeature list1 list2
  , differenceOfUnaryFeature LabialFeature list1 list2
  , differenceOfUnaryFeature CoronalFeature list1 list2
  , differenceOfUnaryFeature DorsalFeature list1 list2
  , differenceOfUnaryFeature PharyngealFeature list1 list2
  , differenceOfUnaryFeature LaryngealFeature list1 list2
  , differenceOfBinaryFeature RoundFeature list1 list2
  , differenceOfBinaryFeature AnteriorFeature list1 list2
  , differenceOfBinaryFeature DistributedFeature list1 list2
  , differenceOfBinaryFeature StridentFeature list1 list2
  , differenceOfBinaryFeature HighFeature list1 list2
  , differenceOfBinaryFeature LowFeature list1 list2
  , differenceOfBinaryFeature BackFeature list1 list2
  ]

differenceOfBinaryFeature :: 
          (Polarity -> PhonemeFeature) 
                    -> [PhonemeFeature] 
                    -> [PhonemeFeature] 
                    -> (Maybe PhonemeFeature, Maybe PhonemeFeature)
differenceOfBinaryFeature feature list1 list2 =
   let
   relevant x = x == feature Plus || x == feature Minus
   list1Relevant = filter relevant list1
   list2Relevant = filter relevant list2
   in
     if  null list1Relevant && null list2Relevant
       || (not (null list1Relevant) && not (null list2Relevant) &&
           head list1Relevant == head list2Relevant)
       then (Nothing, Nothing)
       else if not (null list1Relevant) && not (null list2Relevant)
         then (Just (head list1Relevant), Just (head list2Relevant))
         else if length list1Relevant > length list2Relevant
         then (Just (head list1Relevant), Nothing)
           else (Nothing, Just (head list2Relevant))
           

differenceOfUnaryFeature :: PhonemeFeature 
                         -> [PhonemeFeature] 
                         -> [PhonemeFeature] 
                         -> (Maybe PhonemeFeature, Maybe PhonemeFeature)
differenceOfUnaryFeature feature list1 list2 =
  if elem feature list1 == elem feature list2
     then (Nothing, Nothing)
     else if elem feature list1 && not (elem feature list2)
            then (Just feature, Nothing)
            else (Nothing, Just feature)



syllabic :: Phonet -> Maybe Bool
syllabic (Vowel _ _ _ _ ) = Just True
syllabic (Consonant _ _ _ _) = Just False

syllabicFL = binaryFeature syllabic SyllabicFeature


isGlide :: Phonet -> Bool
isGlide (Consonant _ Palatal Approximant PulmonicEgressive) = True
isGlide (Consonant _ LabialVelar Approximant PulmonicEgressive) = True
isGlide (Consonant _ LabialPalatal Approximant PulmonicEgressive) = True
isGlide (Consonant _ Velar Approximant PulmonicEgressive) = True
isGlide _ = False



consonantal :: Phonet -> Maybe Bool
consonantal consonant@(Consonant v p m a) = 
  Just (not (isGlide consonant))
consonantal (Vowel _ _ _ _) = Just False


    
binaryFeature featureFunction featureConstructor phonete =
  case featureFunction phonete of
    Just True  -> [featureConstructor Plus]
    Just False -> [featureConstructor Minus]
    Nothing    -> []

consonantalFL = binaryFeature consonantal ConsonantalFeature


sonorant :: Phonet -> Maybe Bool
-- Vowels are sonorants.
sonorant (Vowel _ _ _ _) = Just True
-- Nasals are sonorants.
sonorant (Consonant _ _ Nasal _) = Just True
-- Approximants are sonorants.
sonorant (Consonant _ _ Approximant _) = Just True
-- Laterals are sonorants.
sonorant (Consonant _ _ LateralApproximant _ ) = Just True
-- Are Lateral flaps, and Laterals that are not fricatives approximants. 
-- Let us just guess that they are:
-- sonorant (Consonants _ _ Lateral _ ) = True -- unsure

-- sonorant (Consonants _ _ LateralFlap _ ) = Flap 
-- I am unsure whether lateral flaps are sonorants. That is
-- why the previous line of code is commented out.


-- Fricatives are not sonorants.
sonorant (Consonant _ _ Fricative _) = Just False
-- Lateral fricatives are not sonorants.
sonorant (Consonant _ _ LateralFricative _) = Just False
-- Affricates are not sonorants.
sonorant (Consonant _ _ Affricate _) = Just False
sonorant _ = Just False -- Add more


sonorantFL = binaryFeature sonorant SonorantFeature


continuant (Consonant _ _ Fricative _) = Just True
continuant (Consonant _ _ Approximant _ ) = Just True
continuant (Consonant _ _ Lateral _) = Just True
continuant (Consonant _ _ LateralFricative _) = Just True
continuant (Consonant _ _ LateralApproximant _) = Just True
continuant c@(Consonant _ _ _ _) = Just (isGlide c)
continuant (Vowel _ _ _ _) = Just True

continuantFL = binaryFeature continuant ContinuantFeature

nasal (Consonant _ _ Nasal _) = Just True
nasal _ = Just False

nasalFL = unaryFeature nasal NasalFeature

lateral (Consonant _ _ manner _) = Just (
  manner == Lateral || 
  manner == LateralApproximant || 
  manner == LateralFricative)
lateral _ = Just False

lateralFL = unaryFeature lateral LateralFeature

delayedRelease (Consonant _ _ Affricate _) = Just True
delayedRelease _ = Just False

delayedReleaseFL = unaryFeature delayedRelease DelayedReleaseFeature


unaryFeature featurePredicate featureConstructor phoneme = 
  if featurePredicate phoneme == Just True
  then [featureConstructor]
  else []

labial (Consonant _ place _ _) = 
  Just
  (
    place == Bilabial || 
    place == LabioDental
  )
labial _ = Just False

labialFL = unaryFeature labial LabialFeature

coronal (Consonant _ place _ _) = 
  Just (
  place `elem` [ Dental
               , Alveolar
               , PostAlveolar
               , Retroflex
               , Palatal
               , AlveoloPalatal
               ]
        )
coronal _ = Just False

coronalFL = unaryFeature  coronal CoronalFeature

dorsal (Consonant _ place _ _) = 
  Just
  (
  place `elem` [Palatal, AlveoloPalatal, Velar, Uvular] 
  )
  -- Palatal is actually in parentheses in the textbook
dorsal _ = Just False

dorsalFL = unaryFeature dorsal DorsalFeature

pharyngeal (Consonant _ Pharyngeal _ _) = Just True
pharyngeal _ = Just False

pharyngealFL = unaryFeature pharyngeal PharyngealFeature

laryngeal (Consonant _ Glottal _ _ ) = Just True
laryngeal _ = Just False

laryngealFL = unaryFeature laryngeal LaryngealFeature

voice (Consonant Voiceless Glottal Plosive PulmonicEgressive) = 
  Just False 
-- [ʔ] is [- voice]


voice (Consonant v _ _ _) = 
  Just (v == Voiced || v == VoicedAspirated)
voice (Vowel _ _ _ v) = Just (v == Voiced)
-- TODO: handle creaky voice = True

voiceFL = binaryFeature voice VoiceFeature

spreadGlottis :: Phonet -> Maybe Bool
spreadGlottis (Consonant VoicelessAspirated _ _ _) = Just True
spreadGlottis (Consonant VoicedAspirated    _ _ _) = Just True
spreadGlottis _ = Just False

spreadGlottisFL = unaryFeature spreadGlottis SpreadGlottisFeature

constrictedGlottis :: Phonet -> Maybe Bool
constrictedGlottis (Consonant 
                      Voiceless 
                      Glottal 
                      Plosive 
                      PulmonicEgressive) = 
  Just True
constrictedGlottis _  = Just False
-- TODO: add CreakyVoice here = Just True

constrictedGlottisFL = 
  unaryFeature
    constrictedGlottis 
    ConstrictedGlottisFeature

anterior (Consonant _ Dental            _ _) = Just True
anterior (Consonant _ Alveolar          _ _) = Just True
anterior (Consonant _ PostAlveolar      _ _) = Just False
anterior (Consonant _ Retroflex         _ _) = Just False
anterior (Consonant _ Palatal           _ _) = Just False
anterior (Consonant _ AlveoloPalatal    _ _) = Just False
anterior _ = Nothing

anteriorFL = binaryFeature anterior AnteriorFeature

distributed (Consonant _ Dental         _ _) = Just True
distributed (Consonant _ Alveolar       _ _) = Just False
distributed (Consonant _ PostAlveolar   _ _) = Just True
distributed (Consonant _ Retroflex      _ _) = Just False
distributed (Consonant _ Palatal        _ _) = Just True
distributed (Consonant _ AlveoloPalatal _ _) = Just True
distributed _ = Nothing

distributedFL = binaryFeature distributed DistributedFeature

strident (Consonant _ Bilabial       _ _) = Just False
strident (Consonant _ LabioDental    _ _) = Just True
strident (Consonant _ Dental         _ _) = Just False
strident (Consonant _ Alveolar       _ _) = Just True
strident (Consonant _ PostAlveolar   _ _) = Just True
strident (Consonant _ Retroflex      _ _) = Just False
strident (Consonant _ Palatal        _ _) = Just False
strident (Consonant _ AlveoloPalatal _ _) = Just False
strident (Consonant _ Velar          _ _) = Just False
strident (Consonant _ Uvular         _ _) = Just True
strident (Consonant _ Pharyngeal     _ _) = Just False
strident (Consonant _ Glottal        _ _) = Just False
strident _ = Nothing

stridentFL = binaryFeature strident StridentFeature

high (Consonant _ Palatal _ _) = Just True
high (Consonant _ AlveoloPalatal _ _) = Just True
high (Consonant _ Velar _ _) = Just True
high (Consonant _ Uvular _ _) = Just False
high (Consonant _ _ _ _) = Nothing
high (Vowel height _ _ _ ) = 
  Just (height == Close || height == NearClose)

highFL = binaryFeature high HighFeature

low (Consonant _ Uvular _ _) = Just True
low (Consonant _ Pharyngeal _ _) = Just True
low (Consonant _ Glottal _ _) = Just True
low (Consonant _ _ _ _) = Nothing
low (Vowel height _ _ _ ) = Just (height == Open || height == NearOpen)

lowFL = binaryFeature low LowFeature

back (Vowel _ Back _ _) = Just True
back (Vowel _ Central _ _) = Just True
back (Vowel _ Front _ _) = Just False
back _ = Nothing

backFL = binaryFeature back BackFeature

round (Vowel _ _ rounding _) = Just (rounding == Rounded)
round _ = Just False

roundFL = binaryFeature PhonemeFeature.round RoundFeature

atr (Vowel  Close     Front   Unrounded Voiced) = Just True  -- [i]
atr (Vowel  CloseMid  Front   Unrounded Voiced) = Just True  -- [e]
atr (Vowel  Close     Back    Rounded   Voiced) = Just True  -- [u]
atr (Vowel  CloseMid  Front   Rounded   Voiced) = Just True  -- [ø]
atr (Vowel  CloseMid  Back    Rounded   Voiced) = Just True  -- [o]
atr (Vowel  Close     Front   Rounded   Voiced) = Just True  -- [y]
atr (Vowel  NearOpen  Front   Unrounded Voiced) = Just False -- [æ]
atr (Vowel  Open      Back    Unrounded Voiced) = Just False -- [ɑ]
atr (Vowel  Close     Central Unrounded Voiced) = Just False -- [ɨ]
atr (Vowel  OpenMid   Back    Unrounded Voiced) = Just False -- [ʌ]
atr (Vowel  NearClose Front   Unrounded Voiced) = Just False
atr (Vowel  NearClose Back    Rounded   Voiced) = Just False
atr (Vowel  OpenMid   Front   Unrounded Voiced) = Just False
atr (Vowel  OpenMid   Back    Rounded   Voiced) = Just False
atr _ = Nothing

atrFL = binaryFeature atr AdvancedTongueRootFeature

toTextFeaturesVersion2 :: Phonet -> String
toTextFeaturesVersion2 phonete =
  let allString = [ consonantalFL phonete
                  , syllabicFL phonete
                  , continuantFL phonete
                  , sonorantFL phonete
                  , delayedReleaseFL phonete
                  , anteriorFL phonete
                  , distributedFL phonete
                  , stridentFL phonete
                  , highFL phonete
                  , lowFL phonete
                  , nasalFL phonete
                  , labialFL phonete
                  , coronalFL phonete
                  , dorsalFL phonete
                  , pharyngealFL phonete
                  , laryngealFL phonete
                  , backFL phonete
                  , roundFL phonete
                  , atrFL phonete
                  , spreadGlottisFL phonete
                  , constrictedGlottisFL phonete
                  ]
  in "[" ++ intercalate "; " (map show (concat allString)) ++ "]"


toTextConsonantalFeature :: Phonet -> Maybe String
toTextConsonantalFeature phonete =
  toTextFeature consonantalFL phonete 


toTextSyllabicFeature :: Phonet -> Maybe String
toTextSyllabicFeature phonete =
  toTextFeature syllabicFL phonete 


toTextVoiceFeature :: Phonet -> Maybe String
toTextVoiceFeature phonete =
  toTextFeature voiceFL phonete 


toTextContinuantFeature :: Phonet -> Maybe String
toTextContinuantFeature phonete =
  toTextFeature continuantFL phonete 

 
toTextSonorantFeature :: Phonet -> Maybe String
toTextSonorantFeature phonete =
  toTextFeature sonorantFL phonete 

toTextDelayedReleaseFeature :: Phonet -> Maybe String
toTextDelayedReleaseFeature phonete =
  toTextFeature delayedReleaseFL phonete 

toTextBackFeature :: Phonet -> Maybe String
toTextBackFeature phonete = 
  toTextFeature backFL phonete


toTextRoundFeature :: Phonet -> Maybe String
toTextRoundFeature phonete = 
  toTextFeature roundFL phonete

toTextATRFeature :: Phonet -> Maybe String
toTextATRFeature phonete = 
  toTextFeature atrFL phonete

toTextSpreadGlottisFeature :: Phonet -> Maybe String
toTextSpreadGlottisFeature phonete = 
  toTextFeature spreadGlottisFL phonete

toTextConstrictedGlottisFeature :: Phonet -> Maybe String
toTextConstrictedGlottisFeature phonete = 
  toTextFeature constrictedGlottisFL phonete



toTextAnteriorFeature :: Phonet -> Maybe String
toTextAnteriorFeature phonete = 
  toTextFeature anteriorFL phonete

toTextDistributedFeature :: Phonet -> Maybe String
toTextDistributedFeature phonete = 
  toTextFeature distributedFL phonete


toTextStridentFeature :: Phonet -> Maybe String
toTextStridentFeature phonete = 
  toTextFeature stridentFL phonete
 

toTextHighFeature :: Phonet -> Maybe String
toTextHighFeature phonete = 
  toTextFeature highFL phonete

toTextLowFeature :: Phonet -> Maybe String
toTextLowFeature phonete = 
  toTextFeature lowFL phonete


toTextNasalFeature :: Phonet -> Maybe String
toTextNasalFeature phonete =
  toTextFeature nasalFL phonete

toTextLateralFeature :: Phonet -> Maybe String
toTextLateralFeature phonete =
  toTextFeature lateralFL phonete


toTextLabialFeature :: Phonet -> Maybe String
toTextLabialFeature phonete =
  toTextFeature labialFL phonete

toTextCoronalFeature :: Phonet -> Maybe String
toTextCoronalFeature phonete =
  toTextFeature coronalFL phonete

toTextDorsalFeature :: Phonet -> Maybe String
toTextDorsalFeature phonete =
  toTextFeature dorsalFL phonete

toTextPharyngealFeature :: Phonet -> Maybe String
toTextPharyngealFeature phonete =
  toTextFeature pharyngealFL phonete

toTextLaryngealFeature :: Phonet -> Maybe String
toTextLaryngealFeature phonete = 
   toTextFeature laryngealFL phonete

toTextFeature featureListFunction phonete =
  let result = featureListFunction phonete
  in if not (null result)
        then Just (show (head result))
        else Nothing
    

