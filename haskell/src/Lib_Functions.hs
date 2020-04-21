{-# LANGUAGE UnicodeSyntax #-}


module Lib_Functions where

import Lib_Types

import Prelude (Bool(False, True), Show(show), String)

import Prelude.Unicode ((≡))


retractedPlace ∷ Place → Place
retractedPlace Bilabial     = LabioDental
retractedPlace LabioDental  = Dental
retractedPlace Dental       = Alveolar
retractedPlace Alveolar     = PostAlveolar
retractedPlace PostAlveolar = Retroflex
retractedPlace Retroflex    = Palatal
retractedPlace Palatal      = Velar
retractedPlace Velar        = Uvular
retractedPlace Uvular       = Pharyngeal
retractedPlace Pharyngeal   = Glottal
retractedPlace Glottal      = Epiglottal
retractedPlace same         = same




englishDescription ∷ Phonet → String
englishDescription x = show x


-- | A function that given an IPA symbol will convert it to the voiced equivalent.
voicedPhonet ∷ Phonet → Phonet
voicedPhonet (Consonant   VoicelessAspirated x y z) = Consonant   VoicedAspirated x y z
voicedPhonet (Consonant   Voiceless          x y z) = Consonant   Voiced x y z
voicedPhonet (Consonant   Voiced             x y z) = Consonant   Voiced x y z
voicedPhonet (Consonant   VoicedAspirated    x y z) = Consonant   VoicedAspirated x y z
voicedPhonet (Consonant   _                  x y z) = Consonant   Voiced x y z
voicedPhonet (Vowel x y z _                       ) = Vowel x y z Voiced

-- | A function that given an IPA symbol will convert it to the voiceless equivalent.
devoicedPhonet ∷ Phonet → Phonet
devoicedPhonet (Consonant   Voiced             x y z) = Consonant   Voiceless          x y z
devoicedPhonet (Consonant   CreakyVoiced       x y z) = Consonant   Voiceless          x y z
devoicedPhonet (Consonant   Voiceless          x y z) = Consonant   Voiceless          x y z
devoicedPhonet (Consonant   VoicedAspirated    x y z) = Consonant   VoicelessAspirated x y z
devoicedPhonet (Consonant   VoicelessAspirated x y z) = Consonant   VoicelessAspirated x y z
devoicedPhonet (Vowel x y z _                       ) = Vowel x y z Voiceless



spirantizedPhonet ∷ Phonet → Phonet

-- The following is inelegant, but there is no other way in the system,
-- right now. The part that is inelegant is that,
-- a [t] which is considered alveolar, when spirantized becomes [θ] which is dental.
-- So the following line implements this
-- change in place of articulation.
spirantizedPhonet (Consonant x Alveolar Plosive z) =
  Consonant x Dental Fricative z

spirantizedPhonet (Consonant x place1 Plosive z)
  = Consonant x place1 Fricative z
spirantizedPhonet other = other



unmarkDifferences ∷ Phonet → Phonet → UnmarkablePhonet
unmarkDifferences (Consonant voice1 place1 manner1 airstream1) (Consonant voice2 place2 manner2 airstream2)=
  let voice'     = if voice1     ≡ voice2     then MarkedVocalFolds voice1     else UnmarkedVocalFolds
      place'     = if place1     ≡ place2     then MarkedPlace      place1     else UnmarkedPlace
      manner'    = if manner1    ≡ manner2    then MarkedManner     manner1    else UnmarkedManner
      airstream' = if airstream1 ≡ airstream2 then MarkedAirstream  airstream1 else UnmarkedAirstream
  in UnmarkableConsonant voice' place' manner' airstream'

unmarkDifferences (Vowel height1 backness1 rounding1 voice1) (Vowel height2 backness2 rounding2 voice2) =
  let voice'    = if voice1    ≡ voice2    then MarkedVocalFolds voice1    else UnmarkedVocalFolds
      height'   = if height1   ≡ height2   then MarkedHeight     height1   else UnmarkedHeight
      backness' = if backness1 ≡ backness2 then MarkedBackness   backness1 else UnmarkedBackness
      rounding' = if rounding1 ≡ rounding2 then MarkedRounding   rounding1 else UnmarkedRounding
  in UnmarkableVowel height' backness' rounding' voice'

unmarkDifferences (Vowel _ _ _ voice1) (Consonant voice2 _ _ _) =
  let voice' = if voice1 ≡ voice2 then MarkedVocalFolds voice1 else UnmarkedVocalFolds
  in UnmarkableVowel UnmarkedHeight UnmarkedBackness UnmarkedRounding voice'


unmarkDifferences c@(Consonant _ _ _ _) v@(Vowel _ _ _ _) =
  unmarkDifferences v c -- Change the order of arguments



-- This function (I realize it is poorly named)
-- takes any unmarked attributes in the phoneme definition,
-- and returns a list with all possibilities for that attribute.
generateFromUnmarked ∷ UnmarkablePhonet → [Phonet]
generateFromUnmarked (UnmarkableConsonant voice1 place1 manner1 airstream1) =
  let voice'     = unmarkableVoiceToList     voice1
      place'     = unmarkablePlaceToList     place1
      manner'    = unmarkableMannerToList    manner1
      airstream' = unmarkableAirstreamToList airstream1
  in [Consonant v p m a | p ← place', v ← voice',  m ← manner', a ← airstream']

generateFromUnmarked (UnmarkableVowel height1 backness1 rounding1 voice1) =
  let voice'    = unmarkableVoiceToList    voice1
      height'   = unmarkableHeightToList   height1
      backness' = unmarkableBacknessToList backness1
      rounding' = unmarkableRoundingToList rounding1
  in [Vowel h b r v | h ← height', b ← backness', r ← rounding', v ← voice']


unmarkableVoiceToList ∷ UnmarkableVocalFolds → [VocalFolds]
unmarkableVoiceToList voice1 = 
  case voice1 of 
       MarkedVocalFolds x → [x]
       UnmarkedVocalFolds → vocalFoldStates

unmarkablePlaceToList ∷ UnmarkablePlace → [Place]
unmarkablePlaceToList place1 =
  case place1 of
       MarkedPlace x → [x]
       UnmarkedPlace → placeStates


unmarkableMannerToList ∷ UnmarkableManner → [Manner]
unmarkableMannerToList manner1 =
  case manner1 of
       MarkedManner x → [x]
       UnmarkedManner → mannerStates

unmarkableAirstreamToList ∷ UnmarkableAirstream → [Airstream]
unmarkableAirstreamToList airstream1 = 
  case airstream1 of
       MarkedAirstream x → [x]
       UnmarkedAirstream → airstreamStates


unmarkableHeightToList ∷ UnmarkableHeight → [Height]
unmarkableHeightToList height1 = 
  case height1 of
       MarkedHeight x → [x]
       UnmarkedHeight → heightStates

unmarkableBacknessToList ∷ UnmarkableBackness → [Backness]
unmarkableBacknessToList backness1 =
  case backness1 of 
       MarkedBackness x → [x]
       UnmarkedBackness → backnessStates

unmarkableRoundingToList ∷ UnmarkableRounding → [Rounding]
unmarkableRoundingToList rounding1 = 
  case rounding1 of
       MarkedRounding x → [x]
       UnmarkedRounding → roundingStates

-- The following function returns whether an articulation is
-- considered impossible according to the IPA (pulmonic) consonants chart.
-- Does not work for other values.
impossible ∷ Phonet → Bool
impossible (Consonant Voiced          Pharyngeal  Plosive            PulmonicEgressive) = True
impossible (Consonant VoicedAspirated Pharyngeal  Plosive            PulmonicEgressive) = True
impossible (Consonant Voiceless       Glottal     Plosive            PulmonicEgressive) = False  -- [ʔ] is not impossible.
impossible (Consonant _               Glottal     Fricative          PulmonicEgressive) = False  -- [h] and [ɦ] are not impossible.
impossible (Consonant _               Glottal     _                  PulmonicEgressive) = True   -- all other pulmonary egressive consonants are impossible..
impossible (Consonant _               Pharyngeal  Nasal              PulmonicEgressive) = True
impossible (Consonant _               Pharyngeal  LateralFricative   PulmonicEgressive) = True
impossible (Consonant _               Pharyngeal  LateralApproximant PulmonicEgressive) = True
impossible (Consonant _               Velar       Trill              PulmonicEgressive) = True
impossible (Consonant _               Velar       TapOrFlap          PulmonicEgressive) = True
impossible (Consonant _               Bilabial    LateralFricative   PulmonicEgressive) = True
impossible (Consonant _               Bilabial    LateralApproximant PulmonicEgressive) = True
impossible (Consonant _               LabioDental LateralFricative   PulmonicEgressive) = True
impossible (Consonant _               LabioDental LateralApproximant PulmonicEgressive) = True
impossible _ = False -- Everything else is assumed to be possible.
