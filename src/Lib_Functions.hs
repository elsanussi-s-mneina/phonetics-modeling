-- | This module contains functions not
-- categorized in any other module.
module Lib_Functions where

import Types.Airstream ( Airstream(..))
import Types.Manner ( Manner(..) )
import Types.Phonet ( Phonet(..) )
import Types.Place ( Place(..) )
import Types.VocalFolds ( VocalFolds(..) )


import Prelude (Bool (..), Maybe (..), elem, (==), (||))
import Data.Text (Text)
import ShowFunctions (showPhonet)

import qualified GetPhonet (place, vocalFolds)
import qualified SetPhonet (toNasalized, toVoiced, toVoiceless, 
	toVoicedAspirated, toVoicelessAspirated, withManner, withPlace, withVocalFolds)


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
-- returns the place of articulation that is
-- the next more retracted.
retractedPlace :: Place -> Place
retractedPlace placeValue =
	case placeValue of
		Bilabial -> LabioDental
		LabioDental -> Dental
		Dental -> Alveolar
		Alveolar -> PostAlveolar
		PostAlveolar -> Retroflex
		Retroflex -> Palatal
		Palatal -> Velar
		Velar -> Uvular
		Uvular -> Pharyngeal
		Pharyngeal -> Glottal
		Glottal -> Epiglottal
		same -> same


-- | Gives the English description of a phone.
englishDescription :: Phonet -> Text
englishDescription = showPhonet

-- | A function that given an IPA symbol will convert it to the voiced
--   equivalent.
voicedPhonet :: Phonet -> Phonet
voicedPhonet p =
	if isAspirated p
		then SetPhonet.toVoicedAspirated p
		else SetPhonet.toVoiced p

-- | A function that given an IPA symbol will convert it to the voiceless
--   equivalent.
devoicedPhonet :: Phonet -> Phonet
devoicedPhonet p =
	if isAspirated p
		then SetPhonet.toVoicelessAspirated p
		else SetPhonet.toVoiceless p

-- | whether a phoneme is aspirated,
--   (regardless of whether or not it is voiced)
isAspirated :: Phonet -> Bool
isAspirated p =
	let vf = GetPhonet.vocalFolds p
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
	if GetPhonet.place p == Just Alveolar
		then SetPhonet.withManner Fricative (SetPhonet.withPlace Dental p)
		else SetPhonet.withManner Fricative p

-- | The following function returns whether an articulation is
--   considered impossible according to the IPA (pulmonic) consonants chart.
--   Does not work for other values.
impossible :: Phonet -> Bool
impossible p = case p of
	(Consonant Voiced Pharyngeal Plosive PulmonicEgressive _) -> True
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
	Consonant {} ->
		False -- Everything else is assumed to be possible.
	Vowel {} ->
		False

retractPhonet :: Phonet -> Maybe Phonet
retractPhonet (Consonant v p m a sa) = Just (Consonant v (retractedPlace p) m a sa)
retractPhonet _ = Nothing

deaspirate :: Phonet -> Phonet
deaspirate p =
	let vf = GetPhonet.vocalFolds p
	in case vf of
		VoicedAspirated -> SetPhonet.withVocalFolds Voiced p
		VoicelessAspirated -> SetPhonet.withVocalFolds Voiceless p
		Voiced -> p
		Voiceless -> p
		CreakyVoiced -> p
    
aspirate :: Phonet -> Phonet
aspirate p =
	let vf = GetPhonet.vocalFolds p
	in case vf of
		Voiced -> SetPhonet.withVocalFolds VoicedAspirated p
		Voiceless -> SetPhonet.withVocalFolds VoicelessAspirated p
		VoicedAspirated -> p
		VoicelessAspirated -> p
		CreakyVoiced -> p

decreak :: Phonet -> Phonet
decreak p =
	if GetPhonet.vocalFolds p == CreakyVoiced
		then SetPhonet.toVoiced p
		else p


nasalizePhonet :: Phonet -> Maybe Phonet
nasalizePhonet = SetPhonet.toNasalized

