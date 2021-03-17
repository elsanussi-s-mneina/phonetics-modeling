-- | This module defines the phoneme inventory for Cree.
module LanguageSpecific.CreeSpecific where
import Prelude ()
import Types.Airstream ( Airstream(..))
import Types.Backness ( Backness(..) )
import Types.Height ( Height(..) )
import Types.Manner ( Manner(..) )
import Types.Nasalization ( Nasalization(Oral))
import Types.Phonet ( Phonet(..) )
import Types.PhonetInventory ( PhonetInventory(..) )
import Types.Place ( Place(..) )
import Types.Rounding ( Rounding(..) )
import Types.SecondaryArticulation ( SecondaryArticulation(..) )
import Types.VocalFolds ( VocalFolds(..) )
import Types.VowelLength ( VowelLength(..) )


-- | According to
-- the Wikipedia article
-- (found at the URL:  https://en.wikipedia.org/wiki/Plains_Cree )
-- these are the phonemes of Plains Cree.
-- (specific version of Wikipedia article used: https://en.wikipedia.org/w/index.php?title=Plains_Cree&oldid=972468142)
-- The article is accessed at Monday, September 28th, 2020.
plainsCreePhonemeInventory :: PhonetInventory
plainsCreePhonemeInventory =
	PhonetInventory
		[ Consonant Voiceless Bilabial Plosive PulmonicEgressive Normal -- /p/
		, Consonant Voiceless Alveolar Plosive PulmonicEgressive Normal -- /t/
		, Consonant Voiceless Velar Plosive PulmonicEgressive Normal -- /k/
		, Consonant Voiceless Glottal Plosive PulmonicEgressive Normal -- /ʔ/
		, Consonant Voiceless Alveolar Affricate PulmonicEgressive Normal -- /t͡s/
		, Consonant Voiced Bilabial Nasal PulmonicEgressive Normal -- /m/
		, Consonant Voiced Alveolar Nasal PulmonicEgressive Normal -- /n/
		, Consonant Voiceless Alveolar Fricative PulmonicEgressive Normal -- /s/
		, Consonant Voiceless Glottal Fricative PulmonicEgressive Normal -- /h/
		, Consonant Voiced LabialVelar Approximant PulmonicEgressive Normal -- /w/
		, Consonant Voiced Palatal Approximant PulmonicEgressive Normal -- /j/

		-- Short vowels:
		, Vowel Close Front Unrounded Voiced NormalLength Oral -- /i/
		, Vowel Open  Front Unrounded Voiced NormalLength Oral -- /a/
		, Vowel Close Back Rounded Voiced NormalLength Oral -- /u/

		-- Long vowels:
		, Vowel Close Front Unrounded Voiced Long Oral -- /iː/
		, Vowel CloseMid Front Unrounded Voiced Long Oral -- /eː/
		, Vowel Open Front Unrounded Voiced Long Oral -- /aː/
		, Vowel CloseMid Back Rounded Voiced Long Oral -- /oː/
		]

