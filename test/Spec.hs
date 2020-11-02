module Spec(main) where

import Data.Maybe (fromJust)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import IPA (analyzeIPA, constructIPA, voicedIPA, devoicedIPA, analyzeIPAToSPE, describeIPA)
import PhonetInventory (ipaTextToPhonetListReport)
import PhoneticFeatures (isGlide)
import GraphemeGrammarSpec (runGraphemeGrammarSpecs)
import PrimitiveParsersSpec (runPrimitiveParsersSpecs)
import IPATextToDescriptionSpec (pulmonicEgressiveConsonantSpec)
import IPANumberConstantsSpec (ipaNumbersSpec)
import IPAUnicodeConstantsSpec ( ipaUnicodeSpec )
import PlainsCreePhonemeInventorySpec (plainsCreePhonetInventoryReportSpec)
import IrishPhonemeInventorySpec (irishPhonetInventoryReportSpec)
import Types.All
import Types.Nasalization (Nasalization(..))
import Prelude (($), (++), (.), String, Bool(True, False))
import Data.Text (Text, pack, unpack)
import Data.Semigroup (Semigroup((<>)))

toString :: Text -> String
toString = unpack


main = do
	hspec irishPhonetInventoryReportSpec
	hspec plainsCreePhonetInventoryReportSpec
	hspec constructIPASpec
	hspec glideSpec
	hspec ipaTextToPhonetListReportSpec
	hspec voicingSpec
	hspec analyzeIPAToSPESpec
	hspec secondaryArticulationSpec
	hspec vowelLengthSpec
	hspec pulmonicEgressiveConsonantSpec
	hspec ipaUnicodeSpec
	hspec ipaNumbersSpec
	runGraphemeGrammarSpecs
	runPrimitiveParsersSpecs

constructIPASpec :: Spec
constructIPASpec =
	describe "constructing vowel's IPA" $ do
		it "should be that: the IPA representation of the close back rounded voiced vowel is \"u\"." $
			constructIPA (Vowel Close Back Rounded Voiced NormalLength Oral) `shouldBe` (pack "u")
		it "should be that: the IPA representation of the close back rounded voiced vowel is \"u̥\"." $
			constructIPA (Vowel Close Back Rounded Voiceless NormalLength Oral) `shouldBe` (pack "u̥")
		it "should be that: the IPA representation of the close back rounded voiced nasal vowel is \"ũ\"." $
			constructIPA (Vowel Close Back Rounded Voiced NormalLength Nasalized) `shouldBe` (pack "ũ")
		it "should be that: the IPA representation of the close back rounded voiced long nasal vowel is \"ũː\"." $
			constructIPA (Vowel Close Back Rounded Voiced Long Nasalized) `shouldBe` (pack "ũː")



isDescribedAs
	:: Text -- ^ IPA e.g. "pʰ"
	-> Text -- ^ expected english description e.g. "voiceless aspirated bilabial plosive pulmonic egressive consonant"
	-> Spec
(isDescribedAs) ipaText description =
		it ("should be that: [" <> toString ipaText <> "] is the representation of the " <> toString description) $
			 describeIPA ipaText `shouldBe` description

glideSpec :: Spec
glideSpec =
	describe "recognizing a glide" $ do
		it "should be that: [j] the voiced palatal approximant is a glide." $
			(isGlide . fromJust . analyzeIPA) (pack "j") `shouldBe` True
		it "should be that: [ʝ] the voiced palatal fricative is not a glide." $
			(isGlide . fromJust . analyzeIPA) (pack "ʝ") `shouldBe` False
		it "should be that: [w] is a glide." $
			(isGlide . fromJust . analyzeIPA) (pack "w") `shouldBe` True
		it "should be that: [c] is not a glide." $
			(isGlide . fromJust . analyzeIPA) (pack "c") `shouldBe` False
		it "should be that: [ɥ] is a glide." $
			(isGlide . fromJust . analyzeIPA) (pack "ɥ") `shouldBe` True

ipaTextToPhonetListReportSpec :: Spec
ipaTextToPhonetListReportSpec =
	describe "one phoneme test" $ do
		it "should be that [j] is the voiced palatal approximant pulmonic egressive consonant" $
				ipaTextToPhonetListReport (pack "j") `shouldBe` (pack "/j/ voiced palatal approximant pulmonic egressive consonant\n")
		it "should be that [kc] has two lines" $ 
				ipaTextToPhonetListReport (pack "kc") `shouldBe` (pack ("/k/ voiceless velar plosive pulmonic egressive consonant\n"
					<> "/c/ voiceless palatal plosive pulmonic egressive consonant\n"))
		it "should be that [t͡ʃ] is the voiceless post-alveolar affricate pulmonic egressive consonant" $
				ipaTextToPhonetListReport (pack "t͡ʃ") `shouldBe` (pack "/t͡ʃ/ voiceless post-alveolar affricate pulmonic egressive consonant\n")
		it "should be that [t͜ʃ] is also the voiceless post-alveolar affricate pulmonic egressive consonant" $
				ipaTextToPhonetListReport (pack "t͜ʃ") `shouldBe` (pack "/t͜ʃ/ voiceless post-alveolar affricate pulmonic egressive consonant\n")
		it "should be that (tʃ) with no tie bar is 2 phonemes." $
				ipaTextToPhonetListReport (pack "tʃ") `shouldBe` (pack "/t/ voiceless alveolar plosive pulmonic egressive consonant\n/ʃ/ voiceless post-alveolar fricative pulmonic egressive consonant\n")
		it "should be that (t͜ʃdd͜ʒ) is properly split into 3 phonemes" $
				ipaTextToPhonetListReport (pack "t͜ʃdd͜ʒ") `shouldBe` (pack "/t͜ʃ/ voiceless post-alveolar affricate pulmonic egressive consonant\n/d/ voiced alveolar plosive pulmonic egressive consonant\n/d͜ʒ/ voiced post-alveolar affricate pulmonic egressive consonant\n")
		it "should be that (t͜ʃdd͜ʒʒ) is properly split into 4 phonemes" $
				ipaTextToPhonetListReport (pack "t͜ʃdd͜ʒʒ") `shouldBe` (pack "/t͜ʃ/ voiceless post-alveolar affricate pulmonic egressive consonant\n/d/ voiced alveolar plosive pulmonic egressive consonant\n/d͜ʒ/ voiced post-alveolar affricate pulmonic egressive consonant\n/ʒ/ voiced post-alveolar fricative pulmonic egressive consonant\n")

xVoicedIsY :: Text -> Text -> Spec
xVoicedIsY unvoicedPhoneme voicedPhoneme =
	describe ("voicing [" ++ toString unvoicedPhoneme ++ "]") $ do
		it ("should be that: [" ++ toString unvoicedPhoneme ++ "] voiced is [" ++ toString voicedPhoneme ++ "]") $
			voicedIPA unvoicedPhoneme `shouldBe` voicedPhoneme

xDevoicedIsY :: Text -> Text -> Spec
xDevoicedIsY unvoicedPhoneme voicedPhoneme =
	describe ("devoicing [" ++ toString voicedPhoneme ++ "]") $ do
		it ("should be that: [" ++ toString voicedPhoneme ++ "] devoiced is [" ++ toString unvoicedPhoneme ++ "]") $
			devoicedIPA voicedPhoneme `shouldBe` unvoicedPhoneme

isVoicelessCounterpartOf :: Text -> Text -> Spec
isVoicelessCounterpartOf unvoicedPhoneme voicedPhoneme =
	do
	xVoicedIsY unvoicedPhoneme voicedPhoneme
	xDevoicedIsY unvoicedPhoneme voicedPhoneme

voicingSpec :: Spec
voicingSpec = do
	describe "voicing and devoicing a phoneme (no diacritics)" $ do
		(pack "t") `isVoicelessCounterpartOf` (pack "d")
		(pack "p") `isVoicelessCounterpartOf` (pack "b")
		(pack "ʈ") `isVoicelessCounterpartOf` (pack "ɖ")
		(pack "c") `isVoicelessCounterpartOf` (pack "ɟ")
		(pack "ʈ") `isVoicelessCounterpartOf` (pack "ɖ")
		(pack "k") `isVoicelessCounterpartOf` (pack "g")
		(pack "q") `isVoicelessCounterpartOf` (pack "ɢ")
		(pack "ɸ") `isVoicelessCounterpartOf` (pack "β")
		(pack "f") `isVoicelessCounterpartOf` (pack "v")
		(pack "θ") `isVoicelessCounterpartOf` (pack "ð")
		(pack "s") `isVoicelessCounterpartOf` (pack "z")
		(pack "ʃ") `isVoicelessCounterpartOf` (pack "ʒ")
		(pack "ʂ") `isVoicelessCounterpartOf` (pack "ʐ")
		(pack "ç") `isVoicelessCounterpartOf` (pack "ʝ")
		(pack "ɕ") `isVoicelessCounterpartOf` (pack "ʑ")
		(pack "x") `isVoicelessCounterpartOf` (pack "ɣ")
		(pack "x") `isVoicelessCounterpartOf` (pack "ɣ")
		(pack "χ") `isVoicelessCounterpartOf` (pack "ʁ")
		(pack "ħ") `isVoicelessCounterpartOf` (pack "ʕ")
		(pack "h") `isVoicelessCounterpartOf` (pack "ɦ")
		(pack "ɬ") `isVoicelessCounterpartOf` (pack "ɮ")

	describe "voicing and devoicing a phoneme (with voiceless diacritic)" $ do
	 {-
	 Test that phonemes that in IPA require the diacritic symbol
	 to express voicelessness are handled correctly
	 -}
		-- Nasal consonants:
		(pack "m̥") `isVoicelessCounterpartOf` (pack "m")
		(pack "ɱ̊") `isVoicelessCounterpartOf` (pack "ɱ")
		(pack "n̥") `isVoicelessCounterpartOf` (pack "n")
		(pack "ɲ̊") `isVoicelessCounterpartOf` (pack "ɲ")
		(pack "ɳ̊") `isVoicelessCounterpartOf` (pack "ɳ")
		(pack "ŋ̊") `isVoicelessCounterpartOf` (pack "ŋ")
		(pack "ɴ̥") `isVoicelessCounterpartOf` (pack "ɴ")

		-- Trill consonants:
		(pack "ʙ̥") `isVoicelessCounterpartOf` (pack "ʙ")
		(pack "r̥") `isVoicelessCounterpartOf` (pack "r")
		(pack "ʀ̥") `isVoicelessCounterpartOf` (pack "ʀ")

		-- Tap or flap consonants:
		(pack "ⱱ̥") `isVoicelessCounterpartOf` (pack "ⱱ")
		(pack "ɾ̥") `isVoicelessCounterpartOf` (pack "ɾ")
		(pack "ɽ̊") `isVoicelessCounterpartOf` (pack "ɽ")

		-- Approximant consonants:
		(pack "ʋ̥") `isVoicelessCounterpartOf` (pack "ʋ")
		(pack "ɹ̥") `isVoicelessCounterpartOf` (pack "ɹ")
		(pack "ɻ̊") `isVoicelessCounterpartOf` (pack "ɻ")
		(pack "j̊") `isVoicelessCounterpartOf` (pack "j")
		(pack "ɰ̊") `isVoicelessCounterpartOf` (pack "ɰ")

		-- Lateral approximants:
		(pack "l̥") `isVoicelessCounterpartOf` (pack "l")
		(pack "ɭ̥") `isVoicelessCounterpartOf` (pack "ɭ")
		(pack "ʎ̥") `isVoicelessCounterpartOf` (pack "ʎ")
		(pack "ʟ̥") `isVoicelessCounterpartOf` (pack "ʟ")

		-- Vowels
		(pack "i̥") `isVoicelessCounterpartOf` (pack "i")
		(pack "ẙ") `isVoicelessCounterpartOf` (pack "y")
		(pack "ɨ̥") `isVoicelessCounterpartOf` (pack "ɨ")
		(pack "ʉ̥") `isVoicelessCounterpartOf` (pack "ʉ")
		(pack "ɯ̥") `isVoicelessCounterpartOf` (pack "ɯ")
		(pack "u̥") `isVoicelessCounterpartOf` (pack "u")
		(pack "ɪ̥") `isVoicelessCounterpartOf` (pack "ɪ")
		(pack "ʏ̥") `isVoicelessCounterpartOf` (pack "ʏ")
		(pack "ʊ̥") `isVoicelessCounterpartOf` (pack "ʊ")
		(pack "e̥") `isVoicelessCounterpartOf` (pack "e")
		(pack "ø̥") `isVoicelessCounterpartOf` (pack "ø")
		(pack "ɘ̥") `isVoicelessCounterpartOf` (pack "ɘ")
		(pack "ɵ̥") `isVoicelessCounterpartOf` (pack "ɵ")
		(pack "ɤ̥") `isVoicelessCounterpartOf` (pack "ɤ")
		(pack "o̥") `isVoicelessCounterpartOf` (pack "o")
		(pack "ə̥") `isVoicelessCounterpartOf` (pack "ə")
		(pack "ɛ̥") `isVoicelessCounterpartOf` (pack "ɛ")
		(pack "œ̥") `isVoicelessCounterpartOf` (pack "œ")
		(pack "ɜ̥") `isVoicelessCounterpartOf` (pack "ɜ")
		(pack "ɞ̥") `isVoicelessCounterpartOf` (pack "ɞ")
		(pack "ʌ̥") `isVoicelessCounterpartOf` (pack "ʌ")
		(pack "ɔ̥") `isVoicelessCounterpartOf` (pack "ɔ")
		(pack "æ̥") `isVoicelessCounterpartOf` (pack "æ")
		(pack "ɐ̥") `isVoicelessCounterpartOf` (pack "ɐ")
		(pack "ḁ") `isVoicelessCounterpartOf` (pack "a")
		(pack "ɶ̥") `isVoicelessCounterpartOf` (pack "ɶ")
		(pack "ɑ̥") `isVoicelessCounterpartOf` (pack "ɑ")
		(pack "ɒ̥") `isVoicelessCounterpartOf` (pack "ɒ")
		(pack "w̥") `isVoicelessCounterpartOf` (pack "w")
		(pack "ɥ̊") `isVoicelessCounterpartOf` (pack "ɥ")
		(pack "ɕ") `isVoicelessCounterpartOf` (pack "ʑ")
		(pack "ɺ̥") `isVoicelessCounterpartOf` (pack "ɺ")

	describe "voicing a phoneme (with voiceless diacritic above or below)" $ do
		-- Nasal consonants:
		(pack "m̥") `xVoicedIsY` (pack "m")
		(pack "m̊") `xVoicedIsY` (pack "m")
		(pack "ɱ̥") `xVoicedIsY` (pack "ɱ")
		(pack "ɱ̊") `xVoicedIsY` (pack "ɱ")
		(pack "n̥") `xVoicedIsY` (pack "n")
		(pack "n̊") `xVoicedIsY` (pack "n")
		(pack "ɲ̥") `xVoicedIsY` (pack "ɲ")
		(pack "ɲ̊") `xVoicedIsY` (pack "ɲ")
		(pack "ɳ̥") `xVoicedIsY` (pack "ɳ")
		(pack "ɳ̊") `xVoicedIsY` (pack "ɳ")
		(pack "ŋ̥") `xVoicedIsY` (pack "ŋ")
		(pack "ŋ̊") `xVoicedIsY` (pack "ŋ")
		(pack "ɴ̥") `xVoicedIsY` (pack "ɴ")
		(pack "ɴ̊") `xVoicedIsY` (pack "ɴ")

		-- Trill consonants:
		(pack "ʙ̊") `xVoicedIsY` (pack "ʙ")
		(pack "ʙ̥") `xVoicedIsY` (pack "ʙ")
		(pack "r̊") `xVoicedIsY` (pack "r")
		(pack "r̥") `xVoicedIsY` (pack "r")
		(pack "ʀ̊") `xVoicedIsY` (pack "ʀ")
		(pack "ʀ̥") `xVoicedIsY` (pack "ʀ")

		-- Tap or flap consonants:
		(pack "ⱱ̥") `xVoicedIsY` (pack "ⱱ")
		(pack "ⱱ̊") `xVoicedIsY` (pack "ⱱ")
		(pack "ɾ̥") `xVoicedIsY` (pack "ɾ")
		(pack "ɾ̊") `xVoicedIsY` (pack "ɾ")
		(pack "ɽ̥") `xVoicedIsY` (pack "ɽ")
		(pack "ɽ̊") `xVoicedIsY` (pack "ɽ")

		-- Approximant consonants:
		(pack "ʋ̥") `xVoicedIsY` (pack "ʋ")
		(pack "ʋ̊") `xVoicedIsY` (pack "ʋ")
		(pack "ɹ̥") `xVoicedIsY` (pack "ɹ")
		(pack "ɹ̊") `xVoicedIsY` (pack "ɹ")
		(pack "ɻ̥") `xVoicedIsY` (pack "ɻ")
		(pack "ɻ̊") `xVoicedIsY` (pack "ɻ")
		(pack "j̥") `xVoicedIsY` (pack "j")
		(pack "j̊") `xVoicedIsY` (pack "j")
		(pack "ɰ̥") `xVoicedIsY` (pack "ɰ")
		(pack "ɰ̊") `xVoicedIsY` (pack "ɰ")

		-- Lateral approximants:
		(pack "l̥") `xVoicedIsY` (pack "l")
		(pack "l̊") `xVoicedIsY` (pack "l")
		(pack "ɭ̥") `xVoicedIsY` (pack "ɭ")
		(pack "ɭ̊") `xVoicedIsY` (pack "ɭ")
		(pack "ʎ̥") `xVoicedIsY` (pack "ʎ")
		(pack "ʎ̊") `xVoicedIsY` (pack "ʎ")
		(pack "ʟ̥") `xVoicedIsY` (pack "ʟ")
		(pack "ʟ̊") `xVoicedIsY` (pack "ʟ")

		-- Vowels
		(pack "i̥") `xVoicedIsY` (pack "i")
		(pack "i̊") `xVoicedIsY` (pack "i")
		(pack "y̥") `xVoicedIsY` (pack "y")
		(pack "ẙ") `xVoicedIsY` (pack "y")
		(pack "ɨ̥") `xVoicedIsY` (pack "ɨ")
		(pack "ɨ̊") `xVoicedIsY` (pack "ɨ")
		(pack "ʉ̥") `xVoicedIsY` (pack "ʉ")
		(pack "ʉ̊") `xVoicedIsY` (pack "ʉ")
		(pack "ɯ̥") `xVoicedIsY` (pack "ɯ")
		(pack "ɯ̊") `xVoicedIsY` (pack "ɯ")
		(pack "u̥") `xVoicedIsY` (pack "u")
		(pack "ů") `xVoicedIsY` (pack "u")
		(pack "ɪ̥") `xVoicedIsY` (pack "ɪ")
		(pack "ɪ̊") `xVoicedIsY` (pack "ɪ")
		(pack "ʏ̥") `xVoicedIsY` (pack "ʏ")
		(pack "ʏ̊") `xVoicedIsY` (pack "ʏ")
		(pack "ʊ̥") `xVoicedIsY` (pack "ʊ")
		(pack "ʊ̊") `xVoicedIsY` (pack "ʊ")
		(pack "e̥") `xVoicedIsY` (pack "e")
		(pack "e̊") `xVoicedIsY` (pack "e")
		(pack "ø̥") `xVoicedIsY` (pack "ø")
		(pack "ø̊") `xVoicedIsY` (pack "ø")
		(pack "ɘ̥") `xVoicedIsY` (pack "ɘ")
		(pack "ɘ̊") `xVoicedIsY` (pack "ɘ")
		(pack "ɵ̥") `xVoicedIsY` (pack "ɵ")
		(pack "ɵ̊") `xVoicedIsY` (pack "ɵ")
		(pack "ɤ̥") `xVoicedIsY` (pack "ɤ")
		(pack "ɤ̊") `xVoicedIsY` (pack "ɤ")
		(pack "o̥") `xVoicedIsY` (pack "o")
		(pack "o̊") `xVoicedIsY` (pack "o")
		(pack "ə̥") `xVoicedIsY` (pack "ə")
		(pack "ə̊") `xVoicedIsY` (pack "ə")
		(pack "ɛ̥") `xVoicedIsY` (pack "ɛ")
		(pack "ɛ̊") `xVoicedIsY` (pack "ɛ")
		(pack "œ̥") `xVoicedIsY` (pack "œ")
		(pack "œ̊") `xVoicedIsY` (pack "œ")
		(pack "ɜ̥") `xVoicedIsY` (pack "ɜ")
		(pack "ɜ̊") `xVoicedIsY` (pack "ɜ")
		(pack "ɞ̥") `xVoicedIsY` (pack "ɞ")
		(pack "ɞ̊") `xVoicedIsY` (pack "ɞ")
		(pack "ʌ̥") `xVoicedIsY` (pack "ʌ")
		(pack "ʌ̊") `xVoicedIsY` (pack "ʌ")
		(pack "ɔ̥") `xVoicedIsY` (pack "ɔ")
		(pack "ɔ̊") `xVoicedIsY` (pack "ɔ")
		(pack "æ̥") `xVoicedIsY` (pack "æ")
		(pack "æ̊") `xVoicedIsY` (pack "æ")
		(pack "ɐ̥") `xVoicedIsY` (pack "ɐ")
		(pack "ɐ̊") `xVoicedIsY` (pack "ɐ")
		(pack "ḁ") `xVoicedIsY` (pack "a")
		(pack "å") `xVoicedIsY` (pack "a")
		(pack "ɶ̥") `xVoicedIsY` (pack "ɶ")
		(pack "ɶ̊") `xVoicedIsY` (pack "ɶ")
		(pack "ɑ̥") `xVoicedIsY` (pack "ɑ")
		(pack "ɑ̊") `xVoicedIsY` (pack "ɑ")
		(pack "ɒ̥") `xVoicedIsY` (pack "ɒ")
		(pack "ɒ̊") `xVoicedIsY` (pack "ɒ")
		(pack "w̥") `xVoicedIsY` (pack "w")
		(pack "ẘ") `xVoicedIsY` (pack "w")
		(pack "ɥ̥") `xVoicedIsY` (pack "ɥ")
		(pack "ɥ̊") `xVoicedIsY` (pack "ɥ")

		(pack "ɕ") `xVoicedIsY` (pack "ʑ")
		(pack "ɕ̥") `xVoicedIsY` (pack "ʑ")
		(pack "ɕ̊") `xVoicedIsY` (pack "ʑ")

		(pack "ɺ̥") `xVoicedIsY` (pack "ɺ")
		(pack "ɺ̊") `xVoicedIsY` (pack "ɺ")


	describe "voicing and devoicing a phoneme (with voiced diacritic)" $ do
		(pack "ʔ") `isVoicelessCounterpartOf` (pack "ʔ̬")
		(pack "ʡ") `isVoicelessCounterpartOf` (pack "ʡ̬")
		(pack "ʍ") `isVoicelessCounterpartOf` (pack "ʍ̬")


	describe "voicing and devoicing a phoneme (when no change (idempotency))" $ do
		it "should be that: [q] devoiced is the same as itself" $
			devoicedIPA (pack "q") `shouldBe` (pack "q")
		it "should be that: [ɢ] voiced is the same as itself" $
			voicedIPA (pack "ɢ") `shouldBe` (pack "ɢ")
	describe "voicing something twice is the same as voicing it once" $ do
		it "case: [k]" $
			voicedIPA (voicedIPA (pack "k")) `shouldBe` voicedIPA (pack "k")
		it "case: [g]" $
			voicedIPA (voicedIPA (pack "g")) `shouldBe` voicedIPA (pack "g")
	describe "devoicing something twice is the same as devoicing it once" $ do
		it "case: [k]" $
			devoicedIPA (devoicedIPA (pack "k")) `shouldBe` devoicedIPA (pack "k")

analyzeIPAToSPESpec :: Spec
analyzeIPAToSPESpec =
	describe "calculating sound patterns of English features" $ do
		it "case: [t]" $
			analyzeIPAToSPE (pack "t") `shouldBe` (pack "[+consonantal; -syllabic; -continuant; -sonorant; +anterior; -distributed; coronal; -round; -voice]")
		it "case: [d]" $
			analyzeIPAToSPE (pack "d") `shouldBe` (pack "[+consonantal; -syllabic; -continuant; -sonorant; +anterior; -distributed; coronal; -round; +voice]")


secondaryArticulationSpec :: Spec
secondaryArticulationSpec = do
	describe "labialization" $ do
		it "case: t labialized" $
			describeIPA (pack "tʷ") `shouldBe` (pack "voiceless labialized alveolar plosive pulmonic egressive consonant")
		it "case: r labialized" $
			describeIPA (pack "rʷ") `shouldBe` (pack "voiced labialized alveolar trill pulmonic egressive consonant")
	describe "palatalization" $ do
		it "case: t palatalized" $
			describeIPA (pack "tʲ") `shouldBe` (pack "voiceless palatalized alveolar plosive pulmonic egressive consonant")
		it "case: r palatalized" $
			describeIPA (pack "rʲ") `shouldBe` (pack "voiced palatalized alveolar trill pulmonic egressive consonant")
	describe "velarization" $ do
		it "case: t velarized" $
			describeIPA (pack "tˠ") `shouldBe` (pack "voiceless velarized alveolar plosive pulmonic egressive consonant")
		it "case: r velarized" $
			describeIPA (pack "rˠ") `shouldBe` (pack "voiced velarized alveolar trill pulmonic egressive consonant")
	describe "palatalization" $ do
		it "case: t pharyngealized" $
			describeIPA (pack "tˤ") `shouldBe` (pack "voiceless pharyngealized alveolar plosive pulmonic egressive consonant")
		it "case: r pharyngealized" $
			describeIPA (pack "rˤ") `shouldBe` (pack "voiced pharyngealized alveolar trill pulmonic egressive consonant")

vowelLengthSpec :: Spec
vowelLengthSpec = do
	describe "vowel length" $ do
		it "test_normal_a_vowel case: [a]" $
			describeIPA (pack "a") `shouldBe` (pack "voiced unrounded open front oral vowel")
		it "test_long_a_vowel case: [aː]" $
			describeIPA (pack "aː") `shouldBe` (pack "voiced unrounded open front long oral vowel")
		it "test_half_long_a_vowel case: [aˑ]" $
			describeIPA (pack "aˑ") `shouldBe` (pack "voiced unrounded open front half-long oral vowel")
		it "test_extra_short_a_vowel case: [ă]" $
			describeIPA (pack "ă") `shouldBe` (pack "voiced unrounded open front extra-short oral vowel")
		it "test_voiceless_long_i_vowel case [i̥ː]" $
			 describeIPA (pack "i̥ː") `shouldBe` (pack "voiceless unrounded close front long oral vowel")
		it "test_voiceless_half_long_i_vowel case [i̥ˑ]" $
			 describeIPA (pack "i̥ˑ") `shouldBe` (pack "voiceless unrounded close front half-long oral vowel")
		it "test_voiceless_half_long_i_vowel case [ĭ̥]" $
			describeIPA (pack "ĭ̥") `shouldBe` (pack "voiceless unrounded close front extra-short oral vowel")
