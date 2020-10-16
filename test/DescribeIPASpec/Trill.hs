module DescribeIPASpec.Trill where

import Test.Hspec (Spec, describe, it, shouldBe)
import qualified IPA 
import Data.Text (pack, unpack)
describeIPA = unpack . IPA.describeIPA . pack

trillConsonantSpec :: Spec
trillConsonantSpec = do
	describe "voiceless bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙ̊] \
			 \is the representation of the \
			 \voiceless bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̊"
				`shouldBe`
				"voiceless bilabial trill pulmonic egressive consonant"
		it "should be that: [ʙ̥] \
			 \is the representation of the \
			 \voiceless bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̥"
				`shouldBe`
				"voiceless bilabial trill pulmonic egressive consonant"
	describe "voiceless labialized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̊ʷ"
				`shouldBe`
				"voiceless labialized bilabial trill pulmonic egressive consonant"
		it "should be that: [ʙ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̥ʷ"
				`shouldBe`
				"voiceless labialized bilabial trill pulmonic egressive consonant"
	describe "voiceless palatalized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̊ʲ"
				`shouldBe`
				"voiceless palatalized bilabial trill pulmonic egressive consonant"
		it "should be that: [ʙ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̥ʲ"
				`shouldBe`
				"voiceless palatalized bilabial trill pulmonic egressive consonant"
	describe "voiceless velarized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̊ˠ"
				`shouldBe`
				"voiceless velarized bilabial trill pulmonic egressive consonant"
		it "should be that: [ʙ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̥ˠ"
				`shouldBe`
				"voiceless velarized bilabial trill pulmonic egressive consonant"
	describe "voiceless pharyngealized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized bilabial trill pulmonic egressive consonant"
		it "should be that: [ʙ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized bilabial trill pulmonic egressive consonant"
	describe "voiceless aspirated bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̥ʰ"
				`shouldBe`
				"voiceless aspirated bilabial trill pulmonic egressive consonant"
		it "should be that: [ʙ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̊ʰ"
				`shouldBe`
				"voiceless aspirated bilabial trill pulmonic egressive consonant"
	describe "voiceless aspirated labialized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized bilabial trill pulmonic egressive consonant"
		it "should be that: [ʙ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized bilabial trill pulmonic egressive consonant"
	describe "voiceless aspirated palatalized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized bilabial trill pulmonic egressive consonant"
		it "should be that: [ʙ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized bilabial trill pulmonic egressive consonant"
	describe "voiceless aspirated velarized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized bilabial trill pulmonic egressive consonant"
		it "should be that: [ʙ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized bilabial trill pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized bilabial trill pulmonic egressive consonant"
		it "should be that: [ʙ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized bilabial trill pulmonic egressive consonant"
	describe "voiced bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙ] \
			 \is the representation of the \
			 \voiced bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ"
				`shouldBe`
				"voiced bilabial trill pulmonic egressive consonant"
	describe "voiced labialized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙʷ] \
			 \is the representation of the \
			 \voiced labialized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙʷ"
				`shouldBe`
				"voiced labialized bilabial trill pulmonic egressive consonant"
	describe "voiced palatalized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙʲ] \
			 \is the representation of the \
			 \voiced palatalized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙʲ"
				`shouldBe`
				"voiced palatalized bilabial trill pulmonic egressive consonant"
	describe "voiced velarized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙˠ] \
			 \is the representation of the \
			 \voiced velarized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙˠ"
				`shouldBe`
				"voiced velarized bilabial trill pulmonic egressive consonant"
	describe "voiced pharyngealized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙˤ] \
			 \is the representation of the \
			 \voiced pharyngealized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙˤ"
				`shouldBe`
				"voiced pharyngealized bilabial trill pulmonic egressive consonant"
	describe "voiced aspirated bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙʰ] \
			 \is the representation of the \
			 \voiced aspirated bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙʰ"
				`shouldBe`
				"voiced aspirated bilabial trill pulmonic egressive consonant"
		it "should be that: [ʙ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̬ʰ"
				`shouldBe`
				"voiced aspirated bilabial trill pulmonic egressive consonant"
	describe "voiced aspirated labialized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙʰʷ"
				`shouldBe`
				"voiced aspirated labialized bilabial trill pulmonic egressive consonant"
		it "should be that: [ʙ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized bilabial trill pulmonic egressive consonant"
	describe "voiced aspirated palatalized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙʰʲ"
				`shouldBe`
				"voiced aspirated palatalized bilabial trill pulmonic egressive consonant"
		it "should be that: [ʙ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized bilabial trill pulmonic egressive consonant"
	describe "voiced aspirated velarized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙʰˠ"
				`shouldBe`
				"voiced aspirated velarized bilabial trill pulmonic egressive consonant"
		it "should be that: [ʙ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized bilabial trill pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized bilabial trill pulmonic egressive consonant" $
		do
		it "should be that: [ʙʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized bilabial trill pulmonic egressive consonant"
		it "should be that: [ʙ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized bilabial trill pulmonic egressive consonant" $
			describeIPA "ʙ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized bilabial trill pulmonic egressive consonant"
	describe "voiceless alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [r̊] \
			 \is the representation of the \
			 \voiceless alveolar trill pulmonic egressive consonant" $
			describeIPA "r̊"
				`shouldBe`
				"voiceless alveolar trill pulmonic egressive consonant"
		it "should be that: [r̥] \
			 \is the representation of the \
			 \voiceless alveolar trill pulmonic egressive consonant" $
			describeIPA "r̥"
				`shouldBe`
				"voiceless alveolar trill pulmonic egressive consonant"
	describe "voiceless labialized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [r̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̊ʷ"
				`shouldBe`
				"voiceless labialized alveolar trill pulmonic egressive consonant"
		it "should be that: [r̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̥ʷ"
				`shouldBe`
				"voiceless labialized alveolar trill pulmonic egressive consonant"
	describe "voiceless palatalized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [r̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̊ʲ"
				`shouldBe`
				"voiceless palatalized alveolar trill pulmonic egressive consonant"
		it "should be that: [r̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̥ʲ"
				`shouldBe`
				"voiceless palatalized alveolar trill pulmonic egressive consonant"
	describe "voiceless velarized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [r̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̊ˠ"
				`shouldBe`
				"voiceless velarized alveolar trill pulmonic egressive consonant"
		it "should be that: [r̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̥ˠ"
				`shouldBe`
				"voiceless velarized alveolar trill pulmonic egressive consonant"
	describe "voiceless pharyngealized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [r̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̊ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar trill pulmonic egressive consonant"
		it "should be that: [r̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̥ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar trill pulmonic egressive consonant"
	describe "voiceless aspirated alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [r̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolar trill pulmonic egressive consonant" $
			describeIPA "r̥ʰ"
				`shouldBe`
				"voiceless aspirated alveolar trill pulmonic egressive consonant"
		it "should be that: [r̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolar trill pulmonic egressive consonant" $
			describeIPA "r̊ʰ"
				`shouldBe`
				"voiceless aspirated alveolar trill pulmonic egressive consonant"
	describe "voiceless aspirated labialized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [r̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolar trill pulmonic egressive consonant"
		it "should be that: [r̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolar trill pulmonic egressive consonant"
	describe "voiceless aspirated palatalized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [r̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolar trill pulmonic egressive consonant"
		it "should be that: [r̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolar trill pulmonic egressive consonant"
	describe "voiceless aspirated velarized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [r̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolar trill pulmonic egressive consonant"
		it "should be that: [r̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolar trill pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [r̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolar trill pulmonic egressive consonant"
		it "should be that: [r̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolar trill pulmonic egressive consonant"
	describe "voiced alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [r] \
			 \is the representation of the \
			 \voiced alveolar trill pulmonic egressive consonant" $
			describeIPA "r"
				`shouldBe`
				"voiced alveolar trill pulmonic egressive consonant"
	describe "voiced labialized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [rʷ] \
			 \is the representation of the \
			 \voiced labialized alveolar trill pulmonic egressive consonant" $
			describeIPA "rʷ"
				`shouldBe`
				"voiced labialized alveolar trill pulmonic egressive consonant"
	describe "voiced palatalized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [rʲ] \
			 \is the representation of the \
			 \voiced palatalized alveolar trill pulmonic egressive consonant" $
			describeIPA "rʲ"
				`shouldBe`
				"voiced palatalized alveolar trill pulmonic egressive consonant"
	describe "voiced velarized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [rˠ] \
			 \is the representation of the \
			 \voiced velarized alveolar trill pulmonic egressive consonant" $
			describeIPA "rˠ"
				`shouldBe`
				"voiced velarized alveolar trill pulmonic egressive consonant"
	describe "voiced pharyngealized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [rˤ] \
			 \is the representation of the \
			 \voiced pharyngealized alveolar trill pulmonic egressive consonant" $
			describeIPA "rˤ"
				`shouldBe`
				"voiced pharyngealized alveolar trill pulmonic egressive consonant"
	describe "voiced aspirated alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [rʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolar trill pulmonic egressive consonant" $
			describeIPA "rʰ"
				`shouldBe`
				"voiced aspirated alveolar trill pulmonic egressive consonant"
		it "should be that: [r̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolar trill pulmonic egressive consonant" $
			describeIPA "r̬ʰ"
				`shouldBe`
				"voiced aspirated alveolar trill pulmonic egressive consonant"
	describe "voiced aspirated labialized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [rʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolar trill pulmonic egressive consonant" $
			describeIPA "rʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolar trill pulmonic egressive consonant"
		it "should be that: [r̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolar trill pulmonic egressive consonant"
	describe "voiced aspirated palatalized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [rʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolar trill pulmonic egressive consonant" $
			describeIPA "rʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolar trill pulmonic egressive consonant"
		it "should be that: [r̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolar trill pulmonic egressive consonant"
	describe "voiced aspirated velarized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [rʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolar trill pulmonic egressive consonant" $
			describeIPA "rʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolar trill pulmonic egressive consonant"
		it "should be that: [r̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolar trill pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized alveolar trill pulmonic egressive consonant" $
		do
		it "should be that: [rʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolar trill pulmonic egressive consonant" $
			describeIPA "rʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolar trill pulmonic egressive consonant"
		it "should be that: [r̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolar trill pulmonic egressive consonant" $
			describeIPA "r̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolar trill pulmonic egressive consonant"
	describe "voiceless uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀ̊] \
			 \is the representation of the \
			 \voiceless uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̊"
				`shouldBe`
				"voiceless uvular trill pulmonic egressive consonant"
		it "should be that: [ʀ̥] \
			 \is the representation of the \
			 \voiceless uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̥"
				`shouldBe`
				"voiceless uvular trill pulmonic egressive consonant"
	describe "voiceless labialized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̊ʷ"
				`shouldBe`
				"voiceless labialized uvular trill pulmonic egressive consonant"
		it "should be that: [ʀ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̥ʷ"
				`shouldBe`
				"voiceless labialized uvular trill pulmonic egressive consonant"
	describe "voiceless palatalized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̊ʲ"
				`shouldBe`
				"voiceless palatalized uvular trill pulmonic egressive consonant"
		it "should be that: [ʀ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̥ʲ"
				`shouldBe`
				"voiceless palatalized uvular trill pulmonic egressive consonant"
	describe "voiceless velarized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̊ˠ"
				`shouldBe`
				"voiceless velarized uvular trill pulmonic egressive consonant"
		it "should be that: [ʀ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̥ˠ"
				`shouldBe`
				"voiceless velarized uvular trill pulmonic egressive consonant"
	describe "voiceless pharyngealized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized uvular trill pulmonic egressive consonant"
		it "should be that: [ʀ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized uvular trill pulmonic egressive consonant"
	describe "voiceless aspirated uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̥ʰ"
				`shouldBe`
				"voiceless aspirated uvular trill pulmonic egressive consonant"
		it "should be that: [ʀ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̊ʰ"
				`shouldBe`
				"voiceless aspirated uvular trill pulmonic egressive consonant"
	describe "voiceless aspirated labialized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized uvular trill pulmonic egressive consonant"
		it "should be that: [ʀ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized uvular trill pulmonic egressive consonant"
	describe "voiceless aspirated palatalized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized uvular trill pulmonic egressive consonant"
		it "should be that: [ʀ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized uvular trill pulmonic egressive consonant"
	describe "voiceless aspirated velarized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized uvular trill pulmonic egressive consonant"
		it "should be that: [ʀ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized uvular trill pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized uvular trill pulmonic egressive consonant"
		it "should be that: [ʀ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized uvular trill pulmonic egressive consonant"
	describe "voiced uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀ] \
			 \is the representation of the \
			 \voiced uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ"
				`shouldBe`
				"voiced uvular trill pulmonic egressive consonant"
	describe "voiced labialized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀʷ] \
			 \is the representation of the \
			 \voiced labialized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀʷ"
				`shouldBe`
				"voiced labialized uvular trill pulmonic egressive consonant"
	describe "voiced palatalized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀʲ] \
			 \is the representation of the \
			 \voiced palatalized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀʲ"
				`shouldBe`
				"voiced palatalized uvular trill pulmonic egressive consonant"
	describe "voiced velarized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀˠ] \
			 \is the representation of the \
			 \voiced velarized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀˠ"
				`shouldBe`
				"voiced velarized uvular trill pulmonic egressive consonant"
	describe "voiced pharyngealized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀˤ] \
			 \is the representation of the \
			 \voiced pharyngealized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀˤ"
				`shouldBe`
				"voiced pharyngealized uvular trill pulmonic egressive consonant"
	describe "voiced aspirated uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀʰ] \
			 \is the representation of the \
			 \voiced aspirated uvular trill pulmonic egressive consonant" $
			describeIPA "ʀʰ"
				`shouldBe`
				"voiced aspirated uvular trill pulmonic egressive consonant"
		it "should be that: [ʀ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̬ʰ"
				`shouldBe`
				"voiced aspirated uvular trill pulmonic egressive consonant"
	describe "voiced aspirated labialized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀʰʷ"
				`shouldBe`
				"voiced aspirated labialized uvular trill pulmonic egressive consonant"
		it "should be that: [ʀ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized uvular trill pulmonic egressive consonant"
	describe "voiced aspirated palatalized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀʰʲ"
				`shouldBe`
				"voiced aspirated palatalized uvular trill pulmonic egressive consonant"
		it "should be that: [ʀ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized uvular trill pulmonic egressive consonant"
	describe "voiced aspirated velarized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀʰˠ"
				`shouldBe`
				"voiced aspirated velarized uvular trill pulmonic egressive consonant"
		it "should be that: [ʀ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized uvular trill pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized uvular trill pulmonic egressive consonant" $
		do
		it "should be that: [ʀʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized uvular trill pulmonic egressive consonant"
		it "should be that: [ʀ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized uvular trill pulmonic egressive consonant" $
			describeIPA "ʀ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized uvular trill pulmonic egressive consonant"
