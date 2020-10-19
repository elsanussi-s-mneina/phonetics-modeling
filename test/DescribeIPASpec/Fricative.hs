module DescribeIPASpec.Fricative where

import Test.Hspec (Spec, describe, it, shouldBe)
import qualified IPA 
import Data.Text (pack, unpack)

describeIPA :: String -> String
describeIPA = unpack . IPA.describeIPA . pack

fricativeConsonantSpec :: Spec
fricativeConsonantSpec = do
	describe "voiceless bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɸ] \
			 \is the representation of the \
			 \voiceless bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ"
				`shouldBe`
				"voiceless bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̊] \
			 \is the representation of the \
			 \voiceless bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̊"
				`shouldBe`
				"voiceless bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̥] \
			 \is the representation of the \
			 \voiceless bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̥"
				`shouldBe`
				"voiceless bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̊] \
			 \is the representation of the \
			 \voiceless bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̊"
				`shouldBe`
				"voiceless bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̥] \
			 \is the representation of the \
			 \voiceless bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̥"
				`shouldBe`
				"voiceless bilabial fricative pulmonic egressive consonant"
	describe "voiceless labialized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɸʷ] \
			 \is the representation of the \
			 \voiceless labialized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸʷ"
				`shouldBe`
				"voiceless labialized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̊ʷ"
				`shouldBe`
				"voiceless labialized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̥ʷ"
				`shouldBe`
				"voiceless labialized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̊ʷ"
				`shouldBe`
				"voiceless labialized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̥ʷ"
				`shouldBe`
				"voiceless labialized bilabial fricative pulmonic egressive consonant"
	describe "voiceless palatalized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɸʲ] \
			 \is the representation of the \
			 \voiceless palatalized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸʲ"
				`shouldBe`
				"voiceless palatalized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̊ʲ"
				`shouldBe`
				"voiceless palatalized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̥ʲ"
				`shouldBe`
				"voiceless palatalized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̊ʲ"
				`shouldBe`
				"voiceless palatalized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̥ʲ"
				`shouldBe`
				"voiceless palatalized bilabial fricative pulmonic egressive consonant"
	describe "voiceless velarized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɸˠ] \
			 \is the representation of the \
			 \voiceless velarized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸˠ"
				`shouldBe`
				"voiceless velarized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̊ˠ"
				`shouldBe`
				"voiceless velarized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̥ˠ"
				`shouldBe`
				"voiceless velarized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̊ˠ"
				`shouldBe`
				"voiceless velarized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̥ˠ"
				`shouldBe`
				"voiceless velarized bilabial fricative pulmonic egressive consonant"
	describe "voiceless pharyngealized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɸˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸˤ"
				`shouldBe`
				"voiceless pharyngealized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̊ˤ"
				`shouldBe`
				"voiceless pharyngealized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̥ˤ"
				`shouldBe`
				"voiceless pharyngealized bilabial fricative pulmonic egressive consonant"
	describe "voiceless aspirated bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɸʰ] \
			 \is the representation of the \
			 \voiceless aspirated bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸʰ"
				`shouldBe`
				"voiceless aspirated bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̥ʰ"
				`shouldBe`
				"voiceless aspirated bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̊ʰ"
				`shouldBe`
				"voiceless aspirated bilabial fricative pulmonic egressive consonant"
	describe "voiceless aspirated labialized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɸʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸʰʷ"
				`shouldBe`
				"voiceless aspirated labialized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized bilabial fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatalized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɸʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized bilabial fricative pulmonic egressive consonant"
	describe "voiceless aspirated velarized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɸʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸʰˠ"
				`shouldBe`
				"voiceless aspirated velarized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized bilabial fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɸʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized bilabial fricative pulmonic egressive consonant"
	describe "voiced bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [β] \
			 \is the representation of the \
			 \voiced bilabial fricative pulmonic egressive consonant" $
			describeIPA "β"
				`shouldBe`
				"voiced bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̬] \
			 \is the representation of the \
			 \voiced bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̬"
				`shouldBe`
				"voiced bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̬] \
			 \is the representation of the \
			 \voiced bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̬"
				`shouldBe`
				"voiced bilabial fricative pulmonic egressive consonant"
	describe "voiced labialized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [βʷ] \
			 \is the representation of the \
			 \voiced labialized bilabial fricative pulmonic egressive consonant" $
			describeIPA "βʷ"
				`shouldBe`
				"voiced labialized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̬ʷ"
				`shouldBe`
				"voiced labialized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̬ʷ"
				`shouldBe`
				"voiced labialized bilabial fricative pulmonic egressive consonant"
	describe "voiced palatalized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [βʲ] \
			 \is the representation of the \
			 \voiced palatalized bilabial fricative pulmonic egressive consonant" $
			describeIPA "βʲ"
				`shouldBe`
				"voiced palatalized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̬ʲ"
				`shouldBe`
				"voiced palatalized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̬ʲ"
				`shouldBe`
				"voiced palatalized bilabial fricative pulmonic egressive consonant"
	describe "voiced velarized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [βˠ] \
			 \is the representation of the \
			 \voiced velarized bilabial fricative pulmonic egressive consonant" $
			describeIPA "βˠ"
				`shouldBe`
				"voiced velarized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̬ˠ"
				`shouldBe`
				"voiced velarized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̬ˠ"
				`shouldBe`
				"voiced velarized bilabial fricative pulmonic egressive consonant"
	describe "voiced pharyngealized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [βˤ] \
			 \is the representation of the \
			 \voiced pharyngealized bilabial fricative pulmonic egressive consonant" $
			describeIPA "βˤ"
				`shouldBe`
				"voiced pharyngealized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̬ˤ"
				`shouldBe`
				"voiced pharyngealized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̬ˤ"
				`shouldBe`
				"voiced pharyngealized bilabial fricative pulmonic egressive consonant"
	describe "voiced aspirated bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [βʰ] \
			 \is the representation of the \
			 \voiced aspirated bilabial fricative pulmonic egressive consonant" $
			describeIPA "βʰ"
				`shouldBe`
				"voiced aspirated bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̬ʰ"
				`shouldBe`
				"voiced aspirated bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̬ʰ"
				`shouldBe`
				"voiced aspirated bilabial fricative pulmonic egressive consonant"
	describe "voiced aspirated labialized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [βʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized bilabial fricative pulmonic egressive consonant" $
			describeIPA "βʰʷ"
				`shouldBe`
				"voiced aspirated labialized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized bilabial fricative pulmonic egressive consonant"
	describe "voiced aspirated palatalized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [βʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized bilabial fricative pulmonic egressive consonant" $
			describeIPA "βʰʲ"
				`shouldBe`
				"voiced aspirated palatalized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized bilabial fricative pulmonic egressive consonant"
	describe "voiced aspirated velarized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [βʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized bilabial fricative pulmonic egressive consonant" $
			describeIPA "βʰˠ"
				`shouldBe`
				"voiced aspirated velarized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized bilabial fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
		do
		it "should be that: [βʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
			describeIPA "βʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized bilabial fricative pulmonic egressive consonant"
		it "should be that: [β̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
			describeIPA "β̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized bilabial fricative pulmonic egressive consonant"
		it "should be that: [ɸ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized bilabial fricative pulmonic egressive consonant" $
			describeIPA "ɸ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized bilabial fricative pulmonic egressive consonant"
	describe "voiceless labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [f] \
			 \is the representation of the \
			 \voiceless labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f"
				`shouldBe`
				"voiceless labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̊] \
			 \is the representation of the \
			 \voiceless labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̊"
				`shouldBe`
				"voiceless labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̥] \
			 \is the representation of the \
			 \voiceless labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̥"
				`shouldBe`
				"voiceless labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̊] \
			 \is the representation of the \
			 \voiceless labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̊"
				`shouldBe`
				"voiceless labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̥] \
			 \is the representation of the \
			 \voiceless labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̥"
				`shouldBe`
				"voiceless labio-dental fricative pulmonic egressive consonant"
	describe "voiceless labialized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [fʷ] \
			 \is the representation of the \
			 \voiceless labialized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "fʷ"
				`shouldBe`
				"voiceless labialized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̊ʷ"
				`shouldBe`
				"voiceless labialized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̥ʷ"
				`shouldBe`
				"voiceless labialized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̊ʷ"
				`shouldBe`
				"voiceless labialized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̥ʷ"
				`shouldBe`
				"voiceless labialized labio-dental fricative pulmonic egressive consonant"
	describe "voiceless palatalized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [fʲ] \
			 \is the representation of the \
			 \voiceless palatalized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "fʲ"
				`shouldBe`
				"voiceless palatalized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̊ʲ"
				`shouldBe`
				"voiceless palatalized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̥ʲ"
				`shouldBe`
				"voiceless palatalized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̊ʲ"
				`shouldBe`
				"voiceless palatalized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̥ʲ"
				`shouldBe`
				"voiceless palatalized labio-dental fricative pulmonic egressive consonant"
	describe "voiceless velarized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [fˠ] \
			 \is the representation of the \
			 \voiceless velarized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "fˠ"
				`shouldBe`
				"voiceless velarized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̊ˠ"
				`shouldBe`
				"voiceless velarized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̥ˠ"
				`shouldBe`
				"voiceless velarized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̊ˠ"
				`shouldBe`
				"voiceless velarized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̥ˠ"
				`shouldBe`
				"voiceless velarized labio-dental fricative pulmonic egressive consonant"
	describe "voiceless pharyngealized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [fˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "fˤ"
				`shouldBe`
				"voiceless pharyngealized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̊ˤ"
				`shouldBe`
				"voiceless pharyngealized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̥ˤ"
				`shouldBe`
				"voiceless pharyngealized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̊ˤ"
				`shouldBe`
				"voiceless pharyngealized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̥ˤ"
				`shouldBe`
				"voiceless pharyngealized labio-dental fricative pulmonic egressive consonant"
	describe "voiceless aspirated labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [fʰ] \
			 \is the representation of the \
			 \voiceless aspirated labio-dental fricative pulmonic egressive consonant" $
			describeIPA "fʰ"
				`shouldBe`
				"voiceless aspirated labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̥ʰ"
				`shouldBe`
				"voiceless aspirated labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̊ʰ"
				`shouldBe`
				"voiceless aspirated labio-dental fricative pulmonic egressive consonant"
	describe "voiceless aspirated labialized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [fʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "fʰʷ"
				`shouldBe`
				"voiceless aspirated labialized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized labio-dental fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [fʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "fʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized labio-dental fricative pulmonic egressive consonant"
	describe "voiceless aspirated velarized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [fʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "fʰˠ"
				`shouldBe`
				"voiceless aspirated velarized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized labio-dental fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [fʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "fʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized labio-dental fricative pulmonic egressive consonant"
	describe "voiced labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [v] \
			 \is the representation of the \
			 \voiced labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v"
				`shouldBe`
				"voiced labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̬] \
			 \is the representation of the \
			 \voiced labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̬"
				`shouldBe`
				"voiced labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̬] \
			 \is the representation of the \
			 \voiced labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̬"
				`shouldBe`
				"voiced labio-dental fricative pulmonic egressive consonant"
	describe "voiced labialized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [vʷ] \
			 \is the representation of the \
			 \voiced labialized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "vʷ"
				`shouldBe`
				"voiced labialized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̬ʷ] \
			 \is the representation of the \
			 \voiced labialized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̬ʷ"
				`shouldBe`
				"voiced labialized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̬ʷ] \
			 \is the representation of the \
			 \voiced labialized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̬ʷ"
				`shouldBe`
				"voiced labialized labio-dental fricative pulmonic egressive consonant"
	describe "voiced palatalized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [vʲ] \
			 \is the representation of the \
			 \voiced palatalized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "vʲ"
				`shouldBe`
				"voiced palatalized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̬ʲ"
				`shouldBe`
				"voiced palatalized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̬ʲ"
				`shouldBe`
				"voiced palatalized labio-dental fricative pulmonic egressive consonant"
	describe "voiced velarized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [vˠ] \
			 \is the representation of the \
			 \voiced velarized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "vˠ"
				`shouldBe`
				"voiced velarized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̬ˠ] \
			 \is the representation of the \
			 \voiced velarized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̬ˠ"
				`shouldBe`
				"voiced velarized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̬ˠ] \
			 \is the representation of the \
			 \voiced velarized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̬ˠ"
				`shouldBe`
				"voiced velarized labio-dental fricative pulmonic egressive consonant"
	describe "voiced pharyngealized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [vˤ] \
			 \is the representation of the \
			 \voiced pharyngealized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "vˤ"
				`shouldBe`
				"voiced pharyngealized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̬ˤ"
				`shouldBe`
				"voiced pharyngealized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̬ˤ"
				`shouldBe`
				"voiced pharyngealized labio-dental fricative pulmonic egressive consonant"
	describe "voiced aspirated labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [vʰ] \
			 \is the representation of the \
			 \voiced aspirated labio-dental fricative pulmonic egressive consonant" $
			describeIPA "vʰ"
				`shouldBe`
				"voiced aspirated labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̬ʰ"
				`shouldBe`
				"voiced aspirated labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̬ʰ"
				`shouldBe`
				"voiced aspirated labio-dental fricative pulmonic egressive consonant"
	describe "voiced aspirated labialized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [vʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "vʰʷ"
				`shouldBe`
				"voiced aspirated labialized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized labio-dental fricative pulmonic egressive consonant"
	describe "voiced aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [vʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "vʰʲ"
				`shouldBe`
				"voiced aspirated palatalized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized labio-dental fricative pulmonic egressive consonant"
	describe "voiced aspirated velarized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [vʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "vʰˠ"
				`shouldBe`
				"voiced aspirated velarized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized labio-dental fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [vʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "vʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [v̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "v̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized labio-dental fricative pulmonic egressive consonant"
		it "should be that: [f̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized labio-dental fricative pulmonic egressive consonant" $
			describeIPA "f̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized labio-dental fricative pulmonic egressive consonant"
	describe "voiceless dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [θ] \
			 \is the representation of the \
			 \voiceless dental fricative pulmonic egressive consonant" $
			describeIPA "θ"
				`shouldBe`
				"voiceless dental fricative pulmonic egressive consonant"
		it "should be that: [θ̊] \
			 \is the representation of the \
			 \voiceless dental fricative pulmonic egressive consonant" $
			describeIPA "θ̊"
				`shouldBe`
				"voiceless dental fricative pulmonic egressive consonant"
		it "should be that: [θ̥] \
			 \is the representation of the \
			 \voiceless dental fricative pulmonic egressive consonant" $
			describeIPA "θ̥"
				`shouldBe`
				"voiceless dental fricative pulmonic egressive consonant"
		it "should be that: [ð̊] \
			 \is the representation of the \
			 \voiceless dental fricative pulmonic egressive consonant" $
			describeIPA "ð̊"
				`shouldBe`
				"voiceless dental fricative pulmonic egressive consonant"
		it "should be that: [ð̥] \
			 \is the representation of the \
			 \voiceless dental fricative pulmonic egressive consonant" $
			describeIPA "ð̥"
				`shouldBe`
				"voiceless dental fricative pulmonic egressive consonant"
	describe "voiceless labialized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [θʷ] \
			 \is the representation of the \
			 \voiceless labialized dental fricative pulmonic egressive consonant" $
			describeIPA "θʷ"
				`shouldBe`
				"voiceless labialized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̊ʷ"
				`shouldBe`
				"voiceless labialized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̥ʷ"
				`shouldBe`
				"voiceless labialized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̊ʷ"
				`shouldBe`
				"voiceless labialized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̥ʷ"
				`shouldBe`
				"voiceless labialized dental fricative pulmonic egressive consonant"
	describe "voiceless palatalized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [θʲ] \
			 \is the representation of the \
			 \voiceless palatalized dental fricative pulmonic egressive consonant" $
			describeIPA "θʲ"
				`shouldBe`
				"voiceless palatalized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̊ʲ"
				`shouldBe`
				"voiceless palatalized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̥ʲ"
				`shouldBe`
				"voiceless palatalized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̊ʲ"
				`shouldBe`
				"voiceless palatalized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̥ʲ"
				`shouldBe`
				"voiceless palatalized dental fricative pulmonic egressive consonant"
	describe "voiceless velarized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [θˠ] \
			 \is the representation of the \
			 \voiceless velarized dental fricative pulmonic egressive consonant" $
			describeIPA "θˠ"
				`shouldBe`
				"voiceless velarized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̊ˠ"
				`shouldBe`
				"voiceless velarized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̥ˠ"
				`shouldBe`
				"voiceless velarized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̊ˠ"
				`shouldBe`
				"voiceless velarized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̥ˠ"
				`shouldBe`
				"voiceless velarized dental fricative pulmonic egressive consonant"
	describe "voiceless pharyngealized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [θˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized dental fricative pulmonic egressive consonant" $
			describeIPA "θˤ"
				`shouldBe`
				"voiceless pharyngealized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̊ˤ"
				`shouldBe`
				"voiceless pharyngealized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̥ˤ"
				`shouldBe`
				"voiceless pharyngealized dental fricative pulmonic egressive consonant"
	describe "voiceless aspirated dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [θʰ] \
			 \is the representation of the \
			 \voiceless aspirated dental fricative pulmonic egressive consonant" $
			describeIPA "θʰ"
				`shouldBe`
				"voiceless aspirated dental fricative pulmonic egressive consonant"
		it "should be that: [ð̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated dental fricative pulmonic egressive consonant" $
			describeIPA "ð̥ʰ"
				`shouldBe`
				"voiceless aspirated dental fricative pulmonic egressive consonant"
		it "should be that: [ð̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated dental fricative pulmonic egressive consonant" $
			describeIPA "ð̊ʰ"
				`shouldBe`
				"voiceless aspirated dental fricative pulmonic egressive consonant"
	describe "voiceless aspirated labialized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [θʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized dental fricative pulmonic egressive consonant" $
			describeIPA "θʰʷ"
				`shouldBe`
				"voiceless aspirated labialized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized dental fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatalized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [θʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized dental fricative pulmonic egressive consonant" $
			describeIPA "θʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized dental fricative pulmonic egressive consonant"
	describe "voiceless aspirated velarized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [θʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized dental fricative pulmonic egressive consonant" $
			describeIPA "θʰˠ"
				`shouldBe`
				"voiceless aspirated velarized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized dental fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [θʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized dental fricative pulmonic egressive consonant" $
			describeIPA "θʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized dental fricative pulmonic egressive consonant"
	describe "voiced dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [ð] \
			 \is the representation of the \
			 \voiced dental fricative pulmonic egressive consonant" $
			describeIPA "ð"
				`shouldBe`
				"voiced dental fricative pulmonic egressive consonant"
		it "should be that: [θ̬] \
			 \is the representation of the \
			 \voiced dental fricative pulmonic egressive consonant" $
			describeIPA "θ̬"
				`shouldBe`
				"voiced dental fricative pulmonic egressive consonant"
		it "should be that: [θ̬] \
			 \is the representation of the \
			 \voiced dental fricative pulmonic egressive consonant" $
			describeIPA "θ̬"
				`shouldBe`
				"voiced dental fricative pulmonic egressive consonant"
	describe "voiced labialized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [ðʷ] \
			 \is the representation of the \
			 \voiced labialized dental fricative pulmonic egressive consonant" $
			describeIPA "ðʷ"
				`shouldBe`
				"voiced labialized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̬ʷ"
				`shouldBe`
				"voiced labialized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̬ʷ"
				`shouldBe`
				"voiced labialized dental fricative pulmonic egressive consonant"
	describe "voiced palatalized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [ðʲ] \
			 \is the representation of the \
			 \voiced palatalized dental fricative pulmonic egressive consonant" $
			describeIPA "ðʲ"
				`shouldBe`
				"voiced palatalized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̬ʲ"
				`shouldBe`
				"voiced palatalized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̬ʲ"
				`shouldBe`
				"voiced palatalized dental fricative pulmonic egressive consonant"
	describe "voiced velarized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [ðˠ] \
			 \is the representation of the \
			 \voiced velarized dental fricative pulmonic egressive consonant" $
			describeIPA "ðˠ"
				`shouldBe`
				"voiced velarized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̬ˠ"
				`shouldBe`
				"voiced velarized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̬ˠ"
				`shouldBe`
				"voiced velarized dental fricative pulmonic egressive consonant"
	describe "voiced pharyngealized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [ðˤ] \
			 \is the representation of the \
			 \voiced pharyngealized dental fricative pulmonic egressive consonant" $
			describeIPA "ðˤ"
				`shouldBe`
				"voiced pharyngealized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̬ˤ"
				`shouldBe`
				"voiced pharyngealized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̬ˤ"
				`shouldBe`
				"voiced pharyngealized dental fricative pulmonic egressive consonant"
	describe "voiced aspirated dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [ðʰ] \
			 \is the representation of the \
			 \voiced aspirated dental fricative pulmonic egressive consonant" $
			describeIPA "ðʰ"
				`shouldBe`
				"voiced aspirated dental fricative pulmonic egressive consonant"
		it "should be that: [ð̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated dental fricative pulmonic egressive consonant" $
			describeIPA "ð̬ʰ"
				`shouldBe`
				"voiced aspirated dental fricative pulmonic egressive consonant"
		it "should be that: [θ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated dental fricative pulmonic egressive consonant" $
			describeIPA "θ̬ʰ"
				`shouldBe`
				"voiced aspirated dental fricative pulmonic egressive consonant"
	describe "voiced aspirated labialized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [ðʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized dental fricative pulmonic egressive consonant" $
			describeIPA "ðʰʷ"
				`shouldBe`
				"voiced aspirated labialized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized dental fricative pulmonic egressive consonant"
	describe "voiced aspirated palatalized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [ðʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized dental fricative pulmonic egressive consonant" $
			describeIPA "ðʰʲ"
				`shouldBe`
				"voiced aspirated palatalized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized dental fricative pulmonic egressive consonant"
	describe "voiced aspirated velarized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [ðʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized dental fricative pulmonic egressive consonant" $
			describeIPA "ðʰˠ"
				`shouldBe`
				"voiced aspirated velarized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized dental fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized dental fricative pulmonic egressive consonant" $
		do
		it "should be that: [ðʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized dental fricative pulmonic egressive consonant" $
			describeIPA "ðʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized dental fricative pulmonic egressive consonant"
		it "should be that: [ð̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized dental fricative pulmonic egressive consonant" $
			describeIPA "ð̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized dental fricative pulmonic egressive consonant"
		it "should be that: [θ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized dental fricative pulmonic egressive consonant" $
			describeIPA "θ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized dental fricative pulmonic egressive consonant"
	describe "voiceless alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [s] \
			 \is the representation of the \
			 \voiceless alveolar fricative pulmonic egressive consonant" $
			describeIPA "s"
				`shouldBe`
				"voiceless alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̊] \
			 \is the representation of the \
			 \voiceless alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̊"
				`shouldBe`
				"voiceless alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̥] \
			 \is the representation of the \
			 \voiceless alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̥"
				`shouldBe`
				"voiceless alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̊] \
			 \is the representation of the \
			 \voiceless alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̊"
				`shouldBe`
				"voiceless alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̥] \
			 \is the representation of the \
			 \voiceless alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̥"
				`shouldBe`
				"voiceless alveolar fricative pulmonic egressive consonant"
	describe "voiceless labialized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [sʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar fricative pulmonic egressive consonant" $
			describeIPA "sʷ"
				`shouldBe`
				"voiceless labialized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̊ʷ"
				`shouldBe`
				"voiceless labialized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̥ʷ"
				`shouldBe`
				"voiceless labialized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̊ʷ"
				`shouldBe`
				"voiceless labialized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̥ʷ"
				`shouldBe`
				"voiceless labialized alveolar fricative pulmonic egressive consonant"
	describe "voiceless palatalized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [sʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar fricative pulmonic egressive consonant" $
			describeIPA "sʲ"
				`shouldBe`
				"voiceless palatalized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̊ʲ"
				`shouldBe`
				"voiceless palatalized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̥ʲ"
				`shouldBe`
				"voiceless palatalized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̊ʲ"
				`shouldBe`
				"voiceless palatalized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̥ʲ"
				`shouldBe`
				"voiceless palatalized alveolar fricative pulmonic egressive consonant"
	describe "voiceless velarized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [sˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar fricative pulmonic egressive consonant" $
			describeIPA "sˠ"
				`shouldBe`
				"voiceless velarized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̊ˠ"
				`shouldBe`
				"voiceless velarized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̥ˠ"
				`shouldBe`
				"voiceless velarized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̊ˠ"
				`shouldBe`
				"voiceless velarized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̥ˠ"
				`shouldBe`
				"voiceless velarized alveolar fricative pulmonic egressive consonant"
	describe "voiceless pharyngealized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [sˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar fricative pulmonic egressive consonant" $
			describeIPA "sˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̊ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̥ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̊ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̥ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar fricative pulmonic egressive consonant"
	describe "voiceless aspirated alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [sʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolar fricative pulmonic egressive consonant" $
			describeIPA "sʰ"
				`shouldBe`
				"voiceless aspirated alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̥ʰ"
				`shouldBe`
				"voiceless aspirated alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̊ʰ"
				`shouldBe`
				"voiceless aspirated alveolar fricative pulmonic egressive consonant"
	describe "voiceless aspirated labialized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [sʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolar fricative pulmonic egressive consonant" $
			describeIPA "sʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolar fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatalized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [sʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolar fricative pulmonic egressive consonant" $
			describeIPA "sʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolar fricative pulmonic egressive consonant"
	describe "voiceless aspirated velarized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [sʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolar fricative pulmonic egressive consonant" $
			describeIPA "sʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolar fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [sʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
			describeIPA "sʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolar fricative pulmonic egressive consonant"
	describe "voiced alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [z] \
			 \is the representation of the \
			 \voiced alveolar fricative pulmonic egressive consonant" $
			describeIPA "z"
				`shouldBe`
				"voiced alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̬] \
			 \is the representation of the \
			 \voiced alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̬"
				`shouldBe`
				"voiced alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̬] \
			 \is the representation of the \
			 \voiced alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̬"
				`shouldBe`
				"voiced alveolar fricative pulmonic egressive consonant"
	describe "voiced labialized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [zʷ] \
			 \is the representation of the \
			 \voiced labialized alveolar fricative pulmonic egressive consonant" $
			describeIPA "zʷ"
				`shouldBe`
				"voiced labialized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̬ʷ] \
			 \is the representation of the \
			 \voiced labialized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̬ʷ"
				`shouldBe`
				"voiced labialized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̬ʷ] \
			 \is the representation of the \
			 \voiced labialized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̬ʷ"
				`shouldBe`
				"voiced labialized alveolar fricative pulmonic egressive consonant"
	describe "voiced palatalized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [zʲ] \
			 \is the representation of the \
			 \voiced palatalized alveolar fricative pulmonic egressive consonant" $
			describeIPA "zʲ"
				`shouldBe`
				"voiced palatalized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̬ʲ"
				`shouldBe`
				"voiced palatalized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̬ʲ"
				`shouldBe`
				"voiced palatalized alveolar fricative pulmonic egressive consonant"
	describe "voiced velarized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [zˠ] \
			 \is the representation of the \
			 \voiced velarized alveolar fricative pulmonic egressive consonant" $
			describeIPA "zˠ"
				`shouldBe`
				"voiced velarized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̬ˠ] \
			 \is the representation of the \
			 \voiced velarized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̬ˠ"
				`shouldBe`
				"voiced velarized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̬ˠ] \
			 \is the representation of the \
			 \voiced velarized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̬ˠ"
				`shouldBe`
				"voiced velarized alveolar fricative pulmonic egressive consonant"
	describe "voiced pharyngealized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [zˤ] \
			 \is the representation of the \
			 \voiced pharyngealized alveolar fricative pulmonic egressive consonant" $
			describeIPA "zˤ"
				`shouldBe`
				"voiced pharyngealized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̬ˤ"
				`shouldBe`
				"voiced pharyngealized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̬ˤ"
				`shouldBe`
				"voiced pharyngealized alveolar fricative pulmonic egressive consonant"
	describe "voiced aspirated alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [zʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolar fricative pulmonic egressive consonant" $
			describeIPA "zʰ"
				`shouldBe`
				"voiced aspirated alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̬ʰ"
				`shouldBe`
				"voiced aspirated alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̬ʰ"
				`shouldBe`
				"voiced aspirated alveolar fricative pulmonic egressive consonant"
	describe "voiced aspirated labialized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [zʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolar fricative pulmonic egressive consonant" $
			describeIPA "zʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolar fricative pulmonic egressive consonant"
	describe "voiced aspirated palatalized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [zʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolar fricative pulmonic egressive consonant" $
			describeIPA "zʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolar fricative pulmonic egressive consonant"
	describe "voiced aspirated velarized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [zʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolar fricative pulmonic egressive consonant" $
			describeIPA "zʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolar fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [zʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
			describeIPA "zʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolar fricative pulmonic egressive consonant"
		it "should be that: [z̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
			describeIPA "z̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolar fricative pulmonic egressive consonant"
		it "should be that: [s̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolar fricative pulmonic egressive consonant" $
			describeIPA "s̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolar fricative pulmonic egressive consonant"
	describe "voiceless post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʃ] \
			 \is the representation of the \
			 \voiceless post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ"
				`shouldBe`
				"voiceless post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̊] \
			 \is the representation of the \
			 \voiceless post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̊"
				`shouldBe`
				"voiceless post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̥] \
			 \is the representation of the \
			 \voiceless post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̥"
				`shouldBe`
				"voiceless post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̊] \
			 \is the representation of the \
			 \voiceless post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̊"
				`shouldBe`
				"voiceless post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̥] \
			 \is the representation of the \
			 \voiceless post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̥"
				`shouldBe`
				"voiceless post-alveolar fricative pulmonic egressive consonant"
	describe "voiceless labialized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʃʷ] \
			 \is the representation of the \
			 \voiceless labialized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃʷ"
				`shouldBe`
				"voiceless labialized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̊ʷ"
				`shouldBe`
				"voiceless labialized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̥ʷ"
				`shouldBe`
				"voiceless labialized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̊ʷ"
				`shouldBe`
				"voiceless labialized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̥ʷ"
				`shouldBe`
				"voiceless labialized post-alveolar fricative pulmonic egressive consonant"
	describe "voiceless palatalized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʃʲ] \
			 \is the representation of the \
			 \voiceless palatalized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃʲ"
				`shouldBe`
				"voiceless palatalized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̊ʲ"
				`shouldBe`
				"voiceless palatalized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̥ʲ"
				`shouldBe`
				"voiceless palatalized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̊ʲ"
				`shouldBe`
				"voiceless palatalized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̥ʲ"
				`shouldBe`
				"voiceless palatalized post-alveolar fricative pulmonic egressive consonant"
	describe "voiceless velarized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʃˠ] \
			 \is the representation of the \
			 \voiceless velarized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃˠ"
				`shouldBe`
				"voiceless velarized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̊ˠ"
				`shouldBe`
				"voiceless velarized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̥ˠ"
				`shouldBe`
				"voiceless velarized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̊ˠ"
				`shouldBe`
				"voiceless velarized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̥ˠ"
				`shouldBe`
				"voiceless velarized post-alveolar fricative pulmonic egressive consonant"
	describe "voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʃˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃˤ"
				`shouldBe`
				"voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized post-alveolar fricative pulmonic egressive consonant"
	describe "voiceless aspirated post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʃʰ] \
			 \is the representation of the \
			 \voiceless aspirated post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃʰ"
				`shouldBe`
				"voiceless aspirated post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̥ʰ"
				`shouldBe`
				"voiceless aspirated post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̊ʰ"
				`shouldBe`
				"voiceless aspirated post-alveolar fricative pulmonic egressive consonant"
	describe "voiceless aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʃʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃʰʷ"
				`shouldBe`
				"voiceless aspirated labialized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized post-alveolar fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʃʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized post-alveolar fricative pulmonic egressive consonant"
	describe "voiceless aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʃʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃʰˠ"
				`shouldBe`
				"voiceless aspirated velarized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized post-alveolar fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʃʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant"
	describe "voiced post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʒ] \
			 \is the representation of the \
			 \voiced post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ"
				`shouldBe`
				"voiced post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̬] \
			 \is the representation of the \
			 \voiced post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̬"
				`shouldBe`
				"voiced post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̬] \
			 \is the representation of the \
			 \voiced post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̬"
				`shouldBe`
				"voiced post-alveolar fricative pulmonic egressive consonant"
	describe "voiced labialized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʒʷ] \
			 \is the representation of the \
			 \voiced labialized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒʷ"
				`shouldBe`
				"voiced labialized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̬ʷ"
				`shouldBe`
				"voiced labialized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̬ʷ"
				`shouldBe`
				"voiced labialized post-alveolar fricative pulmonic egressive consonant"
	describe "voiced palatalized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʒʲ] \
			 \is the representation of the \
			 \voiced palatalized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒʲ"
				`shouldBe`
				"voiced palatalized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̬ʲ"
				`shouldBe`
				"voiced palatalized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̬ʲ"
				`shouldBe`
				"voiced palatalized post-alveolar fricative pulmonic egressive consonant"
	describe "voiced velarized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʒˠ] \
			 \is the representation of the \
			 \voiced velarized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒˠ"
				`shouldBe`
				"voiced velarized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̬ˠ"
				`shouldBe`
				"voiced velarized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̬ˠ"
				`shouldBe`
				"voiced velarized post-alveolar fricative pulmonic egressive consonant"
	describe "voiced pharyngealized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʒˤ] \
			 \is the representation of the \
			 \voiced pharyngealized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒˤ"
				`shouldBe`
				"voiced pharyngealized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̬ˤ"
				`shouldBe`
				"voiced pharyngealized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̬ˤ"
				`shouldBe`
				"voiced pharyngealized post-alveolar fricative pulmonic egressive consonant"
	describe "voiced aspirated post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʒʰ] \
			 \is the representation of the \
			 \voiced aspirated post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒʰ"
				`shouldBe`
				"voiced aspirated post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̬ʰ"
				`shouldBe`
				"voiced aspirated post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̬ʰ"
				`shouldBe`
				"voiced aspirated post-alveolar fricative pulmonic egressive consonant"
	describe "voiced aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʒʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒʰʷ"
				`shouldBe`
				"voiced aspirated labialized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized post-alveolar fricative pulmonic egressive consonant"
	describe "voiced aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʒʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒʰʲ"
				`shouldBe`
				"voiced aspirated palatalized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized post-alveolar fricative pulmonic egressive consonant"
	describe "voiced aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʒʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒʰˠ"
				`shouldBe`
				"voiced aspirated velarized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized post-alveolar fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʒʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʒ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʒ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant"
		it "should be that: [ʃ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant" $
			describeIPA "ʃ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized post-alveolar fricative pulmonic egressive consonant"
	describe "voiceless retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʂ] \
			 \is the representation of the \
			 \voiceless retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ"
				`shouldBe`
				"voiceless retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̊] \
			 \is the representation of the \
			 \voiceless retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̊"
				`shouldBe`
				"voiceless retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̥] \
			 \is the representation of the \
			 \voiceless retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̥"
				`shouldBe`
				"voiceless retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̊] \
			 \is the representation of the \
			 \voiceless retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̊"
				`shouldBe`
				"voiceless retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̥] \
			 \is the representation of the \
			 \voiceless retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̥"
				`shouldBe`
				"voiceless retroflex fricative pulmonic egressive consonant"
	describe "voiceless labialized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʂʷ] \
			 \is the representation of the \
			 \voiceless labialized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂʷ"
				`shouldBe`
				"voiceless labialized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̊ʷ"
				`shouldBe`
				"voiceless labialized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̥ʷ"
				`shouldBe`
				"voiceless labialized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̊ʷ"
				`shouldBe`
				"voiceless labialized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̥ʷ"
				`shouldBe`
				"voiceless labialized retroflex fricative pulmonic egressive consonant"
	describe "voiceless palatalized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʂʲ] \
			 \is the representation of the \
			 \voiceless palatalized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂʲ"
				`shouldBe`
				"voiceless palatalized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̊ʲ"
				`shouldBe`
				"voiceless palatalized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̥ʲ"
				`shouldBe`
				"voiceless palatalized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̊ʲ"
				`shouldBe`
				"voiceless palatalized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̥ʲ"
				`shouldBe`
				"voiceless palatalized retroflex fricative pulmonic egressive consonant"
	describe "voiceless velarized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʂˠ] \
			 \is the representation of the \
			 \voiceless velarized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂˠ"
				`shouldBe`
				"voiceless velarized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̊ˠ"
				`shouldBe`
				"voiceless velarized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̥ˠ"
				`shouldBe`
				"voiceless velarized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̊ˠ"
				`shouldBe`
				"voiceless velarized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̥ˠ"
				`shouldBe`
				"voiceless velarized retroflex fricative pulmonic egressive consonant"
	describe "voiceless pharyngealized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʂˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂˤ"
				`shouldBe`
				"voiceless pharyngealized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized retroflex fricative pulmonic egressive consonant"
	describe "voiceless aspirated retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʂʰ] \
			 \is the representation of the \
			 \voiceless aspirated retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂʰ"
				`shouldBe`
				"voiceless aspirated retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̥ʰ"
				`shouldBe`
				"voiceless aspirated retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̊ʰ"
				`shouldBe`
				"voiceless aspirated retroflex fricative pulmonic egressive consonant"
	describe "voiceless aspirated labialized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʂʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂʰʷ"
				`shouldBe`
				"voiceless aspirated labialized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized retroflex fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatalized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʂʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized retroflex fricative pulmonic egressive consonant"
	describe "voiceless aspirated velarized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʂʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂʰˠ"
				`shouldBe`
				"voiceless aspirated velarized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized retroflex fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʂʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized retroflex fricative pulmonic egressive consonant"
	describe "voiced retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʐ] \
			 \is the representation of the \
			 \voiced retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ"
				`shouldBe`
				"voiced retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̬] \
			 \is the representation of the \
			 \voiced retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̬"
				`shouldBe`
				"voiced retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̬] \
			 \is the representation of the \
			 \voiced retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̬"
				`shouldBe`
				"voiced retroflex fricative pulmonic egressive consonant"
	describe "voiced labialized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʐʷ] \
			 \is the representation of the \
			 \voiced labialized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐʷ"
				`shouldBe`
				"voiced labialized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̬ʷ"
				`shouldBe`
				"voiced labialized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̬ʷ"
				`shouldBe`
				"voiced labialized retroflex fricative pulmonic egressive consonant"
	describe "voiced palatalized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʐʲ] \
			 \is the representation of the \
			 \voiced palatalized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐʲ"
				`shouldBe`
				"voiced palatalized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̬ʲ"
				`shouldBe`
				"voiced palatalized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̬ʲ"
				`shouldBe`
				"voiced palatalized retroflex fricative pulmonic egressive consonant"
	describe "voiced velarized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʐˠ] \
			 \is the representation of the \
			 \voiced velarized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐˠ"
				`shouldBe`
				"voiced velarized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̬ˠ"
				`shouldBe`
				"voiced velarized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̬ˠ"
				`shouldBe`
				"voiced velarized retroflex fricative pulmonic egressive consonant"
	describe "voiced pharyngealized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʐˤ] \
			 \is the representation of the \
			 \voiced pharyngealized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐˤ"
				`shouldBe`
				"voiced pharyngealized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̬ˤ"
				`shouldBe`
				"voiced pharyngealized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̬ˤ"
				`shouldBe`
				"voiced pharyngealized retroflex fricative pulmonic egressive consonant"
	describe "voiced aspirated retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʐʰ] \
			 \is the representation of the \
			 \voiced aspirated retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐʰ"
				`shouldBe`
				"voiced aspirated retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̬ʰ"
				`shouldBe`
				"voiced aspirated retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̬ʰ"
				`shouldBe`
				"voiced aspirated retroflex fricative pulmonic egressive consonant"
	describe "voiced aspirated labialized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʐʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐʰʷ"
				`shouldBe`
				"voiced aspirated labialized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized retroflex fricative pulmonic egressive consonant"
	describe "voiced aspirated palatalized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʐʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐʰʲ"
				`shouldBe`
				"voiced aspirated palatalized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized retroflex fricative pulmonic egressive consonant"
	describe "voiced aspirated velarized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʐʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐʰˠ"
				`shouldBe`
				"voiced aspirated velarized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized retroflex fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʐʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʐ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʐ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized retroflex fricative pulmonic egressive consonant"
		it "should be that: [ʂ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized retroflex fricative pulmonic egressive consonant" $
			describeIPA "ʂ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized retroflex fricative pulmonic egressive consonant"
	describe "voiceless palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ç] \
			 \is the representation of the \
			 \voiceless palatal fricative pulmonic egressive consonant" $
			describeIPA "ç"
				`shouldBe`
				"voiceless palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̊] \
			 \is the representation of the \
			 \voiceless palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̊"
				`shouldBe`
				"voiceless palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̥] \
			 \is the representation of the \
			 \voiceless palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̥"
				`shouldBe`
				"voiceless palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̊] \
			 \is the representation of the \
			 \voiceless palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̊"
				`shouldBe`
				"voiceless palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̥] \
			 \is the representation of the \
			 \voiceless palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̥"
				`shouldBe`
				"voiceless palatal fricative pulmonic egressive consonant"
	describe "voiceless labialized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [çʷ] \
			 \is the representation of the \
			 \voiceless labialized palatal fricative pulmonic egressive consonant" $
			describeIPA "çʷ"
				`shouldBe`
				"voiceless labialized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̊ʷ"
				`shouldBe`
				"voiceless labialized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̥ʷ"
				`shouldBe`
				"voiceless labialized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̊ʷ"
				`shouldBe`
				"voiceless labialized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̥ʷ"
				`shouldBe`
				"voiceless labialized palatal fricative pulmonic egressive consonant"
	describe "voiceless palatalized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [çʲ] \
			 \is the representation of the \
			 \voiceless palatalized palatal fricative pulmonic egressive consonant" $
			describeIPA "çʲ"
				`shouldBe`
				"voiceless palatalized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̊ʲ"
				`shouldBe`
				"voiceless palatalized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̥ʲ"
				`shouldBe`
				"voiceless palatalized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̊ʲ"
				`shouldBe`
				"voiceless palatalized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̥ʲ"
				`shouldBe`
				"voiceless palatalized palatal fricative pulmonic egressive consonant"
	describe "voiceless velarized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [çˠ] \
			 \is the representation of the \
			 \voiceless velarized palatal fricative pulmonic egressive consonant" $
			describeIPA "çˠ"
				`shouldBe`
				"voiceless velarized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̊ˠ"
				`shouldBe`
				"voiceless velarized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̥ˠ"
				`shouldBe`
				"voiceless velarized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̊ˠ"
				`shouldBe`
				"voiceless velarized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̥ˠ"
				`shouldBe`
				"voiceless velarized palatal fricative pulmonic egressive consonant"
	describe "voiceless pharyngealized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [çˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized palatal fricative pulmonic egressive consonant" $
			describeIPA "çˤ"
				`shouldBe`
				"voiceless pharyngealized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̊ˤ"
				`shouldBe`
				"voiceless pharyngealized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̥ˤ"
				`shouldBe`
				"voiceless pharyngealized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized palatal fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [çʰ] \
			 \is the representation of the \
			 \voiceless aspirated palatal fricative pulmonic egressive consonant" $
			describeIPA "çʰ"
				`shouldBe`
				"voiceless aspirated palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̥ʰ"
				`shouldBe`
				"voiceless aspirated palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̊ʰ"
				`shouldBe`
				"voiceless aspirated palatal fricative pulmonic egressive consonant"
	describe "voiceless aspirated labialized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [çʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized palatal fricative pulmonic egressive consonant" $
			describeIPA "çʰʷ"
				`shouldBe`
				"voiceless aspirated labialized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized palatal fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatalized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [çʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized palatal fricative pulmonic egressive consonant" $
			describeIPA "çʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized palatal fricative pulmonic egressive consonant"
	describe "voiceless aspirated velarized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [çʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized palatal fricative pulmonic egressive consonant" $
			describeIPA "çʰˠ"
				`shouldBe`
				"voiceless aspirated velarized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized palatal fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [çʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
			describeIPA "çʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized palatal fricative pulmonic egressive consonant"
	describe "voiced palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʝ] \
			 \is the representation of the \
			 \voiced palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ"
				`shouldBe`
				"voiced palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̬] \
			 \is the representation of the \
			 \voiced palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̬"
				`shouldBe`
				"voiced palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̬] \
			 \is the representation of the \
			 \voiced palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̬"
				`shouldBe`
				"voiced palatal fricative pulmonic egressive consonant"
	describe "voiced labialized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʝʷ] \
			 \is the representation of the \
			 \voiced labialized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝʷ"
				`shouldBe`
				"voiced labialized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̬ʷ] \
			 \is the representation of the \
			 \voiced labialized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̬ʷ"
				`shouldBe`
				"voiced labialized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̬ʷ] \
			 \is the representation of the \
			 \voiced labialized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̬ʷ"
				`shouldBe`
				"voiced labialized palatal fricative pulmonic egressive consonant"
	describe "voiced palatalized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʝʲ] \
			 \is the representation of the \
			 \voiced palatalized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝʲ"
				`shouldBe`
				"voiced palatalized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̬ʲ"
				`shouldBe`
				"voiced palatalized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̬ʲ"
				`shouldBe`
				"voiced palatalized palatal fricative pulmonic egressive consonant"
	describe "voiced velarized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʝˠ] \
			 \is the representation of the \
			 \voiced velarized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝˠ"
				`shouldBe`
				"voiced velarized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̬ˠ] \
			 \is the representation of the \
			 \voiced velarized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̬ˠ"
				`shouldBe`
				"voiced velarized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̬ˠ] \
			 \is the representation of the \
			 \voiced velarized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̬ˠ"
				`shouldBe`
				"voiced velarized palatal fricative pulmonic egressive consonant"
	describe "voiced pharyngealized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʝˤ] \
			 \is the representation of the \
			 \voiced pharyngealized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝˤ"
				`shouldBe`
				"voiced pharyngealized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̬ˤ"
				`shouldBe`
				"voiced pharyngealized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̬ˤ"
				`shouldBe`
				"voiced pharyngealized palatal fricative pulmonic egressive consonant"
	describe "voiced aspirated palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʝʰ] \
			 \is the representation of the \
			 \voiced aspirated palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝʰ"
				`shouldBe`
				"voiced aspirated palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̬ʰ"
				`shouldBe`
				"voiced aspirated palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̬ʰ"
				`shouldBe`
				"voiced aspirated palatal fricative pulmonic egressive consonant"
	describe "voiced aspirated labialized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʝʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝʰʷ"
				`shouldBe`
				"voiced aspirated labialized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized palatal fricative pulmonic egressive consonant"
	describe "voiced aspirated palatalized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʝʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝʰʲ"
				`shouldBe`
				"voiced aspirated palatalized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized palatal fricative pulmonic egressive consonant"
	describe "voiced aspirated velarized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʝʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝʰˠ"
				`shouldBe`
				"voiced aspirated velarized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized palatal fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʝʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized palatal fricative pulmonic egressive consonant"
		it "should be that: [ʝ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
			describeIPA "ʝ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized palatal fricative pulmonic egressive consonant"
		it "should be that: [ç̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized palatal fricative pulmonic egressive consonant" $
			describeIPA "ç̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized palatal fricative pulmonic egressive consonant"
	describe "voiceless velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [x] \
			 \is the representation of the \
			 \voiceless velar fricative pulmonic egressive consonant" $
			describeIPA "x"
				`shouldBe`
				"voiceless velar fricative pulmonic egressive consonant"
		it "should be that: [x̊] \
			 \is the representation of the \
			 \voiceless velar fricative pulmonic egressive consonant" $
			describeIPA "x̊"
				`shouldBe`
				"voiceless velar fricative pulmonic egressive consonant"
		it "should be that: [x̥] \
			 \is the representation of the \
			 \voiceless velar fricative pulmonic egressive consonant" $
			describeIPA "x̥"
				`shouldBe`
				"voiceless velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̊] \
			 \is the representation of the \
			 \voiceless velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̊"
				`shouldBe`
				"voiceless velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̥] \
			 \is the representation of the \
			 \voiceless velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̥"
				`shouldBe`
				"voiceless velar fricative pulmonic egressive consonant"
	describe "voiceless labialized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [xʷ] \
			 \is the representation of the \
			 \voiceless labialized velar fricative pulmonic egressive consonant" $
			describeIPA "xʷ"
				`shouldBe`
				"voiceless labialized velar fricative pulmonic egressive consonant"
		it "should be that: [x̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized velar fricative pulmonic egressive consonant" $
			describeIPA "x̊ʷ"
				`shouldBe`
				"voiceless labialized velar fricative pulmonic egressive consonant"
		it "should be that: [x̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized velar fricative pulmonic egressive consonant" $
			describeIPA "x̥ʷ"
				`shouldBe`
				"voiceless labialized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̊ʷ"
				`shouldBe`
				"voiceless labialized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̥ʷ"
				`shouldBe`
				"voiceless labialized velar fricative pulmonic egressive consonant"
	describe "voiceless palatalized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [xʲ] \
			 \is the representation of the \
			 \voiceless palatalized velar fricative pulmonic egressive consonant" $
			describeIPA "xʲ"
				`shouldBe`
				"voiceless palatalized velar fricative pulmonic egressive consonant"
		it "should be that: [x̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized velar fricative pulmonic egressive consonant" $
			describeIPA "x̊ʲ"
				`shouldBe`
				"voiceless palatalized velar fricative pulmonic egressive consonant"
		it "should be that: [x̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized velar fricative pulmonic egressive consonant" $
			describeIPA "x̥ʲ"
				`shouldBe`
				"voiceless palatalized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̊ʲ"
				`shouldBe`
				"voiceless palatalized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̥ʲ"
				`shouldBe`
				"voiceless palatalized velar fricative pulmonic egressive consonant"
	describe "voiceless velarized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [xˠ] \
			 \is the representation of the \
			 \voiceless velarized velar fricative pulmonic egressive consonant" $
			describeIPA "xˠ"
				`shouldBe`
				"voiceless velarized velar fricative pulmonic egressive consonant"
		it "should be that: [x̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized velar fricative pulmonic egressive consonant" $
			describeIPA "x̊ˠ"
				`shouldBe`
				"voiceless velarized velar fricative pulmonic egressive consonant"
		it "should be that: [x̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized velar fricative pulmonic egressive consonant" $
			describeIPA "x̥ˠ"
				`shouldBe`
				"voiceless velarized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̊ˠ"
				`shouldBe`
				"voiceless velarized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̥ˠ"
				`shouldBe`
				"voiceless velarized velar fricative pulmonic egressive consonant"
	describe "voiceless pharyngealized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [xˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized velar fricative pulmonic egressive consonant" $
			describeIPA "xˤ"
				`shouldBe`
				"voiceless pharyngealized velar fricative pulmonic egressive consonant"
		it "should be that: [x̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized velar fricative pulmonic egressive consonant" $
			describeIPA "x̊ˤ"
				`shouldBe`
				"voiceless pharyngealized velar fricative pulmonic egressive consonant"
		it "should be that: [x̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized velar fricative pulmonic egressive consonant" $
			describeIPA "x̥ˤ"
				`shouldBe`
				"voiceless pharyngealized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized velar fricative pulmonic egressive consonant"
	describe "voiceless aspirated velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [xʰ] \
			 \is the representation of the \
			 \voiceless aspirated velar fricative pulmonic egressive consonant" $
			describeIPA "xʰ"
				`shouldBe`
				"voiceless aspirated velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̥ʰ"
				`shouldBe`
				"voiceless aspirated velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̊ʰ"
				`shouldBe`
				"voiceless aspirated velar fricative pulmonic egressive consonant"
	describe "voiceless aspirated labialized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [xʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized velar fricative pulmonic egressive consonant" $
			describeIPA "xʰʷ"
				`shouldBe`
				"voiceless aspirated labialized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized velar fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatalized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [xʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized velar fricative pulmonic egressive consonant" $
			describeIPA "xʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized velar fricative pulmonic egressive consonant"
	describe "voiceless aspirated velarized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [xʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized velar fricative pulmonic egressive consonant" $
			describeIPA "xʰˠ"
				`shouldBe`
				"voiceless aspirated velarized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized velar fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [xʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized velar fricative pulmonic egressive consonant" $
			describeIPA "xʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized velar fricative pulmonic egressive consonant"
	describe "voiced velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɣ] \
			 \is the representation of the \
			 \voiced velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ"
				`shouldBe`
				"voiced velar fricative pulmonic egressive consonant"
		it "should be that: [x̬] \
			 \is the representation of the \
			 \voiced velar fricative pulmonic egressive consonant" $
			describeIPA "x̬"
				`shouldBe`
				"voiced velar fricative pulmonic egressive consonant"
		it "should be that: [x̬] \
			 \is the representation of the \
			 \voiced velar fricative pulmonic egressive consonant" $
			describeIPA "x̬"
				`shouldBe`
				"voiced velar fricative pulmonic egressive consonant"
	describe "voiced labialized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɣʷ] \
			 \is the representation of the \
			 \voiced labialized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣʷ"
				`shouldBe`
				"voiced labialized velar fricative pulmonic egressive consonant"
		it "should be that: [x̬ʷ] \
			 \is the representation of the \
			 \voiced labialized velar fricative pulmonic egressive consonant" $
			describeIPA "x̬ʷ"
				`shouldBe`
				"voiced labialized velar fricative pulmonic egressive consonant"
		it "should be that: [x̬ʷ] \
			 \is the representation of the \
			 \voiced labialized velar fricative pulmonic egressive consonant" $
			describeIPA "x̬ʷ"
				`shouldBe`
				"voiced labialized velar fricative pulmonic egressive consonant"
	describe "voiced palatalized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɣʲ] \
			 \is the representation of the \
			 \voiced palatalized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣʲ"
				`shouldBe`
				"voiced palatalized velar fricative pulmonic egressive consonant"
		it "should be that: [x̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized velar fricative pulmonic egressive consonant" $
			describeIPA "x̬ʲ"
				`shouldBe`
				"voiced palatalized velar fricative pulmonic egressive consonant"
		it "should be that: [x̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized velar fricative pulmonic egressive consonant" $
			describeIPA "x̬ʲ"
				`shouldBe`
				"voiced palatalized velar fricative pulmonic egressive consonant"
	describe "voiced velarized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɣˠ] \
			 \is the representation of the \
			 \voiced velarized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣˠ"
				`shouldBe`
				"voiced velarized velar fricative pulmonic egressive consonant"
		it "should be that: [x̬ˠ] \
			 \is the representation of the \
			 \voiced velarized velar fricative pulmonic egressive consonant" $
			describeIPA "x̬ˠ"
				`shouldBe`
				"voiced velarized velar fricative pulmonic egressive consonant"
		it "should be that: [x̬ˠ] \
			 \is the representation of the \
			 \voiced velarized velar fricative pulmonic egressive consonant" $
			describeIPA "x̬ˠ"
				`shouldBe`
				"voiced velarized velar fricative pulmonic egressive consonant"
	describe "voiced pharyngealized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɣˤ] \
			 \is the representation of the \
			 \voiced pharyngealized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣˤ"
				`shouldBe`
				"voiced pharyngealized velar fricative pulmonic egressive consonant"
		it "should be that: [x̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized velar fricative pulmonic egressive consonant" $
			describeIPA "x̬ˤ"
				`shouldBe`
				"voiced pharyngealized velar fricative pulmonic egressive consonant"
		it "should be that: [x̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized velar fricative pulmonic egressive consonant" $
			describeIPA "x̬ˤ"
				`shouldBe`
				"voiced pharyngealized velar fricative pulmonic egressive consonant"
	describe "voiced aspirated velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɣʰ] \
			 \is the representation of the \
			 \voiced aspirated velar fricative pulmonic egressive consonant" $
			describeIPA "ɣʰ"
				`shouldBe`
				"voiced aspirated velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̬ʰ"
				`shouldBe`
				"voiced aspirated velar fricative pulmonic egressive consonant"
		it "should be that: [x̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated velar fricative pulmonic egressive consonant" $
			describeIPA "x̬ʰ"
				`shouldBe`
				"voiced aspirated velar fricative pulmonic egressive consonant"
	describe "voiced aspirated labialized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɣʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣʰʷ"
				`shouldBe`
				"voiced aspirated labialized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized velar fricative pulmonic egressive consonant"
		it "should be that: [x̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized velar fricative pulmonic egressive consonant" $
			describeIPA "x̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized velar fricative pulmonic egressive consonant"
	describe "voiced aspirated palatalized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɣʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣʰʲ"
				`shouldBe`
				"voiced aspirated palatalized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized velar fricative pulmonic egressive consonant"
		it "should be that: [x̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized velar fricative pulmonic egressive consonant" $
			describeIPA "x̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized velar fricative pulmonic egressive consonant"
	describe "voiced aspirated velarized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɣʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣʰˠ"
				`shouldBe`
				"voiced aspirated velarized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized velar fricative pulmonic egressive consonant"
		it "should be that: [x̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized velar fricative pulmonic egressive consonant" $
			describeIPA "x̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized velar fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɣʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized velar fricative pulmonic egressive consonant"
		it "should be that: [ɣ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized velar fricative pulmonic egressive consonant" $
			describeIPA "ɣ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized velar fricative pulmonic egressive consonant"
		it "should be that: [x̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized velar fricative pulmonic egressive consonant" $
			describeIPA "x̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized velar fricative pulmonic egressive consonant"
	describe "voiceless uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [χ] \
			 \is the representation of the \
			 \voiceless uvular fricative pulmonic egressive consonant" $
			describeIPA "χ"
				`shouldBe`
				"voiceless uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̊] \
			 \is the representation of the \
			 \voiceless uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̊"
				`shouldBe`
				"voiceless uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̥] \
			 \is the representation of the \
			 \voiceless uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̥"
				`shouldBe`
				"voiceless uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̊] \
			 \is the representation of the \
			 \voiceless uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̊"
				`shouldBe`
				"voiceless uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̥] \
			 \is the representation of the \
			 \voiceless uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̥"
				`shouldBe`
				"voiceless uvular fricative pulmonic egressive consonant"
	describe "voiceless labialized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [χʷ] \
			 \is the representation of the \
			 \voiceless labialized uvular fricative pulmonic egressive consonant" $
			describeIPA "χʷ"
				`shouldBe`
				"voiceless labialized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̊ʷ"
				`shouldBe`
				"voiceless labialized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̥ʷ"
				`shouldBe`
				"voiceless labialized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̊ʷ"
				`shouldBe`
				"voiceless labialized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̥ʷ"
				`shouldBe`
				"voiceless labialized uvular fricative pulmonic egressive consonant"
	describe "voiceless palatalized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [χʲ] \
			 \is the representation of the \
			 \voiceless palatalized uvular fricative pulmonic egressive consonant" $
			describeIPA "χʲ"
				`shouldBe`
				"voiceless palatalized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̊ʲ"
				`shouldBe`
				"voiceless palatalized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̥ʲ"
				`shouldBe`
				"voiceless palatalized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̊ʲ"
				`shouldBe`
				"voiceless palatalized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̥ʲ"
				`shouldBe`
				"voiceless palatalized uvular fricative pulmonic egressive consonant"
	describe "voiceless velarized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [χˠ] \
			 \is the representation of the \
			 \voiceless velarized uvular fricative pulmonic egressive consonant" $
			describeIPA "χˠ"
				`shouldBe`
				"voiceless velarized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̊ˠ"
				`shouldBe`
				"voiceless velarized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̥ˠ"
				`shouldBe`
				"voiceless velarized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̊ˠ"
				`shouldBe`
				"voiceless velarized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̥ˠ"
				`shouldBe`
				"voiceless velarized uvular fricative pulmonic egressive consonant"
	describe "voiceless pharyngealized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [χˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized uvular fricative pulmonic egressive consonant" $
			describeIPA "χˤ"
				`shouldBe`
				"voiceless pharyngealized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized uvular fricative pulmonic egressive consonant"
	describe "voiceless aspirated uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [χʰ] \
			 \is the representation of the \
			 \voiceless aspirated uvular fricative pulmonic egressive consonant" $
			describeIPA "χʰ"
				`shouldBe`
				"voiceless aspirated uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̥ʰ"
				`shouldBe`
				"voiceless aspirated uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̊ʰ"
				`shouldBe`
				"voiceless aspirated uvular fricative pulmonic egressive consonant"
	describe "voiceless aspirated labialized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [χʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized uvular fricative pulmonic egressive consonant" $
			describeIPA "χʰʷ"
				`shouldBe`
				"voiceless aspirated labialized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized uvular fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatalized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [χʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized uvular fricative pulmonic egressive consonant" $
			describeIPA "χʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized uvular fricative pulmonic egressive consonant"
	describe "voiceless aspirated velarized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [χʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized uvular fricative pulmonic egressive consonant" $
			describeIPA "χʰˠ"
				`shouldBe`
				"voiceless aspirated velarized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized uvular fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [χʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
			describeIPA "χʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized uvular fricative pulmonic egressive consonant"
	describe "voiced uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʁ] \
			 \is the representation of the \
			 \voiced uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ"
				`shouldBe`
				"voiced uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̬] \
			 \is the representation of the \
			 \voiced uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̬"
				`shouldBe`
				"voiced uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̬] \
			 \is the representation of the \
			 \voiced uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̬"
				`shouldBe`
				"voiced uvular fricative pulmonic egressive consonant"
	describe "voiced labialized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʁʷ] \
			 \is the representation of the \
			 \voiced labialized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁʷ"
				`shouldBe`
				"voiced labialized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̬ʷ"
				`shouldBe`
				"voiced labialized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̬ʷ"
				`shouldBe`
				"voiced labialized uvular fricative pulmonic egressive consonant"
	describe "voiced palatalized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʁʲ] \
			 \is the representation of the \
			 \voiced palatalized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁʲ"
				`shouldBe`
				"voiced palatalized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̬ʲ"
				`shouldBe`
				"voiced palatalized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̬ʲ"
				`shouldBe`
				"voiced palatalized uvular fricative pulmonic egressive consonant"
	describe "voiced velarized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʁˠ] \
			 \is the representation of the \
			 \voiced velarized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁˠ"
				`shouldBe`
				"voiced velarized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̬ˠ"
				`shouldBe`
				"voiced velarized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̬ˠ"
				`shouldBe`
				"voiced velarized uvular fricative pulmonic egressive consonant"
	describe "voiced pharyngealized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʁˤ] \
			 \is the representation of the \
			 \voiced pharyngealized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁˤ"
				`shouldBe`
				"voiced pharyngealized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̬ˤ"
				`shouldBe`
				"voiced pharyngealized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̬ˤ"
				`shouldBe`
				"voiced pharyngealized uvular fricative pulmonic egressive consonant"
	describe "voiced aspirated uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʁʰ] \
			 \is the representation of the \
			 \voiced aspirated uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁʰ"
				`shouldBe`
				"voiced aspirated uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̬ʰ"
				`shouldBe`
				"voiced aspirated uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̬ʰ"
				`shouldBe`
				"voiced aspirated uvular fricative pulmonic egressive consonant"
	describe "voiced aspirated labialized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʁʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁʰʷ"
				`shouldBe`
				"voiced aspirated labialized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized uvular fricative pulmonic egressive consonant"
	describe "voiced aspirated palatalized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʁʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁʰʲ"
				`shouldBe`
				"voiced aspirated palatalized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized uvular fricative pulmonic egressive consonant"
	describe "voiced aspirated velarized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʁʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁʰˠ"
				`shouldBe`
				"voiced aspirated velarized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized uvular fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʁʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized uvular fricative pulmonic egressive consonant"
		it "should be that: [ʁ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
			describeIPA "ʁ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized uvular fricative pulmonic egressive consonant"
		it "should be that: [χ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized uvular fricative pulmonic egressive consonant" $
			describeIPA "χ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized uvular fricative pulmonic egressive consonant"
	describe "voiceless pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ħ] \
			 \is the representation of the \
			 \voiceless pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ"
				`shouldBe`
				"voiceless pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̊] \
			 \is the representation of the \
			 \voiceless pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̊"
				`shouldBe`
				"voiceless pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̥] \
			 \is the representation of the \
			 \voiceless pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̥"
				`shouldBe`
				"voiceless pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̊] \
			 \is the representation of the \
			 \voiceless pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̊"
				`shouldBe`
				"voiceless pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̥] \
			 \is the representation of the \
			 \voiceless pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̥"
				`shouldBe`
				"voiceless pharyngeal fricative pulmonic egressive consonant"
	describe "voiceless labialized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ħʷ] \
			 \is the representation of the \
			 \voiceless labialized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħʷ"
				`shouldBe`
				"voiceless labialized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̊ʷ"
				`shouldBe`
				"voiceless labialized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̥ʷ"
				`shouldBe`
				"voiceless labialized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̊ʷ"
				`shouldBe`
				"voiceless labialized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̥ʷ"
				`shouldBe`
				"voiceless labialized pharyngeal fricative pulmonic egressive consonant"
	describe "voiceless palatalized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ħʲ] \
			 \is the representation of the \
			 \voiceless palatalized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħʲ"
				`shouldBe`
				"voiceless palatalized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̊ʲ"
				`shouldBe`
				"voiceless palatalized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̥ʲ"
				`shouldBe`
				"voiceless palatalized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̊ʲ"
				`shouldBe`
				"voiceless palatalized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̥ʲ"
				`shouldBe`
				"voiceless palatalized pharyngeal fricative pulmonic egressive consonant"
	describe "voiceless velarized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ħˠ] \
			 \is the representation of the \
			 \voiceless velarized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħˠ"
				`shouldBe`
				"voiceless velarized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̊ˠ"
				`shouldBe`
				"voiceless velarized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̥ˠ"
				`shouldBe`
				"voiceless velarized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̊ˠ"
				`shouldBe`
				"voiceless velarized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̥ˠ"
				`shouldBe`
				"voiceless velarized pharyngeal fricative pulmonic egressive consonant"
	describe "voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ħˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħˤ"
				`shouldBe`
				"voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized pharyngeal fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ħʰ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħʰ"
				`shouldBe`
				"voiceless aspirated pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̥ʰ"
				`shouldBe`
				"voiceless aspirated pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̊ʰ"
				`shouldBe`
				"voiceless aspirated pharyngeal fricative pulmonic egressive consonant"
	describe "voiceless aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ħʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħʰʷ"
				`shouldBe`
				"voiceless aspirated labialized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized pharyngeal fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ħʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized pharyngeal fricative pulmonic egressive consonant"
	describe "voiceless aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ħʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħʰˠ"
				`shouldBe`
				"voiceless aspirated velarized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized pharyngeal fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ħʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant"
	describe "voiced pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʕ] \
			 \is the representation of the \
			 \voiced pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ"
				`shouldBe`
				"voiced pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̬] \
			 \is the representation of the \
			 \voiced pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̬"
				`shouldBe`
				"voiced pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̬] \
			 \is the representation of the \
			 \voiced pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̬"
				`shouldBe`
				"voiced pharyngeal fricative pulmonic egressive consonant"
	describe "voiced labialized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʕʷ] \
			 \is the representation of the \
			 \voiced labialized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕʷ"
				`shouldBe`
				"voiced labialized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̬ʷ"
				`shouldBe`
				"voiced labialized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̬ʷ"
				`shouldBe`
				"voiced labialized pharyngeal fricative pulmonic egressive consonant"
	describe "voiced palatalized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʕʲ] \
			 \is the representation of the \
			 \voiced palatalized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕʲ"
				`shouldBe`
				"voiced palatalized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̬ʲ"
				`shouldBe`
				"voiced palatalized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̬ʲ"
				`shouldBe`
				"voiced palatalized pharyngeal fricative pulmonic egressive consonant"
	describe "voiced velarized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʕˠ] \
			 \is the representation of the \
			 \voiced velarized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕˠ"
				`shouldBe`
				"voiced velarized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̬ˠ"
				`shouldBe`
				"voiced velarized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̬ˠ"
				`shouldBe`
				"voiced velarized pharyngeal fricative pulmonic egressive consonant"
	describe "voiced pharyngealized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʕˤ] \
			 \is the representation of the \
			 \voiced pharyngealized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕˤ"
				`shouldBe`
				"voiced pharyngealized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̬ˤ"
				`shouldBe`
				"voiced pharyngealized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̬ˤ"
				`shouldBe`
				"voiced pharyngealized pharyngeal fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʕʰ] \
			 \is the representation of the \
			 \voiced aspirated pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕʰ"
				`shouldBe`
				"voiced aspirated pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̬ʰ"
				`shouldBe`
				"voiced aspirated pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̬ʰ"
				`shouldBe`
				"voiced aspirated pharyngeal fricative pulmonic egressive consonant"
	describe "voiced aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʕʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕʰʷ"
				`shouldBe`
				"voiced aspirated labialized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized pharyngeal fricative pulmonic egressive consonant"
	describe "voiced aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʕʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕʰʲ"
				`shouldBe`
				"voiced aspirated palatalized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized pharyngeal fricative pulmonic egressive consonant"
	describe "voiced aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʕʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕʰˠ"
				`shouldBe`
				"voiced aspirated velarized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized pharyngeal fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʕʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ʕ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ʕ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant"
		it "should be that: [ħ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant" $
			describeIPA "ħ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized pharyngeal fricative pulmonic egressive consonant"
	describe "voiceless glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [h] \
			 \is the representation of the \
			 \voiceless glottal fricative pulmonic egressive consonant" $
			describeIPA "h"
				`shouldBe`
				"voiceless glottal fricative pulmonic egressive consonant"
		it "should be that: [h̊] \
			 \is the representation of the \
			 \voiceless glottal fricative pulmonic egressive consonant" $
			describeIPA "h̊"
				`shouldBe`
				"voiceless glottal fricative pulmonic egressive consonant"
		it "should be that: [h̥] \
			 \is the representation of the \
			 \voiceless glottal fricative pulmonic egressive consonant" $
			describeIPA "h̥"
				`shouldBe`
				"voiceless glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̊] \
			 \is the representation of the \
			 \voiceless glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̊"
				`shouldBe`
				"voiceless glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̥] \
			 \is the representation of the \
			 \voiceless glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̥"
				`shouldBe`
				"voiceless glottal fricative pulmonic egressive consonant"
	describe "voiceless labialized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [hʷ] \
			 \is the representation of the \
			 \voiceless labialized glottal fricative pulmonic egressive consonant" $
			describeIPA "hʷ"
				`shouldBe`
				"voiceless labialized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̊ʷ"
				`shouldBe`
				"voiceless labialized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̥ʷ"
				`shouldBe`
				"voiceless labialized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̊ʷ"
				`shouldBe`
				"voiceless labialized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̥ʷ"
				`shouldBe`
				"voiceless labialized glottal fricative pulmonic egressive consonant"
	describe "voiceless palatalized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [hʲ] \
			 \is the representation of the \
			 \voiceless palatalized glottal fricative pulmonic egressive consonant" $
			describeIPA "hʲ"
				`shouldBe`
				"voiceless palatalized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̊ʲ"
				`shouldBe`
				"voiceless palatalized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̥ʲ"
				`shouldBe`
				"voiceless palatalized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̊ʲ"
				`shouldBe`
				"voiceless palatalized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̥ʲ"
				`shouldBe`
				"voiceless palatalized glottal fricative pulmonic egressive consonant"
	describe "voiceless velarized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [hˠ] \
			 \is the representation of the \
			 \voiceless velarized glottal fricative pulmonic egressive consonant" $
			describeIPA "hˠ"
				`shouldBe`
				"voiceless velarized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̊ˠ"
				`shouldBe`
				"voiceless velarized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̥ˠ"
				`shouldBe`
				"voiceless velarized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̊ˠ"
				`shouldBe`
				"voiceless velarized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̥ˠ"
				`shouldBe`
				"voiceless velarized glottal fricative pulmonic egressive consonant"
	describe "voiceless pharyngealized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [hˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized glottal fricative pulmonic egressive consonant" $
			describeIPA "hˤ"
				`shouldBe`
				"voiceless pharyngealized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̊ˤ"
				`shouldBe`
				"voiceless pharyngealized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̥ˤ"
				`shouldBe`
				"voiceless pharyngealized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized glottal fricative pulmonic egressive consonant"
	describe "voiceless aspirated glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [hʰ] \
			 \is the representation of the \
			 \voiceless aspirated glottal fricative pulmonic egressive consonant" $
			describeIPA "hʰ"
				`shouldBe`
				"voiceless aspirated glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̥ʰ"
				`shouldBe`
				"voiceless aspirated glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̊ʰ"
				`shouldBe`
				"voiceless aspirated glottal fricative pulmonic egressive consonant"
	describe "voiceless aspirated labialized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [hʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized glottal fricative pulmonic egressive consonant" $
			describeIPA "hʰʷ"
				`shouldBe`
				"voiceless aspirated labialized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized glottal fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatalized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [hʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized glottal fricative pulmonic egressive consonant" $
			describeIPA "hʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized glottal fricative pulmonic egressive consonant"
	describe "voiceless aspirated velarized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [hʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized glottal fricative pulmonic egressive consonant" $
			describeIPA "hʰˠ"
				`shouldBe`
				"voiceless aspirated velarized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized glottal fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [hʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
			describeIPA "hʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized glottal fricative pulmonic egressive consonant"
	describe "voiced glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɦ] \
			 \is the representation of the \
			 \voiced glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ"
				`shouldBe`
				"voiced glottal fricative pulmonic egressive consonant"
		it "should be that: [h̬] \
			 \is the representation of the \
			 \voiced glottal fricative pulmonic egressive consonant" $
			describeIPA "h̬"
				`shouldBe`
				"voiced glottal fricative pulmonic egressive consonant"
		it "should be that: [h̬] \
			 \is the representation of the \
			 \voiced glottal fricative pulmonic egressive consonant" $
			describeIPA "h̬"
				`shouldBe`
				"voiced glottal fricative pulmonic egressive consonant"
	describe "voiced labialized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɦʷ] \
			 \is the representation of the \
			 \voiced labialized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦʷ"
				`shouldBe`
				"voiced labialized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̬ʷ] \
			 \is the representation of the \
			 \voiced labialized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̬ʷ"
				`shouldBe`
				"voiced labialized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̬ʷ] \
			 \is the representation of the \
			 \voiced labialized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̬ʷ"
				`shouldBe`
				"voiced labialized glottal fricative pulmonic egressive consonant"
	describe "voiced palatalized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɦʲ] \
			 \is the representation of the \
			 \voiced palatalized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦʲ"
				`shouldBe`
				"voiced palatalized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̬ʲ"
				`shouldBe`
				"voiced palatalized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̬ʲ"
				`shouldBe`
				"voiced palatalized glottal fricative pulmonic egressive consonant"
	describe "voiced velarized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɦˠ] \
			 \is the representation of the \
			 \voiced velarized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦˠ"
				`shouldBe`
				"voiced velarized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̬ˠ] \
			 \is the representation of the \
			 \voiced velarized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̬ˠ"
				`shouldBe`
				"voiced velarized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̬ˠ] \
			 \is the representation of the \
			 \voiced velarized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̬ˠ"
				`shouldBe`
				"voiced velarized glottal fricative pulmonic egressive consonant"
	describe "voiced pharyngealized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɦˤ] \
			 \is the representation of the \
			 \voiced pharyngealized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦˤ"
				`shouldBe`
				"voiced pharyngealized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̬ˤ"
				`shouldBe`
				"voiced pharyngealized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̬ˤ"
				`shouldBe`
				"voiced pharyngealized glottal fricative pulmonic egressive consonant"
	describe "voiced aspirated glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɦʰ] \
			 \is the representation of the \
			 \voiced aspirated glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦʰ"
				`shouldBe`
				"voiced aspirated glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̬ʰ"
				`shouldBe`
				"voiced aspirated glottal fricative pulmonic egressive consonant"
		it "should be that: [h̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated glottal fricative pulmonic egressive consonant" $
			describeIPA "h̬ʰ"
				`shouldBe`
				"voiced aspirated glottal fricative pulmonic egressive consonant"
	describe "voiced aspirated labialized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɦʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦʰʷ"
				`shouldBe`
				"voiced aspirated labialized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized glottal fricative pulmonic egressive consonant"
	describe "voiced aspirated palatalized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɦʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦʰʲ"
				`shouldBe`
				"voiced aspirated palatalized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized glottal fricative pulmonic egressive consonant"
	describe "voiced aspirated velarized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɦʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦʰˠ"
				`shouldBe`
				"voiced aspirated velarized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized glottal fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɦʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized glottal fricative pulmonic egressive consonant"
		it "should be that: [ɦ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
			describeIPA "ɦ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized glottal fricative pulmonic egressive consonant"
		it "should be that: [h̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized glottal fricative pulmonic egressive consonant" $
			describeIPA "h̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized glottal fricative pulmonic egressive consonant"
	describe "voiceless alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɬ] \
			 \is the representation of the \
			 \voiceless alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ"
				`shouldBe`
				"voiceless alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̊] \
			 \is the representation of the \
			 \voiceless alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̊"
				`shouldBe`
				"voiceless alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̥] \
			 \is the representation of the \
			 \voiceless alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̥"
				`shouldBe`
				"voiceless alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̊] \
			 \is the representation of the \
			 \voiceless alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̊"
				`shouldBe`
				"voiceless alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̥] \
			 \is the representation of the \
			 \voiceless alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̥"
				`shouldBe`
				"voiceless alveolar lateral fricative pulmonic egressive consonant"
	describe "voiceless labialized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɬʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬʷ"
				`shouldBe`
				"voiceless labialized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̊ʷ"
				`shouldBe`
				"voiceless labialized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̥ʷ"
				`shouldBe`
				"voiceless labialized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̊ʷ"
				`shouldBe`
				"voiceless labialized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̥ʷ"
				`shouldBe`
				"voiceless labialized alveolar lateral fricative pulmonic egressive consonant"
	describe "voiceless palatalized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɬʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬʲ"
				`shouldBe`
				"voiceless palatalized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̊ʲ"
				`shouldBe`
				"voiceless palatalized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̥ʲ"
				`shouldBe`
				"voiceless palatalized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̊ʲ"
				`shouldBe`
				"voiceless palatalized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̥ʲ"
				`shouldBe`
				"voiceless palatalized alveolar lateral fricative pulmonic egressive consonant"
	describe "voiceless velarized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɬˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬˠ"
				`shouldBe`
				"voiceless velarized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̊ˠ"
				`shouldBe`
				"voiceless velarized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̥ˠ"
				`shouldBe`
				"voiceless velarized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̊ˠ"
				`shouldBe`
				"voiceless velarized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̥ˠ"
				`shouldBe`
				"voiceless velarized alveolar lateral fricative pulmonic egressive consonant"
	describe "voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɬˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar lateral fricative pulmonic egressive consonant"
	describe "voiceless aspirated alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɬʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬʰ"
				`shouldBe`
				"voiceless aspirated alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̥ʰ"
				`shouldBe`
				"voiceless aspirated alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̊ʰ"
				`shouldBe`
				"voiceless aspirated alveolar lateral fricative pulmonic egressive consonant"
	describe "voiceless aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɬʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolar lateral fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɬʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolar lateral fricative pulmonic egressive consonant"
	describe "voiceless aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɬʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolar lateral fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɬʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant"
	describe "voiced alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɮ] \
			 \is the representation of the \
			 \voiced alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ"
				`shouldBe`
				"voiced alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̬] \
			 \is the representation of the \
			 \voiced alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̬"
				`shouldBe`
				"voiced alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̬] \
			 \is the representation of the \
			 \voiced alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̬"
				`shouldBe`
				"voiced alveolar lateral fricative pulmonic egressive consonant"
	describe "voiced labialized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɮʷ] \
			 \is the representation of the \
			 \voiced labialized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮʷ"
				`shouldBe`
				"voiced labialized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̬ʷ"
				`shouldBe`
				"voiced labialized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̬ʷ"
				`shouldBe`
				"voiced labialized alveolar lateral fricative pulmonic egressive consonant"
	describe "voiced palatalized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɮʲ] \
			 \is the representation of the \
			 \voiced palatalized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮʲ"
				`shouldBe`
				"voiced palatalized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̬ʲ"
				`shouldBe`
				"voiced palatalized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̬ʲ"
				`shouldBe`
				"voiced palatalized alveolar lateral fricative pulmonic egressive consonant"
	describe "voiced velarized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɮˠ] \
			 \is the representation of the \
			 \voiced velarized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮˠ"
				`shouldBe`
				"voiced velarized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̬ˠ"
				`shouldBe`
				"voiced velarized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̬ˠ"
				`shouldBe`
				"voiced velarized alveolar lateral fricative pulmonic egressive consonant"
	describe "voiced pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɮˤ] \
			 \is the representation of the \
			 \voiced pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮˤ"
				`shouldBe`
				"voiced pharyngealized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̬ˤ"
				`shouldBe`
				"voiced pharyngealized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̬ˤ"
				`shouldBe`
				"voiced pharyngealized alveolar lateral fricative pulmonic egressive consonant"
	describe "voiced aspirated alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɮʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮʰ"
				`shouldBe`
				"voiced aspirated alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̬ʰ"
				`shouldBe`
				"voiced aspirated alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̬ʰ"
				`shouldBe`
				"voiced aspirated alveolar lateral fricative pulmonic egressive consonant"
	describe "voiced aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɮʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolar lateral fricative pulmonic egressive consonant"
	describe "voiced aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɮʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolar lateral fricative pulmonic egressive consonant"
	describe "voiced aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɮʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolar lateral fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɮʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɮ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɮ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant"
		it "should be that: [ɬ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant" $
			describeIPA "ɬ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolar lateral fricative pulmonic egressive consonant"



	describe "voiceless labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍ] \
			 \is the representation of the \
			 \voiceless labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ"
				`shouldBe`
				"voiceless labial-velar fricative pulmonic egressive consonant"
		it "should be that: [ʍ̊] \
			 \is the representation of the \
			 \voiceless labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̊"
				`shouldBe`
				"voiceless labial-velar fricative pulmonic egressive consonant"
		it "should be that: [ʍ̥] \
			 \is the representation of the \
			 \voiceless labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̥"
				`shouldBe`
				"voiceless labial-velar fricative pulmonic egressive consonant"
	describe "voiceless labialized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍʷ] \
			 \is the representation of the \
			 \voiceless labialized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍʷ"
				`shouldBe`
				"voiceless labialized labial-velar fricative pulmonic egressive consonant"
		it "should be that: [ʍ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̊ʷ"
				`shouldBe`
				"voiceless labialized labial-velar fricative pulmonic egressive consonant"
		it "should be that: [ʍ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̥ʷ"
				`shouldBe`
				"voiceless labialized labial-velar fricative pulmonic egressive consonant"
	describe "voiceless palatalized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍʲ] \
			 \is the representation of the \
			 \voiceless palatalized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍʲ"
				`shouldBe`
				"voiceless palatalized labial-velar fricative pulmonic egressive consonant"
		it "should be that: [ʍ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̊ʲ"
				`shouldBe`
				"voiceless palatalized labial-velar fricative pulmonic egressive consonant"
		it "should be that: [ʍ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̥ʲ"
				`shouldBe`
				"voiceless palatalized labial-velar fricative pulmonic egressive consonant"
	describe "voiceless velarized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍˠ] \
			 \is the representation of the \
			 \voiceless velarized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍˠ"
				`shouldBe`
				"voiceless velarized labial-velar fricative pulmonic egressive consonant"
		it "should be that: [ʍ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̊ˠ"
				`shouldBe`
				"voiceless velarized labial-velar fricative pulmonic egressive consonant"
		it "should be that: [ʍ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̥ˠ"
				`shouldBe`
				"voiceless velarized labial-velar fricative pulmonic egressive consonant"
	describe "voiceless pharyngealized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍˤ"
				`shouldBe`
				"voiceless pharyngealized labial-velar fricative pulmonic egressive consonant"
		it "should be that: [ʍ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized labial-velar fricative pulmonic egressive consonant"
		it "should be that: [ʍ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized labial-velar fricative pulmonic egressive consonant"
	describe "voiceless aspirated labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍʰ] \
			 \is the representation of the \
			 \voiceless aspirated labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍʰ"
				`shouldBe`
				"voiceless aspirated labial-velar fricative pulmonic egressive consonant"
	describe "voiceless aspirated labialized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍʰʷ"
				`shouldBe`
				"voiceless aspirated labialized labial-velar fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatalized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized labial-velar fricative pulmonic egressive consonant"
	describe "voiceless aspirated velarized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍʰˠ"
				`shouldBe`
				"voiceless aspirated velarized labial-velar fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized labial-velar fricative pulmonic egressive consonant"
	describe "voiced labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍ̬] \
			 \is the representation of the \
			 \voiced labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̬"
				`shouldBe`
				"voiced labial-velar fricative pulmonic egressive consonant"
		it "should be that: [ʍ̬] \
			 \is the representation of the \
			 \voiced labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̬"
				`shouldBe`
				"voiced labial-velar fricative pulmonic egressive consonant"
	describe "voiced labialized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̬ʷ"
				`shouldBe`
				"voiced labialized labial-velar fricative pulmonic egressive consonant"
		it "should be that: [ʍ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̬ʷ"
				`shouldBe`
				"voiced labialized labial-velar fricative pulmonic egressive consonant"
	describe "voiced palatalized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̬ʲ"
				`shouldBe`
				"voiced palatalized labial-velar fricative pulmonic egressive consonant"
		it "should be that: [ʍ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̬ʲ"
				`shouldBe`
				"voiced palatalized labial-velar fricative pulmonic egressive consonant"
	describe "voiced velarized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̬ˠ"
				`shouldBe`
				"voiced velarized labial-velar fricative pulmonic egressive consonant"
		it "should be that: [ʍ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̬ˠ"
				`shouldBe`
				"voiced velarized labial-velar fricative pulmonic egressive consonant"
	describe "voiced pharyngealized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̬ˤ"
				`shouldBe`
				"voiced pharyngealized labial-velar fricative pulmonic egressive consonant"
		it "should be that: [ʍ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̬ˤ"
				`shouldBe`
				"voiced pharyngealized labial-velar fricative pulmonic egressive consonant"
	describe "voiced aspirated labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̬ʰ"
				`shouldBe`
				"voiced aspirated labial-velar fricative pulmonic egressive consonant"
	describe "voiced aspirated labialized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized labial-velar fricative pulmonic egressive consonant"
	describe "voiced aspirated palatalized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized labial-velar fricative pulmonic egressive consonant"
	describe "voiced aspirated velarized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized labial-velar fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized labial-velar fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʍ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized labial-velar fricative pulmonic egressive consonant" $
			describeIPA "ʍ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized labial-velar fricative pulmonic egressive consonant"
	describe "voiceless epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡ] \
			 \is the representation of the \
			 \voiceless epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ"
				`shouldBe`
				"voiceless epiglottal plosive pulmonic egressive consonant"
		it "should be that: [ʡ̊] \
			 \is the representation of the \
			 \voiceless epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̊"
				`shouldBe`
				"voiceless epiglottal plosive pulmonic egressive consonant"
		it "should be that: [ʡ̥] \
			 \is the representation of the \
			 \voiceless epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̥"
				`shouldBe`
				"voiceless epiglottal plosive pulmonic egressive consonant"
	describe "voiceless labialized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡʷ] \
			 \is the representation of the \
			 \voiceless labialized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡʷ"
				`shouldBe`
				"voiceless labialized epiglottal plosive pulmonic egressive consonant"
		it "should be that: [ʡ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̊ʷ"
				`shouldBe`
				"voiceless labialized epiglottal plosive pulmonic egressive consonant"
		it "should be that: [ʡ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̥ʷ"
				`shouldBe`
				"voiceless labialized epiglottal plosive pulmonic egressive consonant"
	describe "voiceless palatalized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡʲ] \
			 \is the representation of the \
			 \voiceless palatalized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡʲ"
				`shouldBe`
				"voiceless palatalized epiglottal plosive pulmonic egressive consonant"
		it "should be that: [ʡ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̊ʲ"
				`shouldBe`
				"voiceless palatalized epiglottal plosive pulmonic egressive consonant"
		it "should be that: [ʡ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̥ʲ"
				`shouldBe`
				"voiceless palatalized epiglottal plosive pulmonic egressive consonant"
	describe "voiceless velarized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡˠ] \
			 \is the representation of the \
			 \voiceless velarized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡˠ"
				`shouldBe`
				"voiceless velarized epiglottal plosive pulmonic egressive consonant"
		it "should be that: [ʡ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̊ˠ"
				`shouldBe`
				"voiceless velarized epiglottal plosive pulmonic egressive consonant"
		it "should be that: [ʡ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̥ˠ"
				`shouldBe`
				"voiceless velarized epiglottal plosive pulmonic egressive consonant"
	describe "voiceless pharyngealized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡˤ"
				`shouldBe`
				"voiceless pharyngealized epiglottal plosive pulmonic egressive consonant"
		it "should be that: [ʡ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized epiglottal plosive pulmonic egressive consonant"
		it "should be that: [ʡ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized epiglottal plosive pulmonic egressive consonant"
	describe "voiceless aspirated epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡʰ] \
			 \is the representation of the \
			 \voiceless aspirated epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡʰ"
				`shouldBe`
				"voiceless aspirated epiglottal plosive pulmonic egressive consonant"
	describe "voiceless aspirated labialized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡʰʷ"
				`shouldBe`
				"voiceless aspirated labialized epiglottal plosive pulmonic egressive consonant"
	describe "voiceless aspirated palatalized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized epiglottal plosive pulmonic egressive consonant"
	describe "voiceless aspirated velarized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡʰˠ"
				`shouldBe`
				"voiceless aspirated velarized epiglottal plosive pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized epiglottal plosive pulmonic egressive consonant"
	describe "voiced epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡ̬] \
			 \is the representation of the \
			 \voiced epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̬"
				`shouldBe`
				"voiced epiglottal plosive pulmonic egressive consonant"
		it "should be that: [ʡ̬] \
			 \is the representation of the \
			 \voiced epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̬"
				`shouldBe`
				"voiced epiglottal plosive pulmonic egressive consonant"
	describe "voiced labialized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̬ʷ"
				`shouldBe`
				"voiced labialized epiglottal plosive pulmonic egressive consonant"
		it "should be that: [ʡ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̬ʷ"
				`shouldBe`
				"voiced labialized epiglottal plosive pulmonic egressive consonant"
	describe "voiced palatalized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̬ʲ"
				`shouldBe`
				"voiced palatalized epiglottal plosive pulmonic egressive consonant"
		it "should be that: [ʡ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̬ʲ"
				`shouldBe`
				"voiced palatalized epiglottal plosive pulmonic egressive consonant"
	describe "voiced velarized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̬ˠ"
				`shouldBe`
				"voiced velarized epiglottal plosive pulmonic egressive consonant"
		it "should be that: [ʡ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̬ˠ"
				`shouldBe`
				"voiced velarized epiglottal plosive pulmonic egressive consonant"
	describe "voiced pharyngealized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̬ˤ"
				`shouldBe`
				"voiced pharyngealized epiglottal plosive pulmonic egressive consonant"
		it "should be that: [ʡ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̬ˤ"
				`shouldBe`
				"voiced pharyngealized epiglottal plosive pulmonic egressive consonant"
	describe "voiced aspirated epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̬ʰ"
				`shouldBe`
				"voiced aspirated epiglottal plosive pulmonic egressive consonant"
	describe "voiced aspirated labialized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized epiglottal plosive pulmonic egressive consonant"
	describe "voiced aspirated palatalized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized epiglottal plosive pulmonic egressive consonant"
	describe "voiced aspirated velarized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized epiglottal plosive pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized epiglottal plosive pulmonic egressive consonant" $
		do
		it "should be that: [ʡ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized epiglottal plosive pulmonic egressive consonant" $
			describeIPA "ʡ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized epiglottal plosive pulmonic egressive consonant"
	describe "voiceless epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʜ] \
			 \is the representation of the \
			 \voiceless epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ"
				`shouldBe`
				"voiceless epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̊] \
			 \is the representation of the \
			 \voiceless epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̊"
				`shouldBe`
				"voiceless epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̥] \
			 \is the representation of the \
			 \voiceless epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̥"
				`shouldBe`
				"voiceless epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̊] \
			 \is the representation of the \
			 \voiceless epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̊"
				`shouldBe`
				"voiceless epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̥] \
			 \is the representation of the \
			 \voiceless epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̥"
				`shouldBe`
				"voiceless epiglottal fricative pulmonic egressive consonant"
	describe "voiceless labialized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʜʷ] \
			 \is the representation of the \
			 \voiceless labialized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜʷ"
				`shouldBe`
				"voiceless labialized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̊ʷ"
				`shouldBe`
				"voiceless labialized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̥ʷ"
				`shouldBe`
				"voiceless labialized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̊ʷ"
				`shouldBe`
				"voiceless labialized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̥ʷ"
				`shouldBe`
				"voiceless labialized epiglottal fricative pulmonic egressive consonant"
	describe "voiceless palatalized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʜʲ] \
			 \is the representation of the \
			 \voiceless palatalized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜʲ"
				`shouldBe`
				"voiceless palatalized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̊ʲ"
				`shouldBe`
				"voiceless palatalized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̥ʲ"
				`shouldBe`
				"voiceless palatalized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̊ʲ"
				`shouldBe`
				"voiceless palatalized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̥ʲ"
				`shouldBe`
				"voiceless palatalized epiglottal fricative pulmonic egressive consonant"
	describe "voiceless velarized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʜˠ] \
			 \is the representation of the \
			 \voiceless velarized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜˠ"
				`shouldBe`
				"voiceless velarized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̊ˠ"
				`shouldBe`
				"voiceless velarized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̥ˠ"
				`shouldBe`
				"voiceless velarized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̊ˠ"
				`shouldBe`
				"voiceless velarized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̥ˠ"
				`shouldBe`
				"voiceless velarized epiglottal fricative pulmonic egressive consonant"
	describe "voiceless pharyngealized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʜˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜˤ"
				`shouldBe`
				"voiceless pharyngealized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized epiglottal fricative pulmonic egressive consonant"
	describe "voiceless aspirated epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʜʰ] \
			 \is the representation of the \
			 \voiceless aspirated epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜʰ"
				`shouldBe`
				"voiceless aspirated epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̥ʰ"
				`shouldBe`
				"voiceless aspirated epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̊ʰ"
				`shouldBe`
				"voiceless aspirated epiglottal fricative pulmonic egressive consonant"
	describe "voiceless aspirated labialized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʜʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜʰʷ"
				`shouldBe`
				"voiceless aspirated labialized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized epiglottal fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʜʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized epiglottal fricative pulmonic egressive consonant"
	describe "voiceless aspirated velarized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʜʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜʰˠ"
				`shouldBe`
				"voiceless aspirated velarized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized epiglottal fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʜʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized epiglottal fricative pulmonic egressive consonant"
	describe "voiced epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʢ] \
			 \is the representation of the \
			 \voiced epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ"
				`shouldBe`
				"voiced epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̬] \
			 \is the representation of the \
			 \voiced epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̬"
				`shouldBe`
				"voiced epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̬] \
			 \is the representation of the \
			 \voiced epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̬"
				`shouldBe`
				"voiced epiglottal fricative pulmonic egressive consonant"
	describe "voiced labialized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʢʷ] \
			 \is the representation of the \
			 \voiced labialized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢʷ"
				`shouldBe`
				"voiced labialized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̬ʷ"
				`shouldBe`
				"voiced labialized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̬ʷ"
				`shouldBe`
				"voiced labialized epiglottal fricative pulmonic egressive consonant"
	describe "voiced palatalized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʢʲ] \
			 \is the representation of the \
			 \voiced palatalized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢʲ"
				`shouldBe`
				"voiced palatalized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̬ʲ"
				`shouldBe`
				"voiced palatalized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̬ʲ"
				`shouldBe`
				"voiced palatalized epiglottal fricative pulmonic egressive consonant"
	describe "voiced velarized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʢˠ] \
			 \is the representation of the \
			 \voiced velarized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢˠ"
				`shouldBe`
				"voiced velarized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̬ˠ"
				`shouldBe`
				"voiced velarized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̬ˠ"
				`shouldBe`
				"voiced velarized epiglottal fricative pulmonic egressive consonant"
	describe "voiced pharyngealized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʢˤ] \
			 \is the representation of the \
			 \voiced pharyngealized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢˤ"
				`shouldBe`
				"voiced pharyngealized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̬ˤ"
				`shouldBe`
				"voiced pharyngealized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̬ˤ"
				`shouldBe`
				"voiced pharyngealized epiglottal fricative pulmonic egressive consonant"
	describe "voiced aspirated epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʢʰ] \
			 \is the representation of the \
			 \voiced aspirated epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢʰ"
				`shouldBe`
				"voiced aspirated epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̬ʰ"
				`shouldBe`
				"voiced aspirated epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̬ʰ"
				`shouldBe`
				"voiced aspirated epiglottal fricative pulmonic egressive consonant"
	describe "voiced aspirated labialized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʢʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢʰʷ"
				`shouldBe`
				"voiced aspirated labialized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized epiglottal fricative pulmonic egressive consonant"
	describe "voiced aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʢʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢʰʲ"
				`shouldBe`
				"voiced aspirated palatalized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized epiglottal fricative pulmonic egressive consonant"
	describe "voiced aspirated velarized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʢʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢʰˠ"
				`shouldBe`
				"voiced aspirated velarized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized epiglottal fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʢʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʢ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʢ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized epiglottal fricative pulmonic egressive consonant"
		it "should be that: [ʜ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized epiglottal fricative pulmonic egressive consonant" $
			describeIPA "ʜ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized epiglottal fricative pulmonic egressive consonant"
	describe "voiceless alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɕ] \
			 \is the representation of the \
			 \voiceless alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ"
				`shouldBe`
				"voiceless alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̊] \
			 \is the representation of the \
			 \voiceless alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̊"
				`shouldBe`
				"voiceless alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̥] \
			 \is the representation of the \
			 \voiceless alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̥"
				`shouldBe`
				"voiceless alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̊] \
			 \is the representation of the \
			 \voiceless alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̊"
				`shouldBe`
				"voiceless alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̥] \
			 \is the representation of the \
			 \voiceless alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̥"
				`shouldBe`
				"voiceless alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiceless labialized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɕʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕʷ"
				`shouldBe`
				"voiceless labialized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̊ʷ"
				`shouldBe`
				"voiceless labialized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̥ʷ"
				`shouldBe`
				"voiceless labialized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̊ʷ"
				`shouldBe`
				"voiceless labialized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̥ʷ"
				`shouldBe`
				"voiceless labialized alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɕʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕʲ"
				`shouldBe`
				"voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̊ʲ"
				`shouldBe`
				"voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̥ʲ"
				`shouldBe`
				"voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̊ʲ"
				`shouldBe`
				"voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̥ʲ"
				`shouldBe`
				"voiceless palatalized alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiceless velarized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɕˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕˠ"
				`shouldBe`
				"voiceless velarized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̊ˠ"
				`shouldBe`
				"voiceless velarized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̥ˠ"
				`shouldBe`
				"voiceless velarized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̊ˠ"
				`shouldBe`
				"voiceless velarized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̥ˠ"
				`shouldBe`
				"voiceless velarized alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɕˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕˤ"
				`shouldBe`
				"voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiceless aspirated alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɕʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕʰ"
				`shouldBe`
				"voiceless aspirated alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̥ʰ"
				`shouldBe`
				"voiceless aspirated alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̊ʰ"
				`shouldBe`
				"voiceless aspirated alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiceless aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɕʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiceless aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɕʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiceless aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɕʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ɕʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiced alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʑ] \
			 \is the representation of the \
			 \voiced alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ"
				`shouldBe`
				"voiced alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̬] \
			 \is the representation of the \
			 \voiced alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̬"
				`shouldBe`
				"voiced alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̬] \
			 \is the representation of the \
			 \voiced alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̬"
				`shouldBe`
				"voiced alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiced labialized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʑʷ] \
			 \is the representation of the \
			 \voiced labialized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑʷ"
				`shouldBe`
				"voiced labialized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̬ʷ"
				`shouldBe`
				"voiced labialized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̬ʷ] \
			 \is the representation of the \
			 \voiced labialized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̬ʷ"
				`shouldBe`
				"voiced labialized alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiced palatalized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʑʲ] \
			 \is the representation of the \
			 \voiced palatalized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑʲ"
				`shouldBe`
				"voiced palatalized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̬ʲ"
				`shouldBe`
				"voiced palatalized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̬ʲ] \
			 \is the representation of the \
			 \voiced palatalized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̬ʲ"
				`shouldBe`
				"voiced palatalized alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiced velarized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʑˠ] \
			 \is the representation of the \
			 \voiced velarized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑˠ"
				`shouldBe`
				"voiced velarized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̬ˠ"
				`shouldBe`
				"voiced velarized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̬ˠ] \
			 \is the representation of the \
			 \voiced velarized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̬ˠ"
				`shouldBe`
				"voiced velarized alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiced pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʑˤ] \
			 \is the representation of the \
			 \voiced pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑˤ"
				`shouldBe`
				"voiced pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̬ˤ"
				`shouldBe`
				"voiced pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̬ˤ] \
			 \is the representation of the \
			 \voiced pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̬ˤ"
				`shouldBe`
				"voiced pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiced aspirated alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʑʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑʰ"
				`shouldBe`
				"voiced aspirated alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̬ʰ"
				`shouldBe`
				"voiced aspirated alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̬ʰ"
				`shouldBe`
				"voiced aspirated alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiced aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʑʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiced aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʑʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiced aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʑʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolo-palatal fricative pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
		do
		it "should be that: [ʑʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ʑ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ʑ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
		it "should be that: [ɕ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant" $
			describeIPA "ɕ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolo-palatal fricative pulmonic egressive consonant"
