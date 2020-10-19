module DescribeIPASpec.TapOrFlap where

import Test.Hspec (Spec, describe, it, shouldBe)
import qualified IPA 
import Data.Text (pack, unpack)

describeIPA :: String -> String
describeIPA = unpack . IPA.describeIPA . pack

tapOrFlapConsonantSpec :: Spec
tapOrFlapConsonantSpec = do
	describe "voiceless labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱ̊] \
			 \is the representation of the \
			 \voiceless labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̊"
				`shouldBe`
				"voiceless labio-dental tap or flap pulmonic egressive consonant"
		it "should be that: [ⱱ̥] \
			 \is the representation of the \
			 \voiceless labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̥"
				`shouldBe`
				"voiceless labio-dental tap or flap pulmonic egressive consonant"
	describe "voiceless labialized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̊ʷ"
				`shouldBe`
				"voiceless labialized labio-dental tap or flap pulmonic egressive consonant"
		it "should be that: [ⱱ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̥ʷ"
				`shouldBe`
				"voiceless labialized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiceless palatalized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̊ʲ"
				`shouldBe`
				"voiceless palatalized labio-dental tap or flap pulmonic egressive consonant"
		it "should be that: [ⱱ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̥ʲ"
				`shouldBe`
				"voiceless palatalized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiceless velarized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̊ˠ"
				`shouldBe`
				"voiceless velarized labio-dental tap or flap pulmonic egressive consonant"
		it "should be that: [ⱱ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̥ˠ"
				`shouldBe`
				"voiceless velarized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiceless pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized labio-dental tap or flap pulmonic egressive consonant"
		it "should be that: [ⱱ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiceless aspirated labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̥ʰ"
				`shouldBe`
				"voiceless aspirated labio-dental tap or flap pulmonic egressive consonant"
		it "should be that: [ⱱ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̊ʰ"
				`shouldBe`
				"voiceless aspirated labio-dental tap or flap pulmonic egressive consonant"
	describe "voiceless aspirated labialized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized labio-dental tap or flap pulmonic egressive consonant"
		it "should be that: [ⱱ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiceless aspirated palatalized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized labio-dental tap or flap pulmonic egressive consonant"
		it "should be that: [ⱱ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiceless aspirated velarized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized labio-dental tap or flap pulmonic egressive consonant"
		it "should be that: [ⱱ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant"
		it "should be that: [ⱱ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiced labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱ] \
			 \is the representation of the \
			 \voiced labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ"
				`shouldBe`
				"voiced labio-dental tap or flap pulmonic egressive consonant"
	describe "voiced labialized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱʷ] \
			 \is the representation of the \
			 \voiced labialized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱʷ"
				`shouldBe`
				"voiced labialized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiced palatalized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱʲ] \
			 \is the representation of the \
			 \voiced palatalized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱʲ"
				`shouldBe`
				"voiced palatalized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiced velarized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱˠ] \
			 \is the representation of the \
			 \voiced velarized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱˠ"
				`shouldBe`
				"voiced velarized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiced pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱˤ] \
			 \is the representation of the \
			 \voiced pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱˤ"
				`shouldBe`
				"voiced pharyngealized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiced aspirated labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱʰ] \
			 \is the representation of the \
			 \voiced aspirated labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱʰ"
				`shouldBe`
				"voiced aspirated labio-dental tap or flap pulmonic egressive consonant"
		it "should be that: [ⱱ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̬ʰ"
				`shouldBe`
				"voiced aspirated labio-dental tap or flap pulmonic egressive consonant"
	describe "voiced aspirated labialized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱʰʷ"
				`shouldBe`
				"voiced aspirated labialized labio-dental tap or flap pulmonic egressive consonant"
		it "should be that: [ⱱ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiced aspirated palatalized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱʰʲ"
				`shouldBe`
				"voiced aspirated palatalized labio-dental tap or flap pulmonic egressive consonant"
		it "should be that: [ⱱ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiced aspirated velarized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱʰˠ"
				`shouldBe`
				"voiced aspirated velarized labio-dental tap or flap pulmonic egressive consonant"
		it "should be that: [ⱱ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ⱱʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant"
		it "should be that: [ⱱ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant" $
			describeIPA "ⱱ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized labio-dental tap or flap pulmonic egressive consonant"
	describe "voiceless alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾ̊] \
			 \is the representation of the \
			 \voiceless alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̊"
				`shouldBe`
				"voiceless alveolar tap or flap pulmonic egressive consonant"
		it "should be that: [ɾ̥] \
			 \is the representation of the \
			 \voiceless alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̥"
				`shouldBe`
				"voiceless alveolar tap or flap pulmonic egressive consonant"
	describe "voiceless labialized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̊ʷ"
				`shouldBe`
				"voiceless labialized alveolar tap or flap pulmonic egressive consonant"
		it "should be that: [ɾ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̥ʷ"
				`shouldBe`
				"voiceless labialized alveolar tap or flap pulmonic egressive consonant"
	describe "voiceless palatalized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̊ʲ"
				`shouldBe`
				"voiceless palatalized alveolar tap or flap pulmonic egressive consonant"
		it "should be that: [ɾ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̥ʲ"
				`shouldBe`
				"voiceless palatalized alveolar tap or flap pulmonic egressive consonant"
	describe "voiceless velarized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̊ˠ"
				`shouldBe`
				"voiceless velarized alveolar tap or flap pulmonic egressive consonant"
		it "should be that: [ɾ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̥ˠ"
				`shouldBe`
				"voiceless velarized alveolar tap or flap pulmonic egressive consonant"
	describe "voiceless pharyngealized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar tap or flap pulmonic egressive consonant"
		it "should be that: [ɾ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar tap or flap pulmonic egressive consonant"
	describe "voiceless aspirated alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̥ʰ"
				`shouldBe`
				"voiceless aspirated alveolar tap or flap pulmonic egressive consonant"
		it "should be that: [ɾ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̊ʰ"
				`shouldBe`
				"voiceless aspirated alveolar tap or flap pulmonic egressive consonant"
	describe "voiceless aspirated labialized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolar tap or flap pulmonic egressive consonant"
		it "should be that: [ɾ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolar tap or flap pulmonic egressive consonant"
	describe "voiceless aspirated palatalized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolar tap or flap pulmonic egressive consonant"
		it "should be that: [ɾ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolar tap or flap pulmonic egressive consonant"
	describe "voiceless aspirated velarized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolar tap or flap pulmonic egressive consonant"
		it "should be that: [ɾ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolar tap or flap pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant"
		it "should be that: [ɾ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant"
	describe "voiced alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾ] \
			 \is the representation of the \
			 \voiced alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ"
				`shouldBe`
				"voiced alveolar tap or flap pulmonic egressive consonant"
	describe "voiced labialized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾʷ] \
			 \is the representation of the \
			 \voiced labialized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾʷ"
				`shouldBe`
				"voiced labialized alveolar tap or flap pulmonic egressive consonant"
	describe "voiced palatalized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾʲ] \
			 \is the representation of the \
			 \voiced palatalized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾʲ"
				`shouldBe`
				"voiced palatalized alveolar tap or flap pulmonic egressive consonant"
	describe "voiced velarized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾˠ] \
			 \is the representation of the \
			 \voiced velarized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾˠ"
				`shouldBe`
				"voiced velarized alveolar tap or flap pulmonic egressive consonant"
	describe "voiced pharyngealized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾˤ] \
			 \is the representation of the \
			 \voiced pharyngealized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾˤ"
				`shouldBe`
				"voiced pharyngealized alveolar tap or flap pulmonic egressive consonant"
	describe "voiced aspirated alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾʰ"
				`shouldBe`
				"voiced aspirated alveolar tap or flap pulmonic egressive consonant"
		it "should be that: [ɾ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̬ʰ"
				`shouldBe`
				"voiced aspirated alveolar tap or flap pulmonic egressive consonant"
	describe "voiced aspirated labialized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolar tap or flap pulmonic egressive consonant"
		it "should be that: [ɾ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolar tap or flap pulmonic egressive consonant"
	describe "voiced aspirated palatalized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolar tap or flap pulmonic egressive consonant"
		it "should be that: [ɾ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolar tap or flap pulmonic egressive consonant"
	describe "voiced aspirated velarized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolar tap or flap pulmonic egressive consonant"
		it "should be that: [ɾ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolar tap or flap pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɾʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant"
		it "should be that: [ɾ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant" $
			describeIPA "ɾ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolar tap or flap pulmonic egressive consonant"
	describe "voiceless retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽ̊] \
			 \is the representation of the \
			 \voiceless retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̊"
				`shouldBe`
				"voiceless retroflex tap or flap pulmonic egressive consonant"
		it "should be that: [ɽ̥] \
			 \is the representation of the \
			 \voiceless retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̥"
				`shouldBe`
				"voiceless retroflex tap or flap pulmonic egressive consonant"
	describe "voiceless labialized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̊ʷ"
				`shouldBe`
				"voiceless labialized retroflex tap or flap pulmonic egressive consonant"
		it "should be that: [ɽ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̥ʷ"
				`shouldBe`
				"voiceless labialized retroflex tap or flap pulmonic egressive consonant"
	describe "voiceless palatalized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̊ʲ"
				`shouldBe`
				"voiceless palatalized retroflex tap or flap pulmonic egressive consonant"
		it "should be that: [ɽ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̥ʲ"
				`shouldBe`
				"voiceless palatalized retroflex tap or flap pulmonic egressive consonant"
	describe "voiceless velarized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̊ˠ"
				`shouldBe`
				"voiceless velarized retroflex tap or flap pulmonic egressive consonant"
		it "should be that: [ɽ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̥ˠ"
				`shouldBe`
				"voiceless velarized retroflex tap or flap pulmonic egressive consonant"
	describe "voiceless pharyngealized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized retroflex tap or flap pulmonic egressive consonant"
		it "should be that: [ɽ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized retroflex tap or flap pulmonic egressive consonant"
	describe "voiceless aspirated retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̥ʰ"
				`shouldBe`
				"voiceless aspirated retroflex tap or flap pulmonic egressive consonant"
		it "should be that: [ɽ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̊ʰ"
				`shouldBe`
				"voiceless aspirated retroflex tap or flap pulmonic egressive consonant"
	describe "voiceless aspirated labialized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized retroflex tap or flap pulmonic egressive consonant"
		it "should be that: [ɽ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized retroflex tap or flap pulmonic egressive consonant"
	describe "voiceless aspirated palatalized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized retroflex tap or flap pulmonic egressive consonant"
		it "should be that: [ɽ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized retroflex tap or flap pulmonic egressive consonant"
	describe "voiceless aspirated velarized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized retroflex tap or flap pulmonic egressive consonant"
		it "should be that: [ɽ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized retroflex tap or flap pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant"
		it "should be that: [ɽ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant"
	describe "voiced retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽ] \
			 \is the representation of the \
			 \voiced retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ"
				`shouldBe`
				"voiced retroflex tap or flap pulmonic egressive consonant"
	describe "voiced labialized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽʷ] \
			 \is the representation of the \
			 \voiced labialized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽʷ"
				`shouldBe`
				"voiced labialized retroflex tap or flap pulmonic egressive consonant"
	describe "voiced palatalized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽʲ] \
			 \is the representation of the \
			 \voiced palatalized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽʲ"
				`shouldBe`
				"voiced palatalized retroflex tap or flap pulmonic egressive consonant"
	describe "voiced velarized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽˠ] \
			 \is the representation of the \
			 \voiced velarized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽˠ"
				`shouldBe`
				"voiced velarized retroflex tap or flap pulmonic egressive consonant"
	describe "voiced pharyngealized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽˤ] \
			 \is the representation of the \
			 \voiced pharyngealized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽˤ"
				`shouldBe`
				"voiced pharyngealized retroflex tap or flap pulmonic egressive consonant"
	describe "voiced aspirated retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽʰ] \
			 \is the representation of the \
			 \voiced aspirated retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽʰ"
				`shouldBe`
				"voiced aspirated retroflex tap or flap pulmonic egressive consonant"
		it "should be that: [ɽ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̬ʰ"
				`shouldBe`
				"voiced aspirated retroflex tap or flap pulmonic egressive consonant"
	describe "voiced aspirated labialized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽʰʷ"
				`shouldBe`
				"voiced aspirated labialized retroflex tap or flap pulmonic egressive consonant"
		it "should be that: [ɽ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized retroflex tap or flap pulmonic egressive consonant"
	describe "voiced aspirated palatalized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽʰʲ"
				`shouldBe`
				"voiced aspirated palatalized retroflex tap or flap pulmonic egressive consonant"
		it "should be that: [ɽ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized retroflex tap or flap pulmonic egressive consonant"
	describe "voiced aspirated velarized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽʰˠ"
				`shouldBe`
				"voiced aspirated velarized retroflex tap or flap pulmonic egressive consonant"
		it "should be that: [ɽ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized retroflex tap or flap pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant" $
		do
		it "should be that: [ɽʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant"
		it "should be that: [ɽ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant" $
			describeIPA "ɽ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized retroflex tap or flap pulmonic egressive consonant"
