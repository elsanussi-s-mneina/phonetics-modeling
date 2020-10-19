module DescribeIPASpec.Approximant where

import Test.Hspec (Spec, describe, it, shouldBe)
import qualified IPA 
import Data.Text (pack, unpack)

describeIPA :: String -> String
describeIPA = unpack . IPA.describeIPA . pack

approximantConsonantSpec :: Spec
approximantConsonantSpec = do
	describe "voiceless labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋ̊] \
			 \is the representation of the \
			 \voiceless labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̊"
				`shouldBe`
				"voiceless labio-dental approximant pulmonic egressive consonant"
		it "should be that: [ʋ̥] \
			 \is the representation of the \
			 \voiceless labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̥"
				`shouldBe`
				"voiceless labio-dental approximant pulmonic egressive consonant"
	describe "voiceless labialized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̊ʷ"
				`shouldBe`
				"voiceless labialized labio-dental approximant pulmonic egressive consonant"
		it "should be that: [ʋ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̥ʷ"
				`shouldBe`
				"voiceless labialized labio-dental approximant pulmonic egressive consonant"
	describe "voiceless palatalized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̊ʲ"
				`shouldBe`
				"voiceless palatalized labio-dental approximant pulmonic egressive consonant"
		it "should be that: [ʋ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̥ʲ"
				`shouldBe`
				"voiceless palatalized labio-dental approximant pulmonic egressive consonant"
	describe "voiceless velarized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̊ˠ"
				`shouldBe`
				"voiceless velarized labio-dental approximant pulmonic egressive consonant"
		it "should be that: [ʋ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̥ˠ"
				`shouldBe`
				"voiceless velarized labio-dental approximant pulmonic egressive consonant"
	describe "voiceless pharyngealized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized labio-dental approximant pulmonic egressive consonant"
		it "should be that: [ʋ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized labio-dental approximant pulmonic egressive consonant"
	describe "voiceless aspirated labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̥ʰ"
				`shouldBe`
				"voiceless aspirated labio-dental approximant pulmonic egressive consonant"
		it "should be that: [ʋ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̊ʰ"
				`shouldBe`
				"voiceless aspirated labio-dental approximant pulmonic egressive consonant"
	describe "voiceless aspirated labialized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized labio-dental approximant pulmonic egressive consonant"
		it "should be that: [ʋ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized labio-dental approximant pulmonic egressive consonant"
	describe "voiceless aspirated palatalized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized labio-dental approximant pulmonic egressive consonant"
		it "should be that: [ʋ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized labio-dental approximant pulmonic egressive consonant"
	describe "voiceless aspirated velarized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized labio-dental approximant pulmonic egressive consonant"
		it "should be that: [ʋ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized labio-dental approximant pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized labio-dental approximant pulmonic egressive consonant"
		it "should be that: [ʋ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized labio-dental approximant pulmonic egressive consonant"
	describe "voiced labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋ] \
			 \is the representation of the \
			 \voiced labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ"
				`shouldBe`
				"voiced labio-dental approximant pulmonic egressive consonant"
	describe "voiced labialized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋʷ] \
			 \is the representation of the \
			 \voiced labialized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋʷ"
				`shouldBe`
				"voiced labialized labio-dental approximant pulmonic egressive consonant"
	describe "voiced palatalized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋʲ] \
			 \is the representation of the \
			 \voiced palatalized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋʲ"
				`shouldBe`
				"voiced palatalized labio-dental approximant pulmonic egressive consonant"
	describe "voiced velarized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋˠ] \
			 \is the representation of the \
			 \voiced velarized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋˠ"
				`shouldBe`
				"voiced velarized labio-dental approximant pulmonic egressive consonant"
	describe "voiced pharyngealized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋˤ] \
			 \is the representation of the \
			 \voiced pharyngealized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋˤ"
				`shouldBe`
				"voiced pharyngealized labio-dental approximant pulmonic egressive consonant"
	describe "voiced aspirated labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋʰ] \
			 \is the representation of the \
			 \voiced aspirated labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋʰ"
				`shouldBe`
				"voiced aspirated labio-dental approximant pulmonic egressive consonant"
		it "should be that: [ʋ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̬ʰ"
				`shouldBe`
				"voiced aspirated labio-dental approximant pulmonic egressive consonant"
	describe "voiced aspirated labialized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋʰʷ"
				`shouldBe`
				"voiced aspirated labialized labio-dental approximant pulmonic egressive consonant"
		it "should be that: [ʋ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized labio-dental approximant pulmonic egressive consonant"
	describe "voiced aspirated palatalized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋʰʲ"
				`shouldBe`
				"voiced aspirated palatalized labio-dental approximant pulmonic egressive consonant"
		it "should be that: [ʋ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized labio-dental approximant pulmonic egressive consonant"
	describe "voiced aspirated velarized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋʰˠ"
				`shouldBe`
				"voiced aspirated velarized labio-dental approximant pulmonic egressive consonant"
		it "should be that: [ʋ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized labio-dental approximant pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized labio-dental approximant pulmonic egressive consonant" $
		do
		it "should be that: [ʋʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized labio-dental approximant pulmonic egressive consonant"
		it "should be that: [ʋ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized labio-dental approximant pulmonic egressive consonant" $
			describeIPA "ʋ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized labio-dental approximant pulmonic egressive consonant"
	describe "voiceless alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹ̊] \
			 \is the representation of the \
			 \voiceless alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̊"
				`shouldBe`
				"voiceless alveolar approximant pulmonic egressive consonant"
		it "should be that: [ɹ̥] \
			 \is the representation of the \
			 \voiceless alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̥"
				`shouldBe`
				"voiceless alveolar approximant pulmonic egressive consonant"
	describe "voiceless labialized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̊ʷ"
				`shouldBe`
				"voiceless labialized alveolar approximant pulmonic egressive consonant"
		it "should be that: [ɹ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̥ʷ"
				`shouldBe`
				"voiceless labialized alveolar approximant pulmonic egressive consonant"
	describe "voiceless palatalized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̊ʲ"
				`shouldBe`
				"voiceless palatalized alveolar approximant pulmonic egressive consonant"
		it "should be that: [ɹ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̥ʲ"
				`shouldBe`
				"voiceless palatalized alveolar approximant pulmonic egressive consonant"
	describe "voiceless velarized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̊ˠ"
				`shouldBe`
				"voiceless velarized alveolar approximant pulmonic egressive consonant"
		it "should be that: [ɹ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̥ˠ"
				`shouldBe`
				"voiceless velarized alveolar approximant pulmonic egressive consonant"
	describe "voiceless pharyngealized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar approximant pulmonic egressive consonant"
		it "should be that: [ɹ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar approximant pulmonic egressive consonant"
	describe "voiceless aspirated alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̥ʰ"
				`shouldBe`
				"voiceless aspirated alveolar approximant pulmonic egressive consonant"
		it "should be that: [ɹ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̊ʰ"
				`shouldBe`
				"voiceless aspirated alveolar approximant pulmonic egressive consonant"
	describe "voiceless aspirated labialized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolar approximant pulmonic egressive consonant"
		it "should be that: [ɹ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolar approximant pulmonic egressive consonant"
	describe "voiceless aspirated palatalized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolar approximant pulmonic egressive consonant"
		it "should be that: [ɹ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolar approximant pulmonic egressive consonant"
	describe "voiceless aspirated velarized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolar approximant pulmonic egressive consonant"
		it "should be that: [ɹ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolar approximant pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolar approximant pulmonic egressive consonant"
		it "should be that: [ɹ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolar approximant pulmonic egressive consonant"
	describe "voiced alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹ] \
			 \is the representation of the \
			 \voiced alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ"
				`shouldBe`
				"voiced alveolar approximant pulmonic egressive consonant"
	describe "voiced labialized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹʷ] \
			 \is the representation of the \
			 \voiced labialized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹʷ"
				`shouldBe`
				"voiced labialized alveolar approximant pulmonic egressive consonant"
	describe "voiced palatalized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹʲ] \
			 \is the representation of the \
			 \voiced palatalized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹʲ"
				`shouldBe`
				"voiced palatalized alveolar approximant pulmonic egressive consonant"
	describe "voiced velarized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹˠ] \
			 \is the representation of the \
			 \voiced velarized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹˠ"
				`shouldBe`
				"voiced velarized alveolar approximant pulmonic egressive consonant"
	describe "voiced pharyngealized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹˤ] \
			 \is the representation of the \
			 \voiced pharyngealized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹˤ"
				`shouldBe`
				"voiced pharyngealized alveolar approximant pulmonic egressive consonant"
	describe "voiced aspirated alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹʰ"
				`shouldBe`
				"voiced aspirated alveolar approximant pulmonic egressive consonant"
		it "should be that: [ɹ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̬ʰ"
				`shouldBe`
				"voiced aspirated alveolar approximant pulmonic egressive consonant"
	describe "voiced aspirated labialized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolar approximant pulmonic egressive consonant"
		it "should be that: [ɹ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolar approximant pulmonic egressive consonant"
	describe "voiced aspirated palatalized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolar approximant pulmonic egressive consonant"
		it "should be that: [ɹ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolar approximant pulmonic egressive consonant"
	describe "voiced aspirated velarized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolar approximant pulmonic egressive consonant"
		it "should be that: [ɹ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolar approximant pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized alveolar approximant pulmonic egressive consonant" $
		do
		it "should be that: [ɹʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolar approximant pulmonic egressive consonant"
		it "should be that: [ɹ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolar approximant pulmonic egressive consonant" $
			describeIPA "ɹ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolar approximant pulmonic egressive consonant"
