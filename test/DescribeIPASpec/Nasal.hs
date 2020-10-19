module DescribeIPASpec.Nasal where

import Test.Hspec (Spec, describe, it, shouldBe)
import qualified IPA 
import Data.Text (pack, unpack)

describeIPA :: String -> String
describeIPA = unpack . IPA.describeIPA . pack

nasalConsonantSpec :: Spec
nasalConsonantSpec = do
	describe "voiceless bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [m̊] \
			 \is the representation of the \
			 \voiceless bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̊"
				`shouldBe`
				"voiceless bilabial nasal pulmonic egressive consonant"
		it "should be that: [m̥] \
			 \is the representation of the \
			 \voiceless bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̥"
				`shouldBe`
				"voiceless bilabial nasal pulmonic egressive consonant"
	describe "voiceless labialized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [m̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̊ʷ"
				`shouldBe`
				"voiceless labialized bilabial nasal pulmonic egressive consonant"
		it "should be that: [m̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̥ʷ"
				`shouldBe`
				"voiceless labialized bilabial nasal pulmonic egressive consonant"
	describe "voiceless palatalized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [m̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̊ʲ"
				`shouldBe`
				"voiceless palatalized bilabial nasal pulmonic egressive consonant"
		it "should be that: [m̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̥ʲ"
				`shouldBe`
				"voiceless palatalized bilabial nasal pulmonic egressive consonant"
	describe "voiceless velarized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [m̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̊ˠ"
				`shouldBe`
				"voiceless velarized bilabial nasal pulmonic egressive consonant"
		it "should be that: [m̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̥ˠ"
				`shouldBe`
				"voiceless velarized bilabial nasal pulmonic egressive consonant"
	describe "voiceless pharyngealized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [m̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̊ˤ"
				`shouldBe`
				"voiceless pharyngealized bilabial nasal pulmonic egressive consonant"
		it "should be that: [m̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̥ˤ"
				`shouldBe`
				"voiceless pharyngealized bilabial nasal pulmonic egressive consonant"
	describe "voiceless aspirated bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [m̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̥ʰ"
				`shouldBe`
				"voiceless aspirated bilabial nasal pulmonic egressive consonant"
		it "should be that: [m̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̊ʰ"
				`shouldBe`
				"voiceless aspirated bilabial nasal pulmonic egressive consonant"
	describe "voiceless aspirated labialized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [m̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized bilabial nasal pulmonic egressive consonant"
		it "should be that: [m̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized bilabial nasal pulmonic egressive consonant"
	describe "voiceless aspirated palatalized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [m̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized bilabial nasal pulmonic egressive consonant"
		it "should be that: [m̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized bilabial nasal pulmonic egressive consonant"
	describe "voiceless aspirated velarized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [m̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized bilabial nasal pulmonic egressive consonant"
		it "should be that: [m̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized bilabial nasal pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [m̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized bilabial nasal pulmonic egressive consonant"
		it "should be that: [m̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized bilabial nasal pulmonic egressive consonant"
	describe "voiced bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [m] \
			 \is the representation of the \
			 \voiced bilabial nasal pulmonic egressive consonant" $
			describeIPA "m"
				`shouldBe`
				"voiced bilabial nasal pulmonic egressive consonant"
	describe "voiced labialized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [mʷ] \
			 \is the representation of the \
			 \voiced labialized bilabial nasal pulmonic egressive consonant" $
			describeIPA "mʷ"
				`shouldBe`
				"voiced labialized bilabial nasal pulmonic egressive consonant"
	describe "voiced palatalized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [mʲ] \
			 \is the representation of the \
			 \voiced palatalized bilabial nasal pulmonic egressive consonant" $
			describeIPA "mʲ"
				`shouldBe`
				"voiced palatalized bilabial nasal pulmonic egressive consonant"
	describe "voiced velarized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [mˠ] \
			 \is the representation of the \
			 \voiced velarized bilabial nasal pulmonic egressive consonant" $
			describeIPA "mˠ"
				`shouldBe`
				"voiced velarized bilabial nasal pulmonic egressive consonant"
	describe "voiced pharyngealized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [mˤ] \
			 \is the representation of the \
			 \voiced pharyngealized bilabial nasal pulmonic egressive consonant" $
			describeIPA "mˤ"
				`shouldBe`
				"voiced pharyngealized bilabial nasal pulmonic egressive consonant"
	describe "voiced aspirated bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [mʰ] \
			 \is the representation of the \
			 \voiced aspirated bilabial nasal pulmonic egressive consonant" $
			describeIPA "mʰ"
				`shouldBe`
				"voiced aspirated bilabial nasal pulmonic egressive consonant"
		it "should be that: [m̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̬ʰ"
				`shouldBe`
				"voiced aspirated bilabial nasal pulmonic egressive consonant"
	describe "voiced aspirated labialized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [mʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized bilabial nasal pulmonic egressive consonant" $
			describeIPA "mʰʷ"
				`shouldBe`
				"voiced aspirated labialized bilabial nasal pulmonic egressive consonant"
		it "should be that: [m̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized bilabial nasal pulmonic egressive consonant"
	describe "voiced aspirated palatalized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [mʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized bilabial nasal pulmonic egressive consonant" $
			describeIPA "mʰʲ"
				`shouldBe`
				"voiced aspirated palatalized bilabial nasal pulmonic egressive consonant"
		it "should be that: [m̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized bilabial nasal pulmonic egressive consonant"
	describe "voiced aspirated velarized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [mʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized bilabial nasal pulmonic egressive consonant" $
			describeIPA "mʰˠ"
				`shouldBe`
				"voiced aspirated velarized bilabial nasal pulmonic egressive consonant"
		it "should be that: [m̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized bilabial nasal pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized bilabial nasal pulmonic egressive consonant" $
		do
		it "should be that: [mʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized bilabial nasal pulmonic egressive consonant" $
			describeIPA "mʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized bilabial nasal pulmonic egressive consonant"
		it "should be that: [m̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized bilabial nasal pulmonic egressive consonant" $
			describeIPA "m̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized bilabial nasal pulmonic egressive consonant"
	describe "voiceless alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [n̊] \
			 \is the representation of the \
			 \voiceless alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̊"
				`shouldBe`
				"voiceless alveolar nasal pulmonic egressive consonant"
		it "should be that: [n̥] \
			 \is the representation of the \
			 \voiceless alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̥"
				`shouldBe`
				"voiceless alveolar nasal pulmonic egressive consonant"
	describe "voiceless labialized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [n̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̊ʷ"
				`shouldBe`
				"voiceless labialized alveolar nasal pulmonic egressive consonant"
		it "should be that: [n̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̥ʷ"
				`shouldBe`
				"voiceless labialized alveolar nasal pulmonic egressive consonant"
	describe "voiceless palatalized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [n̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̊ʲ"
				`shouldBe`
				"voiceless palatalized alveolar nasal pulmonic egressive consonant"
		it "should be that: [n̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̥ʲ"
				`shouldBe`
				"voiceless palatalized alveolar nasal pulmonic egressive consonant"
	describe "voiceless velarized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [n̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̊ˠ"
				`shouldBe`
				"voiceless velarized alveolar nasal pulmonic egressive consonant"
		it "should be that: [n̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̥ˠ"
				`shouldBe`
				"voiceless velarized alveolar nasal pulmonic egressive consonant"
	describe "voiceless pharyngealized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [n̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̊ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar nasal pulmonic egressive consonant"
		it "should be that: [n̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̥ˤ"
				`shouldBe`
				"voiceless pharyngealized alveolar nasal pulmonic egressive consonant"
	describe "voiceless aspirated alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [n̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̥ʰ"
				`shouldBe`
				"voiceless aspirated alveolar nasal pulmonic egressive consonant"
		it "should be that: [n̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̊ʰ"
				`shouldBe`
				"voiceless aspirated alveolar nasal pulmonic egressive consonant"
	describe "voiceless aspirated labialized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [n̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolar nasal pulmonic egressive consonant"
		it "should be that: [n̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized alveolar nasal pulmonic egressive consonant"
	describe "voiceless aspirated palatalized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [n̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolar nasal pulmonic egressive consonant"
		it "should be that: [n̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized alveolar nasal pulmonic egressive consonant"
	describe "voiceless aspirated velarized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [n̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolar nasal pulmonic egressive consonant"
		it "should be that: [n̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized alveolar nasal pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [n̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolar nasal pulmonic egressive consonant"
		it "should be that: [n̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized alveolar nasal pulmonic egressive consonant"
	describe "voiced alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [n] \
			 \is the representation of the \
			 \voiced alveolar nasal pulmonic egressive consonant" $
			describeIPA "n"
				`shouldBe`
				"voiced alveolar nasal pulmonic egressive consonant"
	describe "voiced labialized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [nʷ] \
			 \is the representation of the \
			 \voiced labialized alveolar nasal pulmonic egressive consonant" $
			describeIPA "nʷ"
				`shouldBe`
				"voiced labialized alveolar nasal pulmonic egressive consonant"
	describe "voiced palatalized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [nʲ] \
			 \is the representation of the \
			 \voiced palatalized alveolar nasal pulmonic egressive consonant" $
			describeIPA "nʲ"
				`shouldBe`
				"voiced palatalized alveolar nasal pulmonic egressive consonant"
	describe "voiced velarized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [nˠ] \
			 \is the representation of the \
			 \voiced velarized alveolar nasal pulmonic egressive consonant" $
			describeIPA "nˠ"
				`shouldBe`
				"voiced velarized alveolar nasal pulmonic egressive consonant"
	describe "voiced pharyngealized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [nˤ] \
			 \is the representation of the \
			 \voiced pharyngealized alveolar nasal pulmonic egressive consonant" $
			describeIPA "nˤ"
				`shouldBe`
				"voiced pharyngealized alveolar nasal pulmonic egressive consonant"
	describe "voiced aspirated alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [nʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolar nasal pulmonic egressive consonant" $
			describeIPA "nʰ"
				`shouldBe`
				"voiced aspirated alveolar nasal pulmonic egressive consonant"
		it "should be that: [n̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̬ʰ"
				`shouldBe`
				"voiced aspirated alveolar nasal pulmonic egressive consonant"
	describe "voiced aspirated labialized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [nʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolar nasal pulmonic egressive consonant" $
			describeIPA "nʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolar nasal pulmonic egressive consonant"
		it "should be that: [n̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized alveolar nasal pulmonic egressive consonant"
	describe "voiced aspirated palatalized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [nʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolar nasal pulmonic egressive consonant" $
			describeIPA "nʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolar nasal pulmonic egressive consonant"
		it "should be that: [n̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized alveolar nasal pulmonic egressive consonant"
	describe "voiced aspirated velarized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [nʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolar nasal pulmonic egressive consonant" $
			describeIPA "nʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolar nasal pulmonic egressive consonant"
		it "should be that: [n̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized alveolar nasal pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized alveolar nasal pulmonic egressive consonant" $
		do
		it "should be that: [nʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolar nasal pulmonic egressive consonant" $
			describeIPA "nʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolar nasal pulmonic egressive consonant"
		it "should be that: [n̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized alveolar nasal pulmonic egressive consonant" $
			describeIPA "n̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized alveolar nasal pulmonic egressive consonant"
	describe "voiceless palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲ̊] \
			 \is the representation of the \
			 \voiceless palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̊"
				`shouldBe`
				"voiceless palatal nasal pulmonic egressive consonant"
		it "should be that: [ɲ̥] \
			 \is the representation of the \
			 \voiceless palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̥"
				`shouldBe`
				"voiceless palatal nasal pulmonic egressive consonant"
	describe "voiceless labialized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̊ʷ"
				`shouldBe`
				"voiceless labialized palatal nasal pulmonic egressive consonant"
		it "should be that: [ɲ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̥ʷ"
				`shouldBe`
				"voiceless labialized palatal nasal pulmonic egressive consonant"
	describe "voiceless palatalized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̊ʲ"
				`shouldBe`
				"voiceless palatalized palatal nasal pulmonic egressive consonant"
		it "should be that: [ɲ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̥ʲ"
				`shouldBe`
				"voiceless palatalized palatal nasal pulmonic egressive consonant"
	describe "voiceless velarized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̊ˠ"
				`shouldBe`
				"voiceless velarized palatal nasal pulmonic egressive consonant"
		it "should be that: [ɲ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̥ˠ"
				`shouldBe`
				"voiceless velarized palatal nasal pulmonic egressive consonant"
	describe "voiceless pharyngealized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized palatal nasal pulmonic egressive consonant"
		it "should be that: [ɲ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized palatal nasal pulmonic egressive consonant"
	describe "voiceless aspirated palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̥ʰ"
				`shouldBe`
				"voiceless aspirated palatal nasal pulmonic egressive consonant"
		it "should be that: [ɲ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̊ʰ"
				`shouldBe`
				"voiceless aspirated palatal nasal pulmonic egressive consonant"
	describe "voiceless aspirated labialized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized palatal nasal pulmonic egressive consonant"
		it "should be that: [ɲ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized palatal nasal pulmonic egressive consonant"
	describe "voiceless aspirated palatalized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized palatal nasal pulmonic egressive consonant"
		it "should be that: [ɲ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized palatal nasal pulmonic egressive consonant"
	describe "voiceless aspirated velarized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized palatal nasal pulmonic egressive consonant"
		it "should be that: [ɲ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized palatal nasal pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized palatal nasal pulmonic egressive consonant"
		it "should be that: [ɲ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized palatal nasal pulmonic egressive consonant"
	describe "voiced palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲ] \
			 \is the representation of the \
			 \voiced palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ"
				`shouldBe`
				"voiced palatal nasal pulmonic egressive consonant"
	describe "voiced labialized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲʷ] \
			 \is the representation of the \
			 \voiced labialized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲʷ"
				`shouldBe`
				"voiced labialized palatal nasal pulmonic egressive consonant"
	describe "voiced palatalized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲʲ] \
			 \is the representation of the \
			 \voiced palatalized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲʲ"
				`shouldBe`
				"voiced palatalized palatal nasal pulmonic egressive consonant"
	describe "voiced velarized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲˠ] \
			 \is the representation of the \
			 \voiced velarized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲˠ"
				`shouldBe`
				"voiced velarized palatal nasal pulmonic egressive consonant"
	describe "voiced pharyngealized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲˤ] \
			 \is the representation of the \
			 \voiced pharyngealized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲˤ"
				`shouldBe`
				"voiced pharyngealized palatal nasal pulmonic egressive consonant"
	describe "voiced aspirated palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲʰ] \
			 \is the representation of the \
			 \voiced aspirated palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲʰ"
				`shouldBe`
				"voiced aspirated palatal nasal pulmonic egressive consonant"
		it "should be that: [ɲ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̬ʰ"
				`shouldBe`
				"voiced aspirated palatal nasal pulmonic egressive consonant"
	describe "voiced aspirated labialized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲʰʷ"
				`shouldBe`
				"voiced aspirated labialized palatal nasal pulmonic egressive consonant"
		it "should be that: [ɲ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized palatal nasal pulmonic egressive consonant"
	describe "voiced aspirated palatalized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲʰʲ"
				`shouldBe`
				"voiced aspirated palatalized palatal nasal pulmonic egressive consonant"
		it "should be that: [ɲ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized palatal nasal pulmonic egressive consonant"
	describe "voiced aspirated velarized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲʰˠ"
				`shouldBe`
				"voiced aspirated velarized palatal nasal pulmonic egressive consonant"
		it "should be that: [ɲ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized palatal nasal pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized palatal nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɲʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized palatal nasal pulmonic egressive consonant"
		it "should be that: [ɲ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized palatal nasal pulmonic egressive consonant" $
			describeIPA "ɲ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized palatal nasal pulmonic egressive consonant"
	describe "voiceless retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳ̊] \
			 \is the representation of the \
			 \voiceless retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̊"
				`shouldBe`
				"voiceless retroflex nasal pulmonic egressive consonant"
		it "should be that: [ɳ̥] \
			 \is the representation of the \
			 \voiceless retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̥"
				`shouldBe`
				"voiceless retroflex nasal pulmonic egressive consonant"
	describe "voiceless labialized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̊ʷ"
				`shouldBe`
				"voiceless labialized retroflex nasal pulmonic egressive consonant"
		it "should be that: [ɳ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̥ʷ"
				`shouldBe`
				"voiceless labialized retroflex nasal pulmonic egressive consonant"
	describe "voiceless palatalized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̊ʲ"
				`shouldBe`
				"voiceless palatalized retroflex nasal pulmonic egressive consonant"
		it "should be that: [ɳ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̥ʲ"
				`shouldBe`
				"voiceless palatalized retroflex nasal pulmonic egressive consonant"
	describe "voiceless velarized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̊ˠ"
				`shouldBe`
				"voiceless velarized retroflex nasal pulmonic egressive consonant"
		it "should be that: [ɳ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̥ˠ"
				`shouldBe`
				"voiceless velarized retroflex nasal pulmonic egressive consonant"
	describe "voiceless pharyngealized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized retroflex nasal pulmonic egressive consonant"
		it "should be that: [ɳ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized retroflex nasal pulmonic egressive consonant"
	describe "voiceless aspirated retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̥ʰ"
				`shouldBe`
				"voiceless aspirated retroflex nasal pulmonic egressive consonant"
		it "should be that: [ɳ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̊ʰ"
				`shouldBe`
				"voiceless aspirated retroflex nasal pulmonic egressive consonant"
	describe "voiceless aspirated labialized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized retroflex nasal pulmonic egressive consonant"
		it "should be that: [ɳ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized retroflex nasal pulmonic egressive consonant"
	describe "voiceless aspirated palatalized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized retroflex nasal pulmonic egressive consonant"
		it "should be that: [ɳ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized retroflex nasal pulmonic egressive consonant"
	describe "voiceless aspirated velarized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized retroflex nasal pulmonic egressive consonant"
		it "should be that: [ɳ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized retroflex nasal pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized retroflex nasal pulmonic egressive consonant"
		it "should be that: [ɳ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized retroflex nasal pulmonic egressive consonant"
	describe "voiced retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳ] \
			 \is the representation of the \
			 \voiced retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ"
				`shouldBe`
				"voiced retroflex nasal pulmonic egressive consonant"
	describe "voiced labialized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳʷ] \
			 \is the representation of the \
			 \voiced labialized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳʷ"
				`shouldBe`
				"voiced labialized retroflex nasal pulmonic egressive consonant"
	describe "voiced palatalized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳʲ] \
			 \is the representation of the \
			 \voiced palatalized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳʲ"
				`shouldBe`
				"voiced palatalized retroflex nasal pulmonic egressive consonant"
	describe "voiced velarized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳˠ] \
			 \is the representation of the \
			 \voiced velarized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳˠ"
				`shouldBe`
				"voiced velarized retroflex nasal pulmonic egressive consonant"
	describe "voiced pharyngealized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳˤ] \
			 \is the representation of the \
			 \voiced pharyngealized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳˤ"
				`shouldBe`
				"voiced pharyngealized retroflex nasal pulmonic egressive consonant"
	describe "voiced aspirated retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳʰ] \
			 \is the representation of the \
			 \voiced aspirated retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳʰ"
				`shouldBe`
				"voiced aspirated retroflex nasal pulmonic egressive consonant"
		it "should be that: [ɳ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̬ʰ"
				`shouldBe`
				"voiced aspirated retroflex nasal pulmonic egressive consonant"
	describe "voiced aspirated labialized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳʰʷ"
				`shouldBe`
				"voiced aspirated labialized retroflex nasal pulmonic egressive consonant"
		it "should be that: [ɳ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized retroflex nasal pulmonic egressive consonant"
	describe "voiced aspirated palatalized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳʰʲ"
				`shouldBe`
				"voiced aspirated palatalized retroflex nasal pulmonic egressive consonant"
		it "should be that: [ɳ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized retroflex nasal pulmonic egressive consonant"
	describe "voiced aspirated velarized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳʰˠ"
				`shouldBe`
				"voiced aspirated velarized retroflex nasal pulmonic egressive consonant"
		it "should be that: [ɳ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized retroflex nasal pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized retroflex nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɳʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized retroflex nasal pulmonic egressive consonant"
		it "should be that: [ɳ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized retroflex nasal pulmonic egressive consonant" $
			describeIPA "ɳ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized retroflex nasal pulmonic egressive consonant"
	describe "voiceless velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋ̊] \
			 \is the representation of the \
			 \voiceless velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̊"
				`shouldBe`
				"voiceless velar nasal pulmonic egressive consonant"
		it "should be that: [ŋ̥] \
			 \is the representation of the \
			 \voiceless velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̥"
				`shouldBe`
				"voiceless velar nasal pulmonic egressive consonant"
	describe "voiceless labialized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̊ʷ"
				`shouldBe`
				"voiceless labialized velar nasal pulmonic egressive consonant"
		it "should be that: [ŋ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̥ʷ"
				`shouldBe`
				"voiceless labialized velar nasal pulmonic egressive consonant"
	describe "voiceless palatalized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̊ʲ"
				`shouldBe`
				"voiceless palatalized velar nasal pulmonic egressive consonant"
		it "should be that: [ŋ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̥ʲ"
				`shouldBe`
				"voiceless palatalized velar nasal pulmonic egressive consonant"
	describe "voiceless velarized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̊ˠ"
				`shouldBe`
				"voiceless velarized velar nasal pulmonic egressive consonant"
		it "should be that: [ŋ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̥ˠ"
				`shouldBe`
				"voiceless velarized velar nasal pulmonic egressive consonant"
	describe "voiceless pharyngealized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized velar nasal pulmonic egressive consonant"
		it "should be that: [ŋ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized velar nasal pulmonic egressive consonant"
	describe "voiceless aspirated velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̥ʰ"
				`shouldBe`
				"voiceless aspirated velar nasal pulmonic egressive consonant"
		it "should be that: [ŋ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̊ʰ"
				`shouldBe`
				"voiceless aspirated velar nasal pulmonic egressive consonant"
	describe "voiceless aspirated labialized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized velar nasal pulmonic egressive consonant"
		it "should be that: [ŋ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized velar nasal pulmonic egressive consonant"
	describe "voiceless aspirated palatalized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized velar nasal pulmonic egressive consonant"
		it "should be that: [ŋ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized velar nasal pulmonic egressive consonant"
	describe "voiceless aspirated velarized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized velar nasal pulmonic egressive consonant"
		it "should be that: [ŋ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized velar nasal pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized velar nasal pulmonic egressive consonant"
		it "should be that: [ŋ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized velar nasal pulmonic egressive consonant"
	describe "voiced velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋ] \
			 \is the representation of the \
			 \voiced velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ"
				`shouldBe`
				"voiced velar nasal pulmonic egressive consonant"
	describe "voiced labialized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋʷ] \
			 \is the representation of the \
			 \voiced labialized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋʷ"
				`shouldBe`
				"voiced labialized velar nasal pulmonic egressive consonant"
	describe "voiced palatalized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋʲ] \
			 \is the representation of the \
			 \voiced palatalized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋʲ"
				`shouldBe`
				"voiced palatalized velar nasal pulmonic egressive consonant"
	describe "voiced velarized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋˠ] \
			 \is the representation of the \
			 \voiced velarized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋˠ"
				`shouldBe`
				"voiced velarized velar nasal pulmonic egressive consonant"
	describe "voiced pharyngealized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋˤ] \
			 \is the representation of the \
			 \voiced pharyngealized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋˤ"
				`shouldBe`
				"voiced pharyngealized velar nasal pulmonic egressive consonant"
	describe "voiced aspirated velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋʰ] \
			 \is the representation of the \
			 \voiced aspirated velar nasal pulmonic egressive consonant" $
			describeIPA "ŋʰ"
				`shouldBe`
				"voiced aspirated velar nasal pulmonic egressive consonant"
		it "should be that: [ŋ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̬ʰ"
				`shouldBe`
				"voiced aspirated velar nasal pulmonic egressive consonant"
	describe "voiced aspirated labialized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋʰʷ"
				`shouldBe`
				"voiced aspirated labialized velar nasal pulmonic egressive consonant"
		it "should be that: [ŋ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized velar nasal pulmonic egressive consonant"
	describe "voiced aspirated palatalized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋʰʲ"
				`shouldBe`
				"voiced aspirated palatalized velar nasal pulmonic egressive consonant"
		it "should be that: [ŋ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized velar nasal pulmonic egressive consonant"
	describe "voiced aspirated velarized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋʰˠ"
				`shouldBe`
				"voiced aspirated velarized velar nasal pulmonic egressive consonant"
		it "should be that: [ŋ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized velar nasal pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized velar nasal pulmonic egressive consonant" $
		do
		it "should be that: [ŋʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized velar nasal pulmonic egressive consonant"
		it "should be that: [ŋ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized velar nasal pulmonic egressive consonant" $
			describeIPA "ŋ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized velar nasal pulmonic egressive consonant"
	describe "voiceless uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴ̊] \
			 \is the representation of the \
			 \voiceless uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̊"
				`shouldBe`
				"voiceless uvular nasal pulmonic egressive consonant"
		it "should be that: [ɴ̥] \
			 \is the representation of the \
			 \voiceless uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̥"
				`shouldBe`
				"voiceless uvular nasal pulmonic egressive consonant"
	describe "voiceless labialized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴ̊ʷ] \
			 \is the representation of the \
			 \voiceless labialized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̊ʷ"
				`shouldBe`
				"voiceless labialized uvular nasal pulmonic egressive consonant"
		it "should be that: [ɴ̥ʷ] \
			 \is the representation of the \
			 \voiceless labialized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̥ʷ"
				`shouldBe`
				"voiceless labialized uvular nasal pulmonic egressive consonant"
	describe "voiceless palatalized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴ̊ʲ] \
			 \is the representation of the \
			 \voiceless palatalized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̊ʲ"
				`shouldBe`
				"voiceless palatalized uvular nasal pulmonic egressive consonant"
		it "should be that: [ɴ̥ʲ] \
			 \is the representation of the \
			 \voiceless palatalized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̥ʲ"
				`shouldBe`
				"voiceless palatalized uvular nasal pulmonic egressive consonant"
	describe "voiceless velarized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴ̊ˠ] \
			 \is the representation of the \
			 \voiceless velarized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̊ˠ"
				`shouldBe`
				"voiceless velarized uvular nasal pulmonic egressive consonant"
		it "should be that: [ɴ̥ˠ] \
			 \is the representation of the \
			 \voiceless velarized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̥ˠ"
				`shouldBe`
				"voiceless velarized uvular nasal pulmonic egressive consonant"
	describe "voiceless pharyngealized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴ̊ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̊ˤ"
				`shouldBe`
				"voiceless pharyngealized uvular nasal pulmonic egressive consonant"
		it "should be that: [ɴ̥ˤ] \
			 \is the representation of the \
			 \voiceless pharyngealized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̥ˤ"
				`shouldBe`
				"voiceless pharyngealized uvular nasal pulmonic egressive consonant"
	describe "voiceless aspirated uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴ̥ʰ] \
			 \is the representation of the \
			 \voiceless aspirated uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̥ʰ"
				`shouldBe`
				"voiceless aspirated uvular nasal pulmonic egressive consonant"
		it "should be that: [ɴ̊ʰ] \
			 \is the representation of the \
			 \voiceless aspirated uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̊ʰ"
				`shouldBe`
				"voiceless aspirated uvular nasal pulmonic egressive consonant"
	describe "voiceless aspirated labialized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴ̥ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̥ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized uvular nasal pulmonic egressive consonant"
		it "should be that: [ɴ̊ʰʷ] \
			 \is the representation of the \
			 \voiceless aspirated labialized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̊ʰʷ"
				`shouldBe`
				"voiceless aspirated labialized uvular nasal pulmonic egressive consonant"
	describe "voiceless aspirated palatalized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴ̥ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̥ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized uvular nasal pulmonic egressive consonant"
		it "should be that: [ɴ̊ʰʲ] \
			 \is the representation of the \
			 \voiceless aspirated palatalized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̊ʰʲ"
				`shouldBe`
				"voiceless aspirated palatalized uvular nasal pulmonic egressive consonant"
	describe "voiceless aspirated velarized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴ̥ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̥ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized uvular nasal pulmonic egressive consonant"
		it "should be that: [ɴ̊ʰˠ] \
			 \is the representation of the \
			 \voiceless aspirated velarized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̊ʰˠ"
				`shouldBe`
				"voiceless aspirated velarized uvular nasal pulmonic egressive consonant"
	describe "voiceless aspirated pharyngealized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴ̥ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̥ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized uvular nasal pulmonic egressive consonant"
		it "should be that: [ɴ̊ʰˤ] \
			 \is the representation of the \
			 \voiceless aspirated pharyngealized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̊ʰˤ"
				`shouldBe`
				"voiceless aspirated pharyngealized uvular nasal pulmonic egressive consonant"
	describe "voiced uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴ] \
			 \is the representation of the \
			 \voiced uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ"
				`shouldBe`
				"voiced uvular nasal pulmonic egressive consonant"
	describe "voiced labialized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴʷ] \
			 \is the representation of the \
			 \voiced labialized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴʷ"
				`shouldBe`
				"voiced labialized uvular nasal pulmonic egressive consonant"
	describe "voiced palatalized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴʲ] \
			 \is the representation of the \
			 \voiced palatalized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴʲ"
				`shouldBe`
				"voiced palatalized uvular nasal pulmonic egressive consonant"
	describe "voiced velarized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴˠ] \
			 \is the representation of the \
			 \voiced velarized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴˠ"
				`shouldBe`
				"voiced velarized uvular nasal pulmonic egressive consonant"
	describe "voiced pharyngealized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴˤ] \
			 \is the representation of the \
			 \voiced pharyngealized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴˤ"
				`shouldBe`
				"voiced pharyngealized uvular nasal pulmonic egressive consonant"
	describe "voiced aspirated uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴʰ] \
			 \is the representation of the \
			 \voiced aspirated uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴʰ"
				`shouldBe`
				"voiced aspirated uvular nasal pulmonic egressive consonant"
		it "should be that: [ɴ̬ʰ] \
			 \is the representation of the \
			 \voiced aspirated uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̬ʰ"
				`shouldBe`
				"voiced aspirated uvular nasal pulmonic egressive consonant"
	describe "voiced aspirated labialized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴʰʷ"
				`shouldBe`
				"voiced aspirated labialized uvular nasal pulmonic egressive consonant"
		it "should be that: [ɴ̬ʰʷ] \
			 \is the representation of the \
			 \voiced aspirated labialized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̬ʰʷ"
				`shouldBe`
				"voiced aspirated labialized uvular nasal pulmonic egressive consonant"
	describe "voiced aspirated palatalized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴʰʲ"
				`shouldBe`
				"voiced aspirated palatalized uvular nasal pulmonic egressive consonant"
		it "should be that: [ɴ̬ʰʲ] \
			 \is the representation of the \
			 \voiced aspirated palatalized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̬ʰʲ"
				`shouldBe`
				"voiced aspirated palatalized uvular nasal pulmonic egressive consonant"
	describe "voiced aspirated velarized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴʰˠ"
				`shouldBe`
				"voiced aspirated velarized uvular nasal pulmonic egressive consonant"
		it "should be that: [ɴ̬ʰˠ] \
			 \is the representation of the \
			 \voiced aspirated velarized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̬ʰˠ"
				`shouldBe`
				"voiced aspirated velarized uvular nasal pulmonic egressive consonant"
	describe "voiced aspirated pharyngealized uvular nasal pulmonic egressive consonant" $
		do
		it "should be that: [ɴʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized uvular nasal pulmonic egressive consonant"
		it "should be that: [ɴ̬ʰˤ] \
			 \is the representation of the \
			 \voiced aspirated pharyngealized uvular nasal pulmonic egressive consonant" $
			describeIPA "ɴ̬ʰˤ"
				`shouldBe`
				"voiced aspirated pharyngealized uvular nasal pulmonic egressive consonant"
