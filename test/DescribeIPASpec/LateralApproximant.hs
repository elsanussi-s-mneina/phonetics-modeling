module DescribeIPASpec.LateralApproximant where

import Test.Hspec    (Spec, describe, it, shouldBe)
import qualified IPA 
import Data.Text (pack, unpack)
describeIPA = unpack . IPA.describeIPA . pack

lateralApproximantConsonantSpec :: Spec
lateralApproximantConsonantSpec = do
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
  describe "voiceless retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̊] \
       \is the representation of the \
       \voiceless retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊"
        `shouldBe`
        "voiceless retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̥] \
       \is the representation of the \
       \voiceless retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥"
        `shouldBe`
        "voiceless retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless labialized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̊ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ʷ"
        `shouldBe`
        "voiceless labialized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̥ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ʷ"
        `shouldBe`
        "voiceless labialized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless palatalized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ʲ"
        `shouldBe`
        "voiceless palatalized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ʲ"
        `shouldBe`
        "voiceless palatalized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless velarized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̊ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ˠ"
        `shouldBe`
        "voiceless velarized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̥ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ˠ"
        `shouldBe`
        "voiceless velarized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ʰ"
        `shouldBe`
        "voiceless aspirated retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ʰ"
        `shouldBe`
        "voiceless aspirated retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated labialized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated palatalized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated velarized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭ] \
       \is the representation of the \
       \voiced retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ"
        `shouldBe`
        "voiced retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced labialized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭʷ] \
       \is the representation of the \
       \voiced labialized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭʷ"
        `shouldBe`
        "voiced labialized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced palatalized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭʲ] \
       \is the representation of the \
       \voiced palatalized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭʲ"
        `shouldBe`
        "voiced palatalized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced velarized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭˠ] \
       \is the representation of the \
       \voiced velarized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭˠ"
        `shouldBe`
        "voiced velarized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭˤ] \
       \is the representation of the \
       \voiced pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭˤ"
        `shouldBe`
        "voiced pharyngealized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭʰ] \
       \is the representation of the \
       \voiced aspirated retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭʰ"
        `shouldBe`
        "voiced aspirated retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̬ʰ] \
       \is the representation of the \
       \voiced aspirated retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̬ʰ"
        `shouldBe`
        "voiced aspirated retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated labialized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated palatalized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated velarized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ɭʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant"
    it "should be that: [ɭ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant" $
      describeIPA "ɭ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex lateral approximant pulmonic egressive consonant"
  describe "voiceless palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̊] \
       \is the representation of the \
       \voiceless palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊"
        `shouldBe`
        "voiceless palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̥] \
       \is the representation of the \
       \voiceless palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥"
        `shouldBe`
        "voiceless palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless labialized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̊ʷ] \
       \is the representation of the \
       \voiceless labialized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ʷ"
        `shouldBe`
        "voiceless labialized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̥ʷ] \
       \is the representation of the \
       \voiceless labialized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ʷ"
        `shouldBe`
        "voiceless labialized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless palatalized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ʲ"
        `shouldBe`
        "voiceless palatalized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ʲ"
        `shouldBe`
        "voiceless palatalized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless velarized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̊ˠ] \
       \is the representation of the \
       \voiceless velarized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ˠ"
        `shouldBe`
        "voiceless velarized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̥ˠ] \
       \is the representation of the \
       \voiceless velarized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ˠ"
        `shouldBe`
        "voiceless velarized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless pharyngealized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ʰ"
        `shouldBe`
        "voiceless aspirated palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ʰ"
        `shouldBe`
        "voiceless aspirated palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated labialized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated palatalized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated velarized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎ] \
       \is the representation of the \
       \voiced palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ"
        `shouldBe`
        "voiced palatal lateral approximant pulmonic egressive consonant"
  describe "voiced labialized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎʷ] \
       \is the representation of the \
       \voiced labialized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎʷ"
        `shouldBe`
        "voiced labialized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced palatalized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎʲ] \
       \is the representation of the \
       \voiced palatalized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎʲ"
        `shouldBe`
        "voiced palatalized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced velarized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎˠ] \
       \is the representation of the \
       \voiced velarized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎˠ"
        `shouldBe`
        "voiced velarized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced pharyngealized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎˤ] \
       \is the representation of the \
       \voiced pharyngealized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎˤ"
        `shouldBe`
        "voiced pharyngealized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎʰ] \
       \is the representation of the \
       \voiced aspirated palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎʰ"
        `shouldBe`
        "voiced aspirated palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̬ʰ] \
       \is the representation of the \
       \voiced aspirated palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̬ʰ"
        `shouldBe`
        "voiced aspirated palatal lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated labialized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎʰʷ"
        `shouldBe`
        "voiced aspirated labialized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated palatalized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎʰʲ"
        `shouldBe`
        "voiced aspirated palatalized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated velarized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎʰˠ"
        `shouldBe`
        "voiced aspirated velarized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized palatal lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʎʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant"
    it "should be that: [ʎ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant" $
      describeIPA "ʎ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized palatal lateral approximant pulmonic egressive consonant"
  describe "voiceless velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̊] \
       \is the representation of the \
       \voiceless velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊"
        `shouldBe`
        "voiceless velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̥] \
       \is the representation of the \
       \voiceless velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥"
        `shouldBe`
        "voiceless velar lateral approximant pulmonic egressive consonant"
  describe "voiceless labialized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̊ʷ] \
       \is the representation of the \
       \voiceless labialized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ʷ"
        `shouldBe`
        "voiceless labialized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̥ʷ] \
       \is the representation of the \
       \voiceless labialized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ʷ"
        `shouldBe`
        "voiceless labialized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless palatalized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ʲ"
        `shouldBe`
        "voiceless palatalized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ʲ"
        `shouldBe`
        "voiceless palatalized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless velarized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̊ˠ] \
       \is the representation of the \
       \voiceless velarized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ˠ"
        `shouldBe`
        "voiceless velarized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̥ˠ] \
       \is the representation of the \
       \voiceless velarized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ˠ"
        `shouldBe`
        "voiceless velarized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless pharyngealized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ʰ"
        `shouldBe`
        "voiceless aspirated velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ʰ"
        `shouldBe`
        "voiceless aspirated velar lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated labialized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated palatalized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated velarized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized velar lateral approximant pulmonic egressive consonant"
  describe "voiced velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟ] \
       \is the representation of the \
       \voiced velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ"
        `shouldBe`
        "voiced velar lateral approximant pulmonic egressive consonant"
  describe "voiced labialized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟʷ] \
       \is the representation of the \
       \voiced labialized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟʷ"
        `shouldBe`
        "voiced labialized velar lateral approximant pulmonic egressive consonant"
  describe "voiced palatalized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟʲ] \
       \is the representation of the \
       \voiced palatalized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟʲ"
        `shouldBe`
        "voiced palatalized velar lateral approximant pulmonic egressive consonant"
  describe "voiced velarized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟˠ] \
       \is the representation of the \
       \voiced velarized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟˠ"
        `shouldBe`
        "voiced velarized velar lateral approximant pulmonic egressive consonant"
  describe "voiced pharyngealized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟˤ] \
       \is the representation of the \
       \voiced pharyngealized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟˤ"
        `shouldBe`
        "voiced pharyngealized velar lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟʰ] \
       \is the representation of the \
       \voiced aspirated velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟʰ"
        `shouldBe`
        "voiced aspirated velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̬ʰ] \
       \is the representation of the \
       \voiced aspirated velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̬ʰ"
        `shouldBe`
        "voiced aspirated velar lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated labialized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟʰʷ"
        `shouldBe`
        "voiced aspirated labialized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized velar lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated palatalized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟʰʲ"
        `shouldBe`
        "voiced aspirated palatalized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized velar lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated velarized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟʰˠ"
        `shouldBe`
        "voiced aspirated velarized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized velar lateral approximant pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized velar lateral approximant pulmonic egressive consonant" $
    do
    it "should be that: [ʟʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized velar lateral approximant pulmonic egressive consonant"
    it "should be that: [ʟ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized velar lateral approximant pulmonic egressive consonant" $
      describeIPA "ʟ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized velar lateral approximant pulmonic egressive consonant"
  describe "voiceless labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ẘ] \
       \is the representation of the \
       \voiceless labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘ"
        `shouldBe`
        "voiceless labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̥] \
       \is the representation of the \
       \voiceless labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥"
        `shouldBe`
        "voiceless labial-velar approximant pulmonic egressive consonant"
  describe "voiceless labialized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ẘʷ] \
       \is the representation of the \
       \voiceless labialized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘʷ"
        `shouldBe`
        "voiceless labialized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̥ʷ] \
       \is the representation of the \
       \voiceless labialized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ʷ"
        `shouldBe`
        "voiceless labialized labial-velar approximant pulmonic egressive consonant"
  describe "voiceless palatalized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ẘʲ] \
       \is the representation of the \
       \voiceless palatalized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘʲ"
        `shouldBe`
        "voiceless palatalized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̥ʲ] \
       \is the representation of the \
       \voiceless palatalized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ʲ"
        `shouldBe`
        "voiceless palatalized labial-velar approximant pulmonic egressive consonant"
  describe "voiceless velarized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ẘˠ] \
       \is the representation of the \
       \voiceless velarized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘˠ"
        `shouldBe`
        "voiceless velarized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̥ˠ] \
       \is the representation of the \
       \voiceless velarized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ˠ"
        `shouldBe`
        "voiceless velarized labial-velar approximant pulmonic egressive consonant"
  describe "voiceless pharyngealized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [ẘˤ] \
       \is the representation of the \
       \voiceless pharyngealized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘˤ"
        `shouldBe`
        "voiceless pharyngealized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ˤ"
        `shouldBe`
        "voiceless pharyngealized labial-velar approximant pulmonic egressive consonant"
  describe "voiceless aspirated labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [w̥ʰ] \
       \is the representation of the \
       \voiceless aspirated labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ʰ"
        `shouldBe`
        "voiceless aspirated labial-velar approximant pulmonic egressive consonant"
    it "should be that: [ẘʰ] \
       \is the representation of the \
       \voiceless aspirated labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘʰ"
        `shouldBe`
        "voiceless aspirated labial-velar approximant pulmonic egressive consonant"
  describe "voiceless aspirated labialized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [w̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [ẘʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘʰʷ"
        `shouldBe`
        "voiceless aspirated labialized labial-velar approximant pulmonic egressive consonant"
  describe "voiceless aspirated palatalized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [w̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [ẘʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized labial-velar approximant pulmonic egressive consonant"
  describe "voiceless aspirated velarized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [w̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [ẘʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘʰˠ"
        `shouldBe`
        "voiceless aspirated velarized labial-velar approximant pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [w̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [ẘʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "ẘʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized labial-velar approximant pulmonic egressive consonant"
  describe "voiced labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [w] \
       \is the representation of the \
       \voiced labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w"
        `shouldBe`
        "voiced labial-velar approximant pulmonic egressive consonant"
  describe "voiced labialized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wʷ] \
       \is the representation of the \
       \voiced labialized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wʷ"
        `shouldBe`
        "voiced labialized labial-velar approximant pulmonic egressive consonant"
  describe "voiced palatalized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wʲ] \
       \is the representation of the \
       \voiced palatalized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wʲ"
        `shouldBe`
        "voiced palatalized labial-velar approximant pulmonic egressive consonant"
  describe "voiced velarized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wˠ] \
       \is the representation of the \
       \voiced velarized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wˠ"
        `shouldBe`
        "voiced velarized labial-velar approximant pulmonic egressive consonant"
  describe "voiced pharyngealized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wˤ] \
       \is the representation of the \
       \voiced pharyngealized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wˤ"
        `shouldBe`
        "voiced pharyngealized labial-velar approximant pulmonic egressive consonant"
  describe "voiced aspirated labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wʰ] \
       \is the representation of the \
       \voiced aspirated labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wʰ"
        `shouldBe`
        "voiced aspirated labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̬ʰ] \
       \is the representation of the \
       \voiced aspirated labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̬ʰ"
        `shouldBe`
        "voiced aspirated labial-velar approximant pulmonic egressive consonant"
  describe "voiced aspirated labialized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wʰʷ"
        `shouldBe`
        "voiced aspirated labialized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized labial-velar approximant pulmonic egressive consonant"
  describe "voiced aspirated palatalized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wʰʲ"
        `shouldBe`
        "voiced aspirated palatalized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized labial-velar approximant pulmonic egressive consonant"
  describe "voiced aspirated velarized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wʰˠ"
        `shouldBe`
        "voiced aspirated velarized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized labial-velar approximant pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized labial-velar approximant pulmonic egressive consonant" $
    do
    it "should be that: [wʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "wʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized labial-velar approximant pulmonic egressive consonant"
    it "should be that: [w̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized labial-velar approximant pulmonic egressive consonant" $
      describeIPA "w̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized labial-velar approximant pulmonic egressive consonant"