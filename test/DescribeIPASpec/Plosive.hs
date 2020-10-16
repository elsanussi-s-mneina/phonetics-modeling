module DescribeIPASpec.Plosive where

import Test.Hspec    (Spec, describe, it, shouldBe)
import qualified IPA 
import Data.Text (pack, unpack)
describeIPA = unpack . IPA.describeIPA . pack

plosiveConsonantSpec :: Spec
plosiveConsonantSpec = do
  describe "voiceless bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [p] \
       \is the representation of the \
       \voiceless bilabial plosive pulmonic egressive consonant" $
      describeIPA "p"
        `shouldBe`
        "voiceless bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̊] \
       \is the representation of the \
       \voiceless bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̊"
        `shouldBe`
        "voiceless bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̥] \
       \is the representation of the \
       \voiceless bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̥"
        `shouldBe`
        "voiceless bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊] \
       \is the representation of the \
       \voiceless bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊"
        `shouldBe`
        "voiceless bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥] \
       \is the representation of the \
       \voiceless bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥"
        `shouldBe`
        "voiceless bilabial plosive pulmonic egressive consonant"
  describe "voiceless labialized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pʷ] \
       \is the representation of the \
       \voiceless labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pʷ"
        `shouldBe`
        "voiceless labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̊ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̊ʷ"
        `shouldBe`
        "voiceless labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̥ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̥ʷ"
        `shouldBe`
        "voiceless labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ʷ"
        `shouldBe`
        "voiceless labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ʷ] \
       \is the representation of the \
       \voiceless labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ʷ"
        `shouldBe`
        "voiceless labialized bilabial plosive pulmonic egressive consonant"
  describe "voiceless palatalized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pʲ"
        `shouldBe`
        "voiceless palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̊ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̊ʲ"
        `shouldBe`
        "voiceless palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̥ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̥ʲ"
        `shouldBe`
        "voiceless palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ʲ"
        `shouldBe`
        "voiceless palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ʲ] \
       \is the representation of the \
       \voiceless palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ʲ"
        `shouldBe`
        "voiceless palatalized bilabial plosive pulmonic egressive consonant"
  describe "voiceless velarized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pˠ] \
       \is the representation of the \
       \voiceless velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pˠ"
        `shouldBe`
        "voiceless velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̊ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̊ˠ"
        `shouldBe`
        "voiceless velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̥ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̥ˠ"
        `shouldBe`
        "voiceless velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ˠ"
        `shouldBe`
        "voiceless velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ˠ] \
       \is the representation of the \
       \voiceless velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ˠ"
        `shouldBe`
        "voiceless velarized bilabial plosive pulmonic egressive consonant"
  describe "voiceless pharyngealized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̊ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̥ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ˤ"
        `shouldBe`
        "voiceless pharyngealized bilabial plosive pulmonic egressive consonant"
  describe "voiceless aspirated bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pʰ] \
       \is the representation of the \
       \voiceless aspirated bilabial plosive pulmonic egressive consonant" $
      describeIPA "pʰ"
        `shouldBe`
        "voiceless aspirated bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ʰ] \
       \is the representation of the \
       \voiceless aspirated bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ʰ"
        `shouldBe`
        "voiceless aspirated bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ʰ] \
       \is the representation of the \
       \voiceless aspirated bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ʰ"
        `shouldBe`
        "voiceless aspirated bilabial plosive pulmonic egressive consonant"
  describe "voiceless aspirated labialized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pʰʷ"
        `shouldBe`
        "voiceless aspirated labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized bilabial plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatalized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized bilabial plosive pulmonic egressive consonant"
  describe "voiceless aspirated velarized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pʰˠ"
        `shouldBe`
        "voiceless aspirated velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized bilabial plosive pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [pʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "pʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized bilabial plosive pulmonic egressive consonant"
  describe "voiced bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [b] \
       \is the representation of the \
       \voiced bilabial plosive pulmonic egressive consonant" $
      describeIPA "b"
        `shouldBe`
        "voiced bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬] \
       \is the representation of the \
       \voiced bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬"
        `shouldBe`
        "voiced bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬] \
       \is the representation of the \
       \voiced bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬"
        `shouldBe`
        "voiced bilabial plosive pulmonic egressive consonant"
  describe "voiced labialized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bʷ] \
       \is the representation of the \
       \voiced labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bʷ"
        `shouldBe`
        "voiced labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʷ] \
       \is the representation of the \
       \voiced labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʷ"
        `shouldBe`
        "voiced labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʷ] \
       \is the representation of the \
       \voiced labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʷ"
        `shouldBe`
        "voiced labialized bilabial plosive pulmonic egressive consonant"
  describe "voiced palatalized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bʲ] \
       \is the representation of the \
       \voiced palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bʲ"
        `shouldBe`
        "voiced palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʲ] \
       \is the representation of the \
       \voiced palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʲ"
        `shouldBe`
        "voiced palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʲ] \
       \is the representation of the \
       \voiced palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʲ"
        `shouldBe`
        "voiced palatalized bilabial plosive pulmonic egressive consonant"
  describe "voiced velarized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bˠ] \
       \is the representation of the \
       \voiced velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bˠ"
        `shouldBe`
        "voiced velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ˠ] \
       \is the representation of the \
       \voiced velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ˠ"
        `shouldBe`
        "voiced velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ˠ] \
       \is the representation of the \
       \voiced velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ˠ"
        `shouldBe`
        "voiced velarized bilabial plosive pulmonic egressive consonant"
  describe "voiced pharyngealized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bˤ] \
       \is the representation of the \
       \voiced pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bˤ"
        `shouldBe`
        "voiced pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ˤ"
        `shouldBe`
        "voiced pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ˤ"
        `shouldBe`
        "voiced pharyngealized bilabial plosive pulmonic egressive consonant"
  describe "voiced aspirated bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bʰ] \
       \is the representation of the \
       \voiced aspirated bilabial plosive pulmonic egressive consonant" $
      describeIPA "bʰ"
        `shouldBe`
        "voiced aspirated bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̬ʰ] \
       \is the representation of the \
       \voiced aspirated bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̬ʰ"
        `shouldBe`
        "voiced aspirated bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʰ] \
       \is the representation of the \
       \voiced aspirated bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʰ"
        `shouldBe`
        "voiced aspirated bilabial plosive pulmonic egressive consonant"
  describe "voiced aspirated labialized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bʰʷ"
        `shouldBe`
        "voiced aspirated labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized bilabial plosive pulmonic egressive consonant"
  describe "voiced aspirated palatalized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bʰʲ"
        `shouldBe`
        "voiced aspirated palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized bilabial plosive pulmonic egressive consonant"
  describe "voiced aspirated velarized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bʰˠ"
        `shouldBe`
        "voiced aspirated velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized bilabial plosive pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
    do
    it "should be that: [bʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "bʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [b̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "b̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized bilabial plosive pulmonic egressive consonant"
    it "should be that: [p̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized bilabial plosive pulmonic egressive consonant" $
      describeIPA "p̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized bilabial plosive pulmonic egressive consonant"
  describe "voiceless alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [t] \
       \is the representation of the \
       \voiceless alveolar plosive pulmonic egressive consonant" $
      describeIPA "t"
        `shouldBe`
        "voiceless alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̊] \
       \is the representation of the \
       \voiceless alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̊"
        `shouldBe`
        "voiceless alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̥] \
       \is the representation of the \
       \voiceless alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̥"
        `shouldBe`
        "voiceless alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊] \
       \is the representation of the \
       \voiceless alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊"
        `shouldBe`
        "voiceless alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥] \
       \is the representation of the \
       \voiceless alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥"
        `shouldBe`
        "voiceless alveolar plosive pulmonic egressive consonant"
  describe "voiceless labialized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tʷ] \
       \is the representation of the \
       \voiceless labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tʷ"
        `shouldBe`
        "voiceless labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̊ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̊ʷ"
        `shouldBe`
        "voiceless labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̥ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̥ʷ"
        `shouldBe`
        "voiceless labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ʷ"
        `shouldBe`
        "voiceless labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ʷ] \
       \is the representation of the \
       \voiceless labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ʷ"
        `shouldBe`
        "voiceless labialized alveolar plosive pulmonic egressive consonant"
  describe "voiceless palatalized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tʲ"
        `shouldBe`
        "voiceless palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̊ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̊ʲ"
        `shouldBe`
        "voiceless palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̥ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̥ʲ"
        `shouldBe`
        "voiceless palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ʲ"
        `shouldBe`
        "voiceless palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ʲ] \
       \is the representation of the \
       \voiceless palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ʲ"
        `shouldBe`
        "voiceless palatalized alveolar plosive pulmonic egressive consonant"
  describe "voiceless velarized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tˠ] \
       \is the representation of the \
       \voiceless velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tˠ"
        `shouldBe`
        "voiceless velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̊ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̊ˠ"
        `shouldBe`
        "voiceless velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̥ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̥ˠ"
        `shouldBe`
        "voiceless velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ˠ"
        `shouldBe`
        "voiceless velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ˠ] \
       \is the representation of the \
       \voiceless velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ˠ"
        `shouldBe`
        "voiceless velarized alveolar plosive pulmonic egressive consonant"
  describe "voiceless pharyngealized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̊ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̥ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ˤ"
        `shouldBe`
        "voiceless pharyngealized alveolar plosive pulmonic egressive consonant"
  describe "voiceless aspirated alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar plosive pulmonic egressive consonant" $
      describeIPA "tʰ"
        `shouldBe`
        "voiceless aspirated alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ʰ"
        `shouldBe`
        "voiceless aspirated alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ʰ] \
       \is the representation of the \
       \voiceless aspirated alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ʰ"
        `shouldBe`
        "voiceless aspirated alveolar plosive pulmonic egressive consonant"
  describe "voiceless aspirated labialized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized alveolar plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatalized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized alveolar plosive pulmonic egressive consonant"
  describe "voiceless aspirated velarized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized alveolar plosive pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [tʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "tʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized alveolar plosive pulmonic egressive consonant"
  describe "voiced alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [d] \
       \is the representation of the \
       \voiced alveolar plosive pulmonic egressive consonant" $
      describeIPA "d"
        `shouldBe`
        "voiced alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬] \
       \is the representation of the \
       \voiced alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬"
        `shouldBe`
        "voiced alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬] \
       \is the representation of the \
       \voiced alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬"
        `shouldBe`
        "voiced alveolar plosive pulmonic egressive consonant"
  describe "voiced labialized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dʷ] \
       \is the representation of the \
       \voiced labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dʷ"
        `shouldBe`
        "voiced labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʷ] \
       \is the representation of the \
       \voiced labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʷ"
        `shouldBe`
        "voiced labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʷ] \
       \is the representation of the \
       \voiced labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʷ"
        `shouldBe`
        "voiced labialized alveolar plosive pulmonic egressive consonant"
  describe "voiced palatalized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dʲ] \
       \is the representation of the \
       \voiced palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dʲ"
        `shouldBe`
        "voiced palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʲ] \
       \is the representation of the \
       \voiced palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʲ"
        `shouldBe`
        "voiced palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʲ] \
       \is the representation of the \
       \voiced palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʲ"
        `shouldBe`
        "voiced palatalized alveolar plosive pulmonic egressive consonant"
  describe "voiced velarized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dˠ] \
       \is the representation of the \
       \voiced velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dˠ"
        `shouldBe`
        "voiced velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ˠ] \
       \is the representation of the \
       \voiced velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ˠ"
        `shouldBe`
        "voiced velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ˠ] \
       \is the representation of the \
       \voiced velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ˠ"
        `shouldBe`
        "voiced velarized alveolar plosive pulmonic egressive consonant"
  describe "voiced pharyngealized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dˤ"
        `shouldBe`
        "voiced pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ˤ"
        `shouldBe`
        "voiced pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ˤ"
        `shouldBe`
        "voiced pharyngealized alveolar plosive pulmonic egressive consonant"
  describe "voiced aspirated alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dʰ] \
       \is the representation of the \
       \voiced aspirated alveolar plosive pulmonic egressive consonant" $
      describeIPA "dʰ"
        `shouldBe`
        "voiced aspirated alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̬ʰ] \
       \is the representation of the \
       \voiced aspirated alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̬ʰ"
        `shouldBe`
        "voiced aspirated alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʰ] \
       \is the representation of the \
       \voiced aspirated alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʰ"
        `shouldBe`
        "voiced aspirated alveolar plosive pulmonic egressive consonant"
  describe "voiced aspirated labialized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized alveolar plosive pulmonic egressive consonant"
  describe "voiced aspirated palatalized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized alveolar plosive pulmonic egressive consonant"
  describe "voiced aspirated velarized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized alveolar plosive pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
    do
    it "should be that: [dʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "dʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [d̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "d̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar plosive pulmonic egressive consonant"
    it "should be that: [t̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized alveolar plosive pulmonic egressive consonant" $
      describeIPA "t̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized alveolar plosive pulmonic egressive consonant"
  describe "voiceless retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈ] \
       \is the representation of the \
       \voiceless retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ"
        `shouldBe`
        "voiceless retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̊] \
       \is the representation of the \
       \voiceless retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̊"
        `shouldBe`
        "voiceless retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̥] \
       \is the representation of the \
       \voiceless retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̥"
        `shouldBe`
        "voiceless retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊] \
       \is the representation of the \
       \voiceless retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊"
        `shouldBe`
        "voiceless retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥] \
       \is the representation of the \
       \voiceless retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥"
        `shouldBe`
        "voiceless retroflex plosive pulmonic egressive consonant"
  describe "voiceless labialized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈʷ] \
       \is the representation of the \
       \voiceless labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈʷ"
        `shouldBe`
        "voiceless labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̊ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̊ʷ"
        `shouldBe`
        "voiceless labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̥ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̥ʷ"
        `shouldBe`
        "voiceless labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ʷ"
        `shouldBe`
        "voiceless labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ʷ] \
       \is the representation of the \
       \voiceless labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ʷ"
        `shouldBe`
        "voiceless labialized retroflex plosive pulmonic egressive consonant"
  describe "voiceless palatalized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈʲ"
        `shouldBe`
        "voiceless palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̊ʲ"
        `shouldBe`
        "voiceless palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̥ʲ"
        `shouldBe`
        "voiceless palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ʲ"
        `shouldBe`
        "voiceless palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ʲ"
        `shouldBe`
        "voiceless palatalized retroflex plosive pulmonic egressive consonant"
  describe "voiceless velarized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈˠ] \
       \is the representation of the \
       \voiceless velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈˠ"
        `shouldBe`
        "voiceless velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̊ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̊ˠ"
        `shouldBe`
        "voiceless velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̥ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̥ˠ"
        `shouldBe`
        "voiceless velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ˠ"
        `shouldBe`
        "voiceless velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ˠ] \
       \is the representation of the \
       \voiceless velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ˠ"
        `shouldBe`
        "voiceless velarized retroflex plosive pulmonic egressive consonant"
  describe "voiceless pharyngealized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized retroflex plosive pulmonic egressive consonant"
  describe "voiceless aspirated retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈʰ"
        `shouldBe`
        "voiceless aspirated retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ʰ"
        `shouldBe`
        "voiceless aspirated retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ʰ"
        `shouldBe`
        "voiceless aspirated retroflex plosive pulmonic egressive consonant"
  describe "voiceless aspirated labialized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized retroflex plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatalized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized retroflex plosive pulmonic egressive consonant"
  describe "voiceless aspirated velarized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized retroflex plosive pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʈʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized retroflex plosive pulmonic egressive consonant"
  describe "voiced retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖ] \
       \is the representation of the \
       \voiced retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ"
        `shouldBe`
        "voiced retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬] \
       \is the representation of the \
       \voiced retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬"
        `shouldBe`
        "voiced retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬] \
       \is the representation of the \
       \voiced retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬"
        `shouldBe`
        "voiced retroflex plosive pulmonic egressive consonant"
  describe "voiced labialized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖʷ] \
       \is the representation of the \
       \voiced labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖʷ"
        `shouldBe`
        "voiced labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʷ] \
       \is the representation of the \
       \voiced labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʷ"
        `shouldBe`
        "voiced labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʷ] \
       \is the representation of the \
       \voiced labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʷ"
        `shouldBe`
        "voiced labialized retroflex plosive pulmonic egressive consonant"
  describe "voiced palatalized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖʲ] \
       \is the representation of the \
       \voiced palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖʲ"
        `shouldBe`
        "voiced palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʲ] \
       \is the representation of the \
       \voiced palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʲ"
        `shouldBe`
        "voiced palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʲ] \
       \is the representation of the \
       \voiced palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʲ"
        `shouldBe`
        "voiced palatalized retroflex plosive pulmonic egressive consonant"
  describe "voiced velarized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖˠ] \
       \is the representation of the \
       \voiced velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖˠ"
        `shouldBe`
        "voiced velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ˠ] \
       \is the representation of the \
       \voiced velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ˠ"
        `shouldBe`
        "voiced velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ˠ] \
       \is the representation of the \
       \voiced velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ˠ"
        `shouldBe`
        "voiced velarized retroflex plosive pulmonic egressive consonant"
  describe "voiced pharyngealized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖˤ] \
       \is the representation of the \
       \voiced pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖˤ"
        `shouldBe`
        "voiced pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ˤ"
        `shouldBe`
        "voiced pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ˤ"
        `shouldBe`
        "voiced pharyngealized retroflex plosive pulmonic egressive consonant"
  describe "voiced aspirated retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖʰ] \
       \is the representation of the \
       \voiced aspirated retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖʰ"
        `shouldBe`
        "voiced aspirated retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̬ʰ] \
       \is the representation of the \
       \voiced aspirated retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̬ʰ"
        `shouldBe`
        "voiced aspirated retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʰ] \
       \is the representation of the \
       \voiced aspirated retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʰ"
        `shouldBe`
        "voiced aspirated retroflex plosive pulmonic egressive consonant"
  describe "voiced aspirated labialized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized retroflex plosive pulmonic egressive consonant"
  describe "voiced aspirated palatalized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized retroflex plosive pulmonic egressive consonant"
  describe "voiced aspirated velarized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized retroflex plosive pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɖʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ɖ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ɖ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex plosive pulmonic egressive consonant"
    it "should be that: [ʈ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized retroflex plosive pulmonic egressive consonant" $
      describeIPA "ʈ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized retroflex plosive pulmonic egressive consonant"
  describe "voiceless palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [c] \
       \is the representation of the \
       \voiceless palatal plosive pulmonic egressive consonant" $
      describeIPA "c"
        `shouldBe`
        "voiceless palatal plosive pulmonic egressive consonant"
    it "should be that: [c̊] \
       \is the representation of the \
       \voiceless palatal plosive pulmonic egressive consonant" $
      describeIPA "c̊"
        `shouldBe`
        "voiceless palatal plosive pulmonic egressive consonant"
    it "should be that: [c̥] \
       \is the representation of the \
       \voiceless palatal plosive pulmonic egressive consonant" $
      describeIPA "c̥"
        `shouldBe`
        "voiceless palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊] \
       \is the representation of the \
       \voiceless palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊"
        `shouldBe`
        "voiceless palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥] \
       \is the representation of the \
       \voiceless palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥"
        `shouldBe`
        "voiceless palatal plosive pulmonic egressive consonant"
  describe "voiceless labialized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cʷ] \
       \is the representation of the \
       \voiceless labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "cʷ"
        `shouldBe`
        "voiceless labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̊ʷ] \
       \is the representation of the \
       \voiceless labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̊ʷ"
        `shouldBe`
        "voiceless labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̥ʷ] \
       \is the representation of the \
       \voiceless labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̥ʷ"
        `shouldBe`
        "voiceless labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ʷ] \
       \is the representation of the \
       \voiceless labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ʷ"
        `shouldBe`
        "voiceless labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ʷ] \
       \is the representation of the \
       \voiceless labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ʷ"
        `shouldBe`
        "voiceless labialized palatal plosive pulmonic egressive consonant"
  describe "voiceless palatalized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cʲ] \
       \is the representation of the \
       \voiceless palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "cʲ"
        `shouldBe`
        "voiceless palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̊ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̊ʲ"
        `shouldBe`
        "voiceless palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̥ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̥ʲ"
        `shouldBe`
        "voiceless palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ʲ"
        `shouldBe`
        "voiceless palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ʲ"
        `shouldBe`
        "voiceless palatalized palatal plosive pulmonic egressive consonant"
  describe "voiceless velarized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cˠ] \
       \is the representation of the \
       \voiceless velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "cˠ"
        `shouldBe`
        "voiceless velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̊ˠ] \
       \is the representation of the \
       \voiceless velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̊ˠ"
        `shouldBe`
        "voiceless velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̥ˠ] \
       \is the representation of the \
       \voiceless velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̥ˠ"
        `shouldBe`
        "voiceless velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ˠ] \
       \is the representation of the \
       \voiceless velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ˠ"
        `shouldBe`
        "voiceless velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ˠ] \
       \is the representation of the \
       \voiceless velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ˠ"
        `shouldBe`
        "voiceless velarized palatal plosive pulmonic egressive consonant"
  describe "voiceless pharyngealized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "cˤ"
        `shouldBe`
        "voiceless pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̊ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̥ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized palatal plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cʰ] \
       \is the representation of the \
       \voiceless aspirated palatal plosive pulmonic egressive consonant" $
      describeIPA "cʰ"
        `shouldBe`
        "voiceless aspirated palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ʰ"
        `shouldBe`
        "voiceless aspirated palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ʰ"
        `shouldBe`
        "voiceless aspirated palatal plosive pulmonic egressive consonant"
  describe "voiceless aspirated labialized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "cʰʷ"
        `shouldBe`
        "voiceless aspirated labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized palatal plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatalized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "cʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized palatal plosive pulmonic egressive consonant"
  describe "voiceless aspirated velarized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "cʰˠ"
        `shouldBe`
        "voiceless aspirated velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized palatal plosive pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [cʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "cʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized palatal plosive pulmonic egressive consonant"
  describe "voiced palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟ] \
       \is the representation of the \
       \voiced palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ"
        `shouldBe`
        "voiced palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬] \
       \is the representation of the \
       \voiced palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬"
        `shouldBe`
        "voiced palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬] \
       \is the representation of the \
       \voiced palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬"
        `shouldBe`
        "voiced palatal plosive pulmonic egressive consonant"
  describe "voiced labialized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟʷ] \
       \is the representation of the \
       \voiced labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟʷ"
        `shouldBe`
        "voiced labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʷ] \
       \is the representation of the \
       \voiced labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʷ"
        `shouldBe`
        "voiced labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʷ] \
       \is the representation of the \
       \voiced labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʷ"
        `shouldBe`
        "voiced labialized palatal plosive pulmonic egressive consonant"
  describe "voiced palatalized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟʲ] \
       \is the representation of the \
       \voiced palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟʲ"
        `shouldBe`
        "voiced palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʲ] \
       \is the representation of the \
       \voiced palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʲ"
        `shouldBe`
        "voiced palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʲ] \
       \is the representation of the \
       \voiced palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʲ"
        `shouldBe`
        "voiced palatalized palatal plosive pulmonic egressive consonant"
  describe "voiced velarized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟˠ] \
       \is the representation of the \
       \voiced velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟˠ"
        `shouldBe`
        "voiced velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ˠ] \
       \is the representation of the \
       \voiced velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ˠ"
        `shouldBe`
        "voiced velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ˠ] \
       \is the representation of the \
       \voiced velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ˠ"
        `shouldBe`
        "voiced velarized palatal plosive pulmonic egressive consonant"
  describe "voiced pharyngealized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟˤ] \
       \is the representation of the \
       \voiced pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟˤ"
        `shouldBe`
        "voiced pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ˤ"
        `shouldBe`
        "voiced pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ˤ"
        `shouldBe`
        "voiced pharyngealized palatal plosive pulmonic egressive consonant"
  describe "voiced aspirated palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟʰ] \
       \is the representation of the \
       \voiced aspirated palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟʰ"
        `shouldBe`
        "voiced aspirated palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̬ʰ] \
       \is the representation of the \
       \voiced aspirated palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̬ʰ"
        `shouldBe`
        "voiced aspirated palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʰ] \
       \is the representation of the \
       \voiced aspirated palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʰ"
        `shouldBe`
        "voiced aspirated palatal plosive pulmonic egressive consonant"
  describe "voiced aspirated labialized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟʰʷ"
        `shouldBe`
        "voiced aspirated labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized palatal plosive pulmonic egressive consonant"
  describe "voiced aspirated palatalized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟʰʲ"
        `shouldBe`
        "voiced aspirated palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized palatal plosive pulmonic egressive consonant"
  describe "voiced aspirated velarized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟʰˠ"
        `shouldBe`
        "voiced aspirated velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized palatal plosive pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɟʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [ɟ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "ɟ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized palatal plosive pulmonic egressive consonant"
    it "should be that: [c̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized palatal plosive pulmonic egressive consonant" $
      describeIPA "c̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized palatal plosive pulmonic egressive consonant"
  describe "voiceless velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [k] \
       \is the representation of the \
       \voiceless velar plosive pulmonic egressive consonant" $
      describeIPA "k"
        `shouldBe`
        "voiceless velar plosive pulmonic egressive consonant"
    it "should be that: [k̊] \
       \is the representation of the \
       \voiceless velar plosive pulmonic egressive consonant" $
      describeIPA "k̊"
        `shouldBe`
        "voiceless velar plosive pulmonic egressive consonant"
    it "should be that: [k̥] \
       \is the representation of the \
       \voiceless velar plosive pulmonic egressive consonant" $
      describeIPA "k̥"
        `shouldBe`
        "voiceless velar plosive pulmonic egressive consonant"
    it "should be that: [g̊] \
       \is the representation of the \
       \voiceless velar plosive pulmonic egressive consonant" $
      describeIPA "g̊"
        `shouldBe`
        "voiceless velar plosive pulmonic egressive consonant"
    it "should be that: [g̥] \
       \is the representation of the \
       \voiceless velar plosive pulmonic egressive consonant" $
      describeIPA "g̥"
        `shouldBe`
        "voiceless velar plosive pulmonic egressive consonant"
  describe "voiceless labialized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kʷ] \
       \is the representation of the \
       \voiceless labialized velar plosive pulmonic egressive consonant" $
      describeIPA "kʷ"
        `shouldBe`
        "voiceless labialized velar plosive pulmonic egressive consonant"
    it "should be that: [k̊ʷ] \
       \is the representation of the \
       \voiceless labialized velar plosive pulmonic egressive consonant" $
      describeIPA "k̊ʷ"
        `shouldBe`
        "voiceless labialized velar plosive pulmonic egressive consonant"
    it "should be that: [k̥ʷ] \
       \is the representation of the \
       \voiceless labialized velar plosive pulmonic egressive consonant" $
      describeIPA "k̥ʷ"
        `shouldBe`
        "voiceless labialized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ʷ] \
       \is the representation of the \
       \voiceless labialized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ʷ"
        `shouldBe`
        "voiceless labialized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ʷ] \
       \is the representation of the \
       \voiceless labialized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ʷ"
        `shouldBe`
        "voiceless labialized velar plosive pulmonic egressive consonant"
  describe "voiceless palatalized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kʲ] \
       \is the representation of the \
       \voiceless palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "kʲ"
        `shouldBe`
        "voiceless palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [k̊ʲ] \
       \is the representation of the \
       \voiceless palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "k̊ʲ"
        `shouldBe`
        "voiceless palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [k̥ʲ] \
       \is the representation of the \
       \voiceless palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "k̥ʲ"
        `shouldBe`
        "voiceless palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ʲ] \
       \is the representation of the \
       \voiceless palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ʲ"
        `shouldBe`
        "voiceless palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ʲ] \
       \is the representation of the \
       \voiceless palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ʲ"
        `shouldBe`
        "voiceless palatalized velar plosive pulmonic egressive consonant"
  describe "voiceless velarized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kˠ] \
       \is the representation of the \
       \voiceless velarized velar plosive pulmonic egressive consonant" $
      describeIPA "kˠ"
        `shouldBe`
        "voiceless velarized velar plosive pulmonic egressive consonant"
    it "should be that: [k̊ˠ] \
       \is the representation of the \
       \voiceless velarized velar plosive pulmonic egressive consonant" $
      describeIPA "k̊ˠ"
        `shouldBe`
        "voiceless velarized velar plosive pulmonic egressive consonant"
    it "should be that: [k̥ˠ] \
       \is the representation of the \
       \voiceless velarized velar plosive pulmonic egressive consonant" $
      describeIPA "k̥ˠ"
        `shouldBe`
        "voiceless velarized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ˠ] \
       \is the representation of the \
       \voiceless velarized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ˠ"
        `shouldBe`
        "voiceless velarized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ˠ] \
       \is the representation of the \
       \voiceless velarized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ˠ"
        `shouldBe`
        "voiceless velarized velar plosive pulmonic egressive consonant"
  describe "voiceless pharyngealized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "kˤ"
        `shouldBe`
        "voiceless pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [k̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "k̊ˤ"
        `shouldBe`
        "voiceless pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [k̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "k̥ˤ"
        `shouldBe`
        "voiceless pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ˤ"
        `shouldBe`
        "voiceless pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ˤ"
        `shouldBe`
        "voiceless pharyngealized velar plosive pulmonic egressive consonant"
  describe "voiceless aspirated velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kʰ] \
       \is the representation of the \
       \voiceless aspirated velar plosive pulmonic egressive consonant" $
      describeIPA "kʰ"
        `shouldBe`
        "voiceless aspirated velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ʰ] \
       \is the representation of the \
       \voiceless aspirated velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ʰ"
        `shouldBe`
        "voiceless aspirated velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ʰ] \
       \is the representation of the \
       \voiceless aspirated velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ʰ"
        `shouldBe`
        "voiceless aspirated velar plosive pulmonic egressive consonant"
  describe "voiceless aspirated labialized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized velar plosive pulmonic egressive consonant" $
      describeIPA "kʰʷ"
        `shouldBe`
        "voiceless aspirated labialized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized velar plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatalized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "kʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized velar plosive pulmonic egressive consonant"
  describe "voiceless aspirated velarized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized velar plosive pulmonic egressive consonant" $
      describeIPA "kʰˠ"
        `shouldBe`
        "voiceless aspirated velarized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized velar plosive pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [kʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "kʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [g̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "g̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [g̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "g̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized velar plosive pulmonic egressive consonant"
  describe "voiced velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [g] \
       \is the representation of the \
       \voiced velar plosive pulmonic egressive consonant" $
      describeIPA "g"
        `shouldBe`
        "voiced velar plosive pulmonic egressive consonant"
    it "should be that: [k̬] \
       \is the representation of the \
       \voiced velar plosive pulmonic egressive consonant" $
      describeIPA "k̬"
        `shouldBe`
        "voiced velar plosive pulmonic egressive consonant"
    it "should be that: [k̬] \
       \is the representation of the \
       \voiced velar plosive pulmonic egressive consonant" $
      describeIPA "k̬"
        `shouldBe`
        "voiced velar plosive pulmonic egressive consonant"
  describe "voiced labialized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gʷ] \
       \is the representation of the \
       \voiced labialized velar plosive pulmonic egressive consonant" $
      describeIPA "gʷ"
        `shouldBe`
        "voiced labialized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʷ] \
       \is the representation of the \
       \voiced labialized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʷ"
        `shouldBe`
        "voiced labialized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʷ] \
       \is the representation of the \
       \voiced labialized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʷ"
        `shouldBe`
        "voiced labialized velar plosive pulmonic egressive consonant"
  describe "voiced palatalized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gʲ] \
       \is the representation of the \
       \voiced palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "gʲ"
        `shouldBe`
        "voiced palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʲ] \
       \is the representation of the \
       \voiced palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʲ"
        `shouldBe`
        "voiced palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʲ] \
       \is the representation of the \
       \voiced palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʲ"
        `shouldBe`
        "voiced palatalized velar plosive pulmonic egressive consonant"
  describe "voiced velarized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gˠ] \
       \is the representation of the \
       \voiced velarized velar plosive pulmonic egressive consonant" $
      describeIPA "gˠ"
        `shouldBe`
        "voiced velarized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ˠ] \
       \is the representation of the \
       \voiced velarized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ˠ"
        `shouldBe`
        "voiced velarized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ˠ] \
       \is the representation of the \
       \voiced velarized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ˠ"
        `shouldBe`
        "voiced velarized velar plosive pulmonic egressive consonant"
  describe "voiced pharyngealized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gˤ] \
       \is the representation of the \
       \voiced pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "gˤ"
        `shouldBe`
        "voiced pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ˤ"
        `shouldBe`
        "voiced pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ˤ"
        `shouldBe`
        "voiced pharyngealized velar plosive pulmonic egressive consonant"
  describe "voiced aspirated velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gʰ] \
       \is the representation of the \
       \voiced aspirated velar plosive pulmonic egressive consonant" $
      describeIPA "gʰ"
        `shouldBe`
        "voiced aspirated velar plosive pulmonic egressive consonant"
    it "should be that: [g̬ʰ] \
       \is the representation of the \
       \voiced aspirated velar plosive pulmonic egressive consonant" $
      describeIPA "g̬ʰ"
        `shouldBe`
        "voiced aspirated velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʰ] \
       \is the representation of the \
       \voiced aspirated velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʰ"
        `shouldBe`
        "voiced aspirated velar plosive pulmonic egressive consonant"
  describe "voiced aspirated labialized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized velar plosive pulmonic egressive consonant" $
      describeIPA "gʰʷ"
        `shouldBe`
        "voiced aspirated labialized velar plosive pulmonic egressive consonant"
    it "should be that: [g̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized velar plosive pulmonic egressive consonant" $
      describeIPA "g̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized velar plosive pulmonic egressive consonant"
  describe "voiced aspirated palatalized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "gʰʲ"
        `shouldBe`
        "voiced aspirated palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [g̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "g̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized velar plosive pulmonic egressive consonant"
  describe "voiced aspirated velarized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized velar plosive pulmonic egressive consonant" $
      describeIPA "gʰˠ"
        `shouldBe`
        "voiced aspirated velarized velar plosive pulmonic egressive consonant"
    it "should be that: [g̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized velar plosive pulmonic egressive consonant" $
      describeIPA "g̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized velar plosive pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized velar plosive pulmonic egressive consonant" $
    do
    it "should be that: [gʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "gʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [g̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "g̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized velar plosive pulmonic egressive consonant"
    it "should be that: [k̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized velar plosive pulmonic egressive consonant" $
      describeIPA "k̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized velar plosive pulmonic egressive consonant"
  describe "voiceless uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [q] \
       \is the representation of the \
       \voiceless uvular plosive pulmonic egressive consonant" $
      describeIPA "q"
        `shouldBe`
        "voiceless uvular plosive pulmonic egressive consonant"
    it "should be that: [q̊] \
       \is the representation of the \
       \voiceless uvular plosive pulmonic egressive consonant" $
      describeIPA "q̊"
        `shouldBe`
        "voiceless uvular plosive pulmonic egressive consonant"
    it "should be that: [q̥] \
       \is the representation of the \
       \voiceless uvular plosive pulmonic egressive consonant" $
      describeIPA "q̥"
        `shouldBe`
        "voiceless uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊] \
       \is the representation of the \
       \voiceless uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊"
        `shouldBe`
        "voiceless uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥] \
       \is the representation of the \
       \voiceless uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥"
        `shouldBe`
        "voiceless uvular plosive pulmonic egressive consonant"
  describe "voiceless labialized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qʷ] \
       \is the representation of the \
       \voiceless labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "qʷ"
        `shouldBe`
        "voiceless labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̊ʷ] \
       \is the representation of the \
       \voiceless labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̊ʷ"
        `shouldBe`
        "voiceless labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̥ʷ] \
       \is the representation of the \
       \voiceless labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̥ʷ"
        `shouldBe`
        "voiceless labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ʷ] \
       \is the representation of the \
       \voiceless labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ʷ"
        `shouldBe`
        "voiceless labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ʷ] \
       \is the representation of the \
       \voiceless labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ʷ"
        `shouldBe`
        "voiceless labialized uvular plosive pulmonic egressive consonant"
  describe "voiceless palatalized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qʲ] \
       \is the representation of the \
       \voiceless palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "qʲ"
        `shouldBe`
        "voiceless palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̊ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̊ʲ"
        `shouldBe`
        "voiceless palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̥ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̥ʲ"
        `shouldBe`
        "voiceless palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ʲ"
        `shouldBe`
        "voiceless palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ʲ"
        `shouldBe`
        "voiceless palatalized uvular plosive pulmonic egressive consonant"
  describe "voiceless velarized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qˠ] \
       \is the representation of the \
       \voiceless velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "qˠ"
        `shouldBe`
        "voiceless velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̊ˠ] \
       \is the representation of the \
       \voiceless velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̊ˠ"
        `shouldBe`
        "voiceless velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̥ˠ] \
       \is the representation of the \
       \voiceless velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̥ˠ"
        `shouldBe`
        "voiceless velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ˠ] \
       \is the representation of the \
       \voiceless velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ˠ"
        `shouldBe`
        "voiceless velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ˠ] \
       \is the representation of the \
       \voiceless velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ˠ"
        `shouldBe`
        "voiceless velarized uvular plosive pulmonic egressive consonant"
  describe "voiceless pharyngealized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "qˤ"
        `shouldBe`
        "voiceless pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̊ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̥ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized uvular plosive pulmonic egressive consonant"
  describe "voiceless aspirated uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qʰ] \
       \is the representation of the \
       \voiceless aspirated uvular plosive pulmonic egressive consonant" $
      describeIPA "qʰ"
        `shouldBe`
        "voiceless aspirated uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ʰ] \
       \is the representation of the \
       \voiceless aspirated uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ʰ"
        `shouldBe`
        "voiceless aspirated uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ʰ] \
       \is the representation of the \
       \voiceless aspirated uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ʰ"
        `shouldBe`
        "voiceless aspirated uvular plosive pulmonic egressive consonant"
  describe "voiceless aspirated labialized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "qʰʷ"
        `shouldBe`
        "voiceless aspirated labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ʰʷ"
        `shouldBe`
        "voiceless aspirated labialized uvular plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatalized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "qʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized uvular plosive pulmonic egressive consonant"
  describe "voiceless aspirated velarized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "qʰˠ"
        `shouldBe`
        "voiceless aspirated velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ʰˠ"
        `shouldBe`
        "voiceless aspirated velarized uvular plosive pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [qʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "qʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̥ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̥ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̊ʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̊ʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized uvular plosive pulmonic egressive consonant"
  describe "voiced uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢ] \
       \is the representation of the \
       \voiced uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ"
        `shouldBe`
        "voiced uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬] \
       \is the representation of the \
       \voiced uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬"
        `shouldBe`
        "voiced uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬] \
       \is the representation of the \
       \voiced uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬"
        `shouldBe`
        "voiced uvular plosive pulmonic egressive consonant"
  describe "voiced labialized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢʷ] \
       \is the representation of the \
       \voiced labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢʷ"
        `shouldBe`
        "voiced labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʷ] \
       \is the representation of the \
       \voiced labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʷ"
        `shouldBe`
        "voiced labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʷ] \
       \is the representation of the \
       \voiced labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʷ"
        `shouldBe`
        "voiced labialized uvular plosive pulmonic egressive consonant"
  describe "voiced palatalized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢʲ] \
       \is the representation of the \
       \voiced palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢʲ"
        `shouldBe`
        "voiced palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʲ] \
       \is the representation of the \
       \voiced palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʲ"
        `shouldBe`
        "voiced palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʲ] \
       \is the representation of the \
       \voiced palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʲ"
        `shouldBe`
        "voiced palatalized uvular plosive pulmonic egressive consonant"
  describe "voiced velarized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢˠ] \
       \is the representation of the \
       \voiced velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢˠ"
        `shouldBe`
        "voiced velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ˠ] \
       \is the representation of the \
       \voiced velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ˠ"
        `shouldBe`
        "voiced velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ˠ] \
       \is the representation of the \
       \voiced velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ˠ"
        `shouldBe`
        "voiced velarized uvular plosive pulmonic egressive consonant"
  describe "voiced pharyngealized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢˤ] \
       \is the representation of the \
       \voiced pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢˤ"
        `shouldBe`
        "voiced pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ˤ"
        `shouldBe`
        "voiced pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ˤ"
        `shouldBe`
        "voiced pharyngealized uvular plosive pulmonic egressive consonant"
  describe "voiced aspirated uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢʰ] \
       \is the representation of the \
       \voiced aspirated uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢʰ"
        `shouldBe`
        "voiced aspirated uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̬ʰ] \
       \is the representation of the \
       \voiced aspirated uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̬ʰ"
        `shouldBe`
        "voiced aspirated uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʰ] \
       \is the representation of the \
       \voiced aspirated uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʰ"
        `shouldBe`
        "voiced aspirated uvular plosive pulmonic egressive consonant"
  describe "voiced aspirated labialized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢʰʷ"
        `shouldBe`
        "voiced aspirated labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized uvular plosive pulmonic egressive consonant"
  describe "voiced aspirated palatalized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢʰʲ"
        `shouldBe`
        "voiced aspirated palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized uvular plosive pulmonic egressive consonant"
  describe "voiced aspirated velarized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢʰˠ"
        `shouldBe`
        "voiced aspirated velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized uvular plosive pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
    do
    it "should be that: [ɢʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [ɢ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "ɢ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized uvular plosive pulmonic egressive consonant"
    it "should be that: [q̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized uvular plosive pulmonic egressive consonant" $
      describeIPA "q̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized uvular plosive pulmonic egressive consonant"
  describe "voiceless glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ] \
       \is the representation of the \
       \voiceless glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ"
        `shouldBe`
        "voiceless glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̊] \
       \is the representation of the \
       \voiceless glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̊"
        `shouldBe`
        "voiceless glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̥] \
       \is the representation of the \
       \voiceless glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̥"
        `shouldBe`
        "voiceless glottal plosive pulmonic egressive consonant"
  describe "voiceless labialized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔʷ] \
       \is the representation of the \
       \voiceless labialized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔʷ"
        `shouldBe`
        "voiceless labialized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̊ʷ] \
       \is the representation of the \
       \voiceless labialized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̊ʷ"
        `shouldBe`
        "voiceless labialized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̥ʷ] \
       \is the representation of the \
       \voiceless labialized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̥ʷ"
        `shouldBe`
        "voiceless labialized glottal plosive pulmonic egressive consonant"
  describe "voiceless palatalized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔʲ] \
       \is the representation of the \
       \voiceless palatalized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔʲ"
        `shouldBe`
        "voiceless palatalized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̊ʲ] \
       \is the representation of the \
       \voiceless palatalized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̊ʲ"
        `shouldBe`
        "voiceless palatalized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̥ʲ] \
       \is the representation of the \
       \voiceless palatalized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̥ʲ"
        `shouldBe`
        "voiceless palatalized glottal plosive pulmonic egressive consonant"
  describe "voiceless velarized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔˠ] \
       \is the representation of the \
       \voiceless velarized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔˠ"
        `shouldBe`
        "voiceless velarized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̊ˠ] \
       \is the representation of the \
       \voiceless velarized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̊ˠ"
        `shouldBe`
        "voiceless velarized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̥ˠ] \
       \is the representation of the \
       \voiceless velarized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̥ˠ"
        `shouldBe`
        "voiceless velarized glottal plosive pulmonic egressive consonant"
  describe "voiceless pharyngealized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔˤ] \
       \is the representation of the \
       \voiceless pharyngealized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔˤ"
        `shouldBe`
        "voiceless pharyngealized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̊ˤ] \
       \is the representation of the \
       \voiceless pharyngealized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̊ˤ"
        `shouldBe`
        "voiceless pharyngealized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̥ˤ] \
       \is the representation of the \
       \voiceless pharyngealized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̥ˤ"
        `shouldBe`
        "voiceless pharyngealized glottal plosive pulmonic egressive consonant"
  describe "voiceless aspirated glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔʰ] \
       \is the representation of the \
       \voiceless aspirated glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔʰ"
        `shouldBe`
        "voiceless aspirated glottal plosive pulmonic egressive consonant"
  describe "voiceless aspirated labialized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔʰʷ] \
       \is the representation of the \
       \voiceless aspirated labialized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔʰʷ"
        `shouldBe`
        "voiceless aspirated labialized glottal plosive pulmonic egressive consonant"
  describe "voiceless aspirated palatalized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔʰʲ] \
       \is the representation of the \
       \voiceless aspirated palatalized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔʰʲ"
        `shouldBe`
        "voiceless aspirated palatalized glottal plosive pulmonic egressive consonant"
  describe "voiceless aspirated velarized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔʰˠ] \
       \is the representation of the \
       \voiceless aspirated velarized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔʰˠ"
        `shouldBe`
        "voiceless aspirated velarized glottal plosive pulmonic egressive consonant"
  describe "voiceless aspirated pharyngealized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔʰˤ] \
       \is the representation of the \
       \voiceless aspirated pharyngealized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔʰˤ"
        `shouldBe`
        "voiceless aspirated pharyngealized glottal plosive pulmonic egressive consonant"
  describe "voiced glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬] \
       \is the representation of the \
       \voiced glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬"
        `shouldBe`
        "voiced glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̬] \
       \is the representation of the \
       \voiced glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬"
        `shouldBe`
        "voiced glottal plosive pulmonic egressive consonant"
  describe "voiced labialized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ʷ] \
       \is the representation of the \
       \voiced labialized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʷ"
        `shouldBe`
        "voiced labialized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̬ʷ] \
       \is the representation of the \
       \voiced labialized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʷ"
        `shouldBe`
        "voiced labialized glottal plosive pulmonic egressive consonant"
  describe "voiced palatalized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ʲ] \
       \is the representation of the \
       \voiced palatalized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʲ"
        `shouldBe`
        "voiced palatalized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̬ʲ] \
       \is the representation of the \
       \voiced palatalized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʲ"
        `shouldBe`
        "voiced palatalized glottal plosive pulmonic egressive consonant"
  describe "voiced velarized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ˠ] \
       \is the representation of the \
       \voiced velarized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ˠ"
        `shouldBe`
        "voiced velarized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̬ˠ] \
       \is the representation of the \
       \voiced velarized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ˠ"
        `shouldBe`
        "voiced velarized glottal plosive pulmonic egressive consonant"
  describe "voiced pharyngealized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ˤ"
        `shouldBe`
        "voiced pharyngealized glottal plosive pulmonic egressive consonant"
    it "should be that: [ʔ̬ˤ] \
       \is the representation of the \
       \voiced pharyngealized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ˤ"
        `shouldBe`
        "voiced pharyngealized glottal plosive pulmonic egressive consonant"
  describe "voiced aspirated glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ʰ] \
       \is the representation of the \
       \voiced aspirated glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʰ"
        `shouldBe`
        "voiced aspirated glottal plosive pulmonic egressive consonant"
  describe "voiced aspirated labialized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ʰʷ] \
       \is the representation of the \
       \voiced aspirated labialized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʰʷ"
        `shouldBe`
        "voiced aspirated labialized glottal plosive pulmonic egressive consonant"
  describe "voiced aspirated palatalized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ʰʲ] \
       \is the representation of the \
       \voiced aspirated palatalized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʰʲ"
        `shouldBe`
        "voiced aspirated palatalized glottal plosive pulmonic egressive consonant"
  describe "voiced aspirated velarized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ʰˠ] \
       \is the representation of the \
       \voiced aspirated velarized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʰˠ"
        `shouldBe`
        "voiced aspirated velarized glottal plosive pulmonic egressive consonant"
  describe "voiced aspirated pharyngealized glottal plosive pulmonic egressive consonant" $
    do
    it "should be that: [ʔ̬ʰˤ] \
       \is the representation of the \
       \voiced aspirated pharyngealized glottal plosive pulmonic egressive consonant" $
      describeIPA "ʔ̬ʰˤ"
        `shouldBe`
        "voiced aspirated pharyngealized glottal plosive pulmonic egressive consonant"

