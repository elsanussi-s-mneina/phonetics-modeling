module DescribeIPASpec.VowelNasalization where

import Test.Hspec (Spec, describe, it, shouldBe)
import qualified IPA
import Data.Text (pack, unpack)
describeIPA = unpack . IPA.describeIPA . pack

nasalVowelSpec :: Spec
nasalVowelSpec = do
  describe "vowel nasalization" $
    do
    describe "close vowels" $
      do
      it "should be that: [ĩ] \
         \is the representation of the \
         \voiced unrounded close front nasal vowel" $
        describeIPA "ĩ"
          `shouldBe`
          "voiced unrounded close front nasal vowel"
      it "should be that: [ỹ] \
         \is the representation of the \
         \voiced rounded close front nasal vowel" $
        describeIPA "ỹ"
          `shouldBe`
          "voiced rounded close front nasal vowel"
      it "should be that: [ɨ̃] \
         \is the representation of the \
         \voiced unrounded close central nasal vowel" $
        describeIPA "ɨ̃"
          `shouldBe`
          "voiced unrounded close central nasal vowel"
      it "should be that: [ʉ̃] \
         \is the representation of the \
         \voiced rounded close central nasal vowel" $
        describeIPA "ʉ̃"
          `shouldBe`
          "voiced rounded close central nasal vowel"
      it "should be that: [ɯ̃] \
         \is the representation of the \
         \voiced unrounded close back nasal vowel" $
        describeIPA "ɯ̃"
          `shouldBe`
          "voiced unrounded close back nasal vowel"
      it "should be that: [ũ] \
         \is the representation of the \
         \voiced rounded close back nasal vowel" $
        describeIPA "ũ"
          `shouldBe`
          "voiced rounded close back nasal vowel"
    describe "Near-close Vowels" $
      do
      it "should be that: [ɪ̃] \
         \is the representation of the \
         \voiced unrounded near-close front nasal vowel" $
        describeIPA "ɪ̃"
          `shouldBe`
          "voiced unrounded near-close front nasal vowel"
      it "should be that: [ʏ̃] \
         \is the representation of the \
         \voiced rounded near-close front nasal vowel" $
        describeIPA "ʏ̃"
          `shouldBe`
          "voiced rounded near-close front nasal vowel"
      it "should be that: [ʊ̃] \
         \is the representation of the \
         \voiced rounded near-close back nasal vowel" $
        describeIPA "ʊ̃"
          `shouldBe`
          "voiced rounded near-close back nasal vowel"
    describe "Close-mid Vowels" $
      do
      it "should be that: [ẽ] \
         \is the representation of the \
         \voiced unrounded close-mid front nasal vowel" $
        describeIPA "ẽ"
          `shouldBe`
          "voiced unrounded close-mid front nasal vowel"
      it "should be that: [ø̃] \
         \is the representation of the \
         \voiced rounded close-mid front nasal vowel" $
        describeIPA "ø̃"
          `shouldBe`
          "voiced rounded close-mid front nasal vowel"
      it "should be that: [ɘ̃] \
         \is the representation of the \
         \voiced unrounded close-mid central nasal vowel" $
        describeIPA "ɘ̃"
          `shouldBe`
          "voiced unrounded close-mid central nasal vowel"
      it "should be that: [ɵ̃] \
         \is the representation of the \
         \voiced rounded close-mid central nasal vowel" $
        describeIPA "ɵ̃"
          `shouldBe`
          "voiced rounded close-mid central nasal vowel"
      it "should be that: [ɤ̃] \
         \is the representation of the \
         \voiced unrounded close-mid back nasal vowel" $
        describeIPA "ɤ̃"
          `shouldBe`
          "voiced unrounded close-mid back nasal vowel"
      it "should be that: [õ] \
         \is the representation of the \
         \voiced rounded close-mid back nasal vowel" $
        describeIPA "õ"
          `shouldBe`
          "voiced rounded close-mid back nasal vowel"
    describe "Mid Vowels" $
      do
      it "should be that: [ə̃] \
         \is the representation of the \
         \voiced unrounded mid central nasal vowel" $
        describeIPA "ə̃"
          `shouldBe`
          "voiced unrounded mid central nasal vowel"
    describe "Open-mid Vowels" $
      do
      it "should be that: [ɛ̃] \
         \is the representation of the \
         \voiced unrounded open-mid front nasal vowel" $
        describeIPA "ɛ̃"
          `shouldBe`
          "voiced unrounded open-mid front nasal vowel"
      it "should be that: [œ̃] \
         \is the representation of the \
         \voiced rounded open-mid front nasal vowel" $
        describeIPA "œ̃"
          `shouldBe`
          "voiced rounded open-mid front nasal vowel"
      it "should be that: [ɜ̃] \
         \is the representation of the \
         \voiced unrounded open-mid central nasal vowel" $
        describeIPA "ɜ̃"
          `shouldBe`
          "voiced unrounded open-mid central nasal vowel"
      it "should be that: [ɞ̃] \
         \is the representation of the \
         \voiced rounded open-mid central nasal vowel" $
        describeIPA "ɞ̃"
          `shouldBe`
          "voiced rounded open-mid central nasal vowel"
      it "should be that: [ʌ̃] \
         \is the representation of the \
         \voiced unrounded open-mid back nasal vowel" $
        describeIPA "ʌ̃"
          `shouldBe`
          "voiced unrounded open-mid back nasal vowel"
      it "should be that: [ɔ̃] \
         \is the representation of the \
         \voiced rounded open-mid back nasal vowel" $
        describeIPA "ɔ̃"
          `shouldBe`
          "voiced rounded open-mid back nasal vowel"
    describe "Near-open Vowels" $
      do
      it "should be that: [æ̃] \
         \is the representation of the \
         \voiced unrounded near-open front nasal vowel" $
        describeIPA "æ̃"
          `shouldBe`
          "voiced unrounded near-open front nasal vowel"
      it "should be that: [ɐ̃] \
         \is the representation of the \
         \voiced unrounded near-open central nasal vowel" $
        describeIPA "ɐ̃"
          `shouldBe`
          "voiced unrounded near-open central nasal vowel"
    describe "Open Vowels" $
      do
      it "should be that: [ã] \
         \is the representation of the \
         \voiced unrounded open front nasal vowel" $
        describeIPA "ã"
          `shouldBe`
          "voiced unrounded open front nasal vowel"
      it "should be that: [ɶ̃] \
         \is the representation of the \
         \voiced rounded open front nasal vowel" $
        describeIPA "ɶ̃"
          `shouldBe`
          "voiced rounded open front nasal vowel"
      it "should be that: [ɑ̃] \
         \is the representation of the \
         \voiced unrounded open back nasal vowel" $
        describeIPA "ɑ̃"
          `shouldBe`
          "voiced unrounded open back nasal vowel"
      it "should be that: [ɒ̃] \
         \is the representation of the \
         \voiced rounded open back nasal vowel" $
        describeIPA "ɒ̃"
          `shouldBe`
          "voiced rounded open back nasal vowel"
