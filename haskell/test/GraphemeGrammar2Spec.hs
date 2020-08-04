{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module GraphemeGrammar2Spec where

import           Data.Maybe    (fromJust)
import           Test.Hspec    (Spec, describe, hspec, it, shouldBe)
import           Relude
import           GraphemeGrammar2

import qualified Data.Text as T


graphemeGrammar2Spec = do
  hspec singleCharParserSpec

singleCharParserSpec :: Spec
singleCharParserSpec =
  describe "single character parser" $ do
    it "should be that: single character parser of no characters fails to parse empty text" $
      isNothing (singleCharParser [] "") `shouldBe` True
    it "should be that: single character parser of no characters fails to parse text of length 3" $
      isNothing (singleCharParser [] "abc") `shouldBe` True
    it "should be that: single character parser of the character 'a' fails to parse the character \"b\"" $
      isNothing (singleCharParser ['a'] "b") `shouldBe` True
    it "should be that: single character parser of the character 'a' does parse the character \"a\"" $
      singleCharParser ['a'] "a" `shouldBe` Just("a", "")
    it "should be that: single character parser of the character 'a' does parse the string containing two \"aa\"\
       \ characters and leaves one left" $
      singleCharParser ['a'] "aa" `shouldBe` Just("a", "a")
    it "should be that: single character parser of the character 'a' does parse the string containing \"ab\"\
       \ characters and leaves \"b\"" $
      singleCharParser ['a', 'b'] "ab" `shouldBe` Just("a", "b")
    it "should be that: single character parser of the character 'a' does parse the string containing \"abc\"\
       \ characters and leaves \"bc\"" $
      singleCharParser ['a', 'b'] "abc" `shouldBe` Just("a", "bc")
    it "should be that: single character parser of the character 'a' does parse the string containing \"abc\"\
       \ characters and leaves \"bc\"" $
      isNothing (singleCharParser ['a', 'b'] "cba") `shouldBe` True


secondaryArticulationDiacriticParserSpec :: Spec
secondaryArticulationDiacriticParserSpec =
  let f = secondaryArticulationDiacriticParser
  in describe "secondary articulation parser" $ do
    it "should be that: parsing empty text fails" $
      isNothing (f "") `shouldBe` True
    it "should be that: parsing \"abc\" text fails" $
      isNothing (f "abc") `shouldBe` True
    it "should be that: parsing \"aʲ\" text fails" $
      isNothing (f "aʲ") `shouldBe` True
    it "should be that: parsing \"ʲa\" text succeeds with \"a\" remaining" $
      f "ʲa" `shouldBe` Just ("ʲ", "a")
    it "should be that: parsing \"ʷb\" text succeeds with \"b\" remaining" $
      f "ʷa" `shouldBe` Just ("ʷ", "a")
    it "should be that: parsing \"ˠc\" text succeeds with \"c\" remaining" $
      f "ˠc" `shouldBe` Just ("ˠ", "c")
    it "should be that: parsing \"ˤdefg\" text succeeds with \"defg\" remaining" $
      f "ˤa" `shouldBe` Just ("ˤ", "defg")

