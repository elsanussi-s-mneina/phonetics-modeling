{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module GraphemeGrammar2Spec where

import           Data.Maybe    (fromJust)
import           Test.Hspec    (Spec, describe, hspec, it, shouldBe)
import           Relude
import           GraphemeGrammar2 ( singleCharParser
                                  , secondaryArticulationDiacriticParser
                                  , phonemeParser
                                  , thenParser
                                  , manyParser
                                  , tieBarParser
                                  , digraphParser)

import qualified Data.Text as T


graphemeGrammar2Spec = do
  hspec singleCharParserSpec
  hspec secondaryArticulationDiacriticParserSpec
  hspec phonemeParserSpec
  hspec thenParserSpec
  hspec manyParserSpec
  hspec tieBarParserSpec
  hspec digraphParserSpec

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
      f "ˤdefg" `shouldBe` Just ("ˤ", "defg")

phonemeParserSpec :: Spec
phonemeParserSpec =
  let f = phonemeParser
  in describe "phoneme parser" $ do
    it "should be that: parsing empty text fails" $
      isNothing (f "") `shouldBe` True
    it "should be that: parsing \"a\" succeeds." $
      f "a" `shouldBe` Just ("a", "")
    it "should be that: parsing \"bc\" succeeds and consumes only one character" $
      f "bc" `shouldBe` Just ("b", "c")
    it "should be that: parsing a string containing only a superscript j fails" $
      isNothing (f "ʲ") `shouldBe` True
    it "should be that: parsing a string containing only a \"t\" followed by superscript j succeeds" $
      f "tʲ" `shouldBe` Just("tʲ", "")

thenParserSpec :: Spec
thenParserSpec =
  describe "then-parser" $ do
    it "should be that: combining two single character parsers, parses two characters in same order" $ do
      thenParser (singleCharParser ['a']) (singleCharParser ['b']) "abc" `shouldBe` Just("ab", "c")
    it "should be that: combining two single character parsers, does not parse two characters in opposite order" $ do
      thenParser (singleCharParser ['a']) (singleCharParser ['b']) "abc" `shouldBe` Just("ab", "c")
    it "should be that: combining two single character parsers, parses two characters" $ do
      thenParser (singleCharParser ['a']) (singleCharParser ['b']) "bac" `shouldBe` Nothing

manyParserSpec :: Spec
manyParserSpec =
  describe "many parser" $ do
    it "should be that: a many-parser on one character succeeds parsing when the character is the same." $ do
      manyParser (singleCharParser ['a']) "a" `shouldBe` Just ("a", "")
      manyParser (singleCharParser ['b']) "b" `shouldBe` Just ("b", "")
      manyParser (singleCharParser ['3']) "3" `shouldBe` Just ("3", "")
    it "should be that: a many-parser on one character fails when parsing a string that does not start with that character." $ do
      manyParser (singleCharParser ['a']) "baa" `shouldBe` Nothing
      manyParser (singleCharParser ['z']) "az" `shouldBe` Nothing
    it "should be that: a many-parser on one characters succeeds on a string that contains only that character" $ do
      manyParser (singleCharParser ['f']) "fff" `shouldBe` Just ("fff", "")
      manyParser (singleCharParser ['f']) "fffa" `shouldBe` Just ("fff", "a")
      manyParser (singleCharParser ['d']) "ddrst" `shouldBe` Just ("dd", "rst")

tieBarParserSpec :: Spec
tieBarParserSpec =
  describe "tie-bar parser" $ do
    it "parses the upper tie bar successfully"$ do
      tieBarParser "͡" `shouldBe` Just ("͡", "")
    it "parses the lower tie bar successfully"$ do
      tieBarParser "͜" `shouldBe` Just ("͜", "")
    it "parses only one tie bar at a time successfully"$ do
      tieBarParser "͜͜" `shouldBe` Just ("͜", "͜")
      -- Sorry this is hard to see, but there are two tie bar characters in the input.
    it "does not parse characters that are not tie-bars"$ do
      tieBarParser "abcdef" `shouldBe` Nothing

digraphParserSpec :: Spec
digraphParserSpec =
  describe "digraph parser" $ do
    it "parses t͜s successfully once" $ do
      digraphParser "t͜s" `shouldBe` Just ("t͜s", "")
    it "parses t͡s successfully once" $ do
      digraphParser "t͡s" `shouldBe` Just ("t͡s", "")
    it "fails to parse ts because it has no digraph" $ do
      digraphParser "ts" `shouldBe` Nothing

