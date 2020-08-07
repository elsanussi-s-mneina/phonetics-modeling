{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module PrimitiveParsersSpec where

import           Data.Maybe    (fromJust)
import           Test.Hspec    (Spec, describe, hspec, it, shouldBe)
import           Relude
import           PrimitiveParsers  ( singleCharParser
                                   , thenParser
                                   , manyParser
                                   , orParser
                                   )

import qualified Data.Text as T

graphemeGrammarSpec = do
  hspec singleCharParserSpec
  hspec thenParserSpec
  hspec manyParserSpec
  hspec orParserSpec

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

orParserSpec :: Spec
orParserSpec =
  describe "or-parser" $ do
    it "parses \"aaa\" successfully" $ do
      orParser (manyParser (singleCharParser ['f'])) (manyParser (singleCharParser ['a'])) "aaaf" `shouldBe` Just ("aaa", "f")
      orParser (singleCharParser ['f']) (manyParser $ singleCharParser ['a']) "ffffa" `shouldBe` Just ("f", "fffa")
    it "parse failure case" $ do
      orParser (manyParser $ singleCharParser ['a', 'b']) (manyParser $ singleCharParser ['k']) "ttttnnn" `shouldBe` Nothing
