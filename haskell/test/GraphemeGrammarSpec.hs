{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module GraphemeGrammarSpec where

import           Data.Maybe    (fromJust)
import           Test.Hspec    (Spec, describe, hspec, it, shouldBe)
import           Relude
import           GraphemeGrammar  ( secondaryArticulationDiacriticParser
                                  , phonemeParser
                                  , tieBarParser
                                  , digraphParser
                                  , splitIntoPhonemes
                                  )

import qualified Data.Text as T


graphemeGrammarSpec = do
  hspec secondaryArticulationDiacriticParserSpec
  hspec phonemeParserSpec
  hspec tieBarParserSpec
  hspec digraphParserSpec
  hspec splitIntoPhonemesSpec


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
    it "parses \\t\\" $ do
      phonemeParser "t" `shouldBe` Just ("t", "")
    it "parses \\d\\" $ do
      phonemeParser "d" `shouldBe` Just ("d", "")
    it "parses \\ʃ\\" $ do
      phonemeParser "ʃ" `shouldBe` Just ("ʃ", "")
    it "does not parse dollar sign" $ do
      phonemeParser "$" `shouldBe` Nothing
    it "does not parse an empty string" $ do
      phonemeParser "" `shouldBe` Nothing
    it "does parse digraph \\t͜ʃ\\" $ do
      phonemeParser "t͜ʃ" `shouldBe` Just ("t͜ʃ", "")
    it "does parse digraph \\t͡s\\" $ do
      phonemeParser "t͡s" `shouldBe` Just ("t͡s", "")
    it "does parse half of two phonemes \\t\\ \\s\\" $ do
      phonemeParser "ts" `shouldBe` Just ("t", "s")

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

splitIntoPhonemesSpec :: Spec
splitIntoPhonemesSpec =
  describe "split into phonemes" $ do
    it "splits \"td\" it into two phonemes." $ do
      splitIntoPhonemes "td" `shouldBe` ["t", "d"]
    it "splits \"tst͜s\" it into two phonemes." $ do
      splitIntoPhonemes "tst͜s" `shouldBe` ["t", "s", "t͜s"]
    it "splits \"ftst͜ss̬r̥dd͜ʒ\" into 8 phonemes." $ do
      splitIntoPhonemes "ftst͜ss̬r̥dd͜ʒ" `shouldBe` ["f", "t", "s", "t͜s", "s̬", "r̥", "d", "d͜ʒ"]
    it "splits \"fʰpʰ\" into 2 phonemes" $ do
      splitIntoPhonemes "fʰpʰ" `shouldBe` ["fʰ", "pʰ"]