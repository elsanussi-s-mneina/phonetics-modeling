module GraphemeGrammarSpec where

import           Data.Maybe    (fromJust)
import           Test.Hspec    (Spec, describe, hspec, it, shouldBe)
import           GraphemeGrammar  ( secondaryArticulationDiacriticParser
                                  , phonemeParser
                                  , tieBarParser
                                  , digraphParser
                                  , splitIntoPhonemes
                                  )
import Data.Text (Text, pack, unpack)

secondaryArticulationDiacriticParserString :: String -> Maybe (String, String)
secondaryArticulationDiacriticParserString = convertTextParserToStringParser secondaryArticulationDiacriticParser

phonemeParserString :: String -> Maybe (String, String)
phonemeParserString = convertTextParserToStringParser phonemeParser

tieBarParserString :: String -> Maybe (String, String)
tieBarParserString  = convertTextParserToStringParser tieBarParser

digraphParserString :: String -> Maybe (String, String)
digraphParserString  = convertTextParserToStringParser digraphParser

splitIntoPhonemesString :: String -> [String]
splitIntoPhonemesString  = map unpack . splitIntoPhonemes . pack

convertTextParserToStringParser :: (Text -> Maybe (Text, Text)) -> String -> Maybe (String, String)
convertTextParserToStringParser parser x =
  let
    intermediateResult = parser (pack x)
  in 
    case intermediateResult of 
       Nothing -> Nothing
       Just (t1, t2) -> Just (unpack t1, unpack t2)

graphemeGrammarSpec = do
  hspec secondaryArticulationDiacriticParserSpec
  hspec phonemeParserSpec
  hspec tieBarParserSpec
  hspec digraphParserSpec
  hspec splitIntoPhonemesSpec


secondaryArticulationDiacriticParserSpec :: Spec
secondaryArticulationDiacriticParserSpec =
  describe "secondary articulation parser" $ do
    it "should be that: parsing empty text fails" $
      secondaryArticulationDiacriticParserString "" `shouldBe` Nothing
    it "should be that: parsing \"abc\" text fails" $
      secondaryArticulationDiacriticParserString "abc" `shouldBe` Nothing
    it "should be that: parsing \"aʲ\" text fails" $
      secondaryArticulationDiacriticParserString "aʲ" `shouldBe` Nothing
    it "should be that: parsing \"ʲa\" text succeeds with \"a\" remaining" $
      secondaryArticulationDiacriticParserString "ʲa" `shouldBe` Just ("ʲ", "a")
    it "should be that: parsing \"ʷb\" text succeeds with \"b\" remaining" $
      secondaryArticulationDiacriticParserString "ʷa" `shouldBe` Just ("ʷ", "a")
    it "should be that: parsing \"ˠc\" text succeeds with \"c\" remaining" $
      secondaryArticulationDiacriticParserString "ˠc" `shouldBe` Just ("ˠ", "c")
    it "should be that: parsing \"ˤdefg\" text succeeds with \"defg\" remaining" $
      secondaryArticulationDiacriticParserString "ˤdefg" `shouldBe` Just ("ˤ", "defg")

phonemeParserSpec :: Spec
phonemeParserSpec =
  describe "phoneme parser" $ do
    it "should be that: parsing empty text fails" $
      (phonemeParserString "") `shouldBe` Nothing
    it "should be that: parsing \"a\" succeeds." $
      phonemeParserString "a" `shouldBe` Just ("a", "")
    it "should be that: parsing \"bc\" succeeds and consumes only one character" $
      phonemeParserString "bc" `shouldBe` Just ("b", "c")
    it "should be that: parsing a string containing only a superscript j fails" $
      (phonemeParserString "ʲ") `shouldBe` Nothing
    it "should be that: parsing a string containing only a \"t\" followed by superscript j succeeds" $
      phonemeParserString "tʲ" `shouldBe` Just("tʲ", "")
    it "parses \\t\\" $ do
      phonemeParserString "t" `shouldBe` Just ("t", "")
    it "parses \\d\\" $ do
      phonemeParserString "d" `shouldBe` Just ("d", "")
    it "parses \\ʃ\\" $ do
      phonemeParserString "ʃ" `shouldBe` Just ("ʃ", "")
    it "does not parse dollar sign" $ do
      phonemeParserString "$" `shouldBe` Nothing
    it "does not parse an empty string" $ do
      phonemeParserString "" `shouldBe` Nothing
    it "does parse digraph \\t͜ʃ\\" $ do
      phonemeParserString "t͜ʃ" `shouldBe` Just ("t͜ʃ", "")
    it "does parse digraph \\t͡s\\" $ do
      phonemeParserString "t͡s" `shouldBe` Just ("t͡s", "")
    it "does parse half of two phonemes \\t\\ \\s\\" $ do
      phonemeParserString "ts" `shouldBe` Just ("t", "s")
    it "does parse pharyngealized \\t\\ when given \\tˤs\\" $ do
      phonemeParserString "tˤs" `shouldBe` Just ("tˤ", "s")
    it "does parse pharyngealized \\t\\ when given \\tˤa\\" $ do
      phonemeParserString "tˤa" `shouldBe` Just ("tˤ", "a")
    it "does parse palatalized \\t\\ when given \\tʲs\\" $ do
      phonemeParserString "tʲs" `shouldBe` Just ("tʲ", "s")
    it "does parse palatalized \\t\\ when given \\tʲabcd\\" $ do
      phonemeParserString "tʲabcd" `shouldBe` Just ("tʲ", "abcd")
    it "does parse labialized \\b\\ when given \\bʷek\\" $ do
      phonemeParserString "bʷek" `shouldBe` Just ("bʷ", "ek")
    it "does parse labialized \\n\\ when given \\nʷabcd\\" $ do
      phonemeParserString "nʷabcd" `shouldBe` Just ("nʷ", "abcd")
    it "does parse velarized \\b\\ when given \\bˠek\\" $ do
      phonemeParserString "bˠek" `shouldBe` Just ("bˠ", "ek")
    it "does parse velarized \\n\\ when given \\nˠabcd\\" $ do
      phonemeParserString "nˠabcd" `shouldBe` Just ("nˠ", "abcd")
    it "does parse dental \\n\\ when given \\n̪abcd\\" $ do
      phonemeParserString "n̪abcd" `shouldBe` Just ("n̪", "abcd")
    it "does parse voiceless dental \\n\\ when given \\n̪̊abcd\\" $ do
      phonemeParserString "n̪̊uxcd" `shouldBe` Just ("n̪̊", "uxcd")


tieBarParserSpec :: Spec
tieBarParserSpec =
  describe "tie-bar parser" $ do
    it "parses the upper tie bar successfully"$ do
      tieBarParserString "͡" `shouldBe` Just ("͡", "")
    it "parses the lower tie bar successfully"$ do
      tieBarParserString "͜" `shouldBe` Just ("͜", "")
    it "parses only one tie bar at a time successfully"$ do
      tieBarParserString "͜͜" `shouldBe` Just ("͜", "͜")
      -- Sorry this is hard to see, but there are two tie bar characters in the input on the previous line.
    it "does not parse characters that are not tie-bars"$ do
      tieBarParserString "abcdef" `shouldBe` Nothing

digraphParserSpec :: Spec
digraphParserSpec =
  describe "digraph parser" $ do
    it "parses t͜s successfully once" $ do
      digraphParserString "t͜s" `shouldBe` Just ("t͜s", "")
    it "parses t͡s successfully once" $ do
      digraphParserString "t͡s" `shouldBe` Just ("t͡s", "")
    it "fails to parse ts because it has no digraph" $ do
      digraphParserString "ts" `shouldBe` Nothing

splitIntoPhonemesSpec :: Spec
splitIntoPhonemesSpec =
  describe "split into phonemes" $ do
    it "splits \"td\" it into two phonemes." $ do
      splitIntoPhonemesString "td" `shouldBe` ["t", "d"]
    it "splits \"tst͜s\" it into two phonemes." $ do
      splitIntoPhonemesString "tst͜s" `shouldBe` ["t", "s", "t͜s"]
    it "splits \"ftst͜ss̬r̥dd͜ʒ\" into 8 phonemes." $ do
      splitIntoPhonemesString "ftst͜ss̬r̥dd͜ʒ" `shouldBe` ["f", "t", "s", "t͜s", "s̬", "r̥", "d", "d͜ʒ"]
    it "splits \"fʰpʰ\" into 2 phonemes" $ do
      splitIntoPhonemesString "fʰpʰ" `shouldBe` ["fʰ", "pʰ"]