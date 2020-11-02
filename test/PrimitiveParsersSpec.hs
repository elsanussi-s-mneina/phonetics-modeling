module PrimitiveParsersSpec where

import Prelude (($), IO, Maybe(Just, Nothing))
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import PrimitiveParsers (singleCharParser, thenParser, manyParser, orParser, optionalParser)

import Data.Text (pack)

runPrimitiveParsersSpecs :: IO ()
runPrimitiveParsersSpecs = do
	hspec singleCharParserSpec
	hspec thenParserSpec
	hspec manyParserSpec
	hspec orParserSpec
	hspec optionalParserSpec

singleCharParserSpec :: Spec
singleCharParserSpec =
	describe "single character parser" $ do
		it "should be that: single character parser of no characters fails to parse empty text" $
			singleCharParser [] (pack "") `shouldBe` Nothing
		it "should be that: single character parser of no characters fails to parse text of length 3" $
			singleCharParser [] (pack "abc") `shouldBe` Nothing
		it "should be that: single character parser of the character 'a' fails to parse the character \"b\"" $
			singleCharParser ['a'] (pack "b") `shouldBe` Nothing
		it "should be that: single character parser of the character 'a' does parse the character \"a\"" $
			singleCharParser ['a'] (pack "a") `shouldBe` Just(pack "a", pack "")
		it "should be that: single character parser of the character 'a' does parse the string containing two \"aa\"\
			\ characters and leaves one left" $
			singleCharParser ['a'] (pack "aa") `shouldBe` Just(pack "a", pack "a")
		it "should be that: single character parser of the character 'a' does parse the string containing \"ab\"\
			\ characters and leaves \"b\"" $
			singleCharParser ['a', 'b'] (pack "ab") `shouldBe` Just(pack "a", pack "b")
		it "should be that: single character parser of the character 'a' does parse the string containing \"abc\"\
			\ characters and leaves \"bc\"" $
			singleCharParser ['a', 'b'] (pack "abc") `shouldBe` Just(pack "a", pack "bc")
		it "should be that: single character parser of the character 'a' does parse the string containing \"abc\"\
			\ characters and leaves \"bc\"" $
			singleCharParser ['a', 'b'] (pack "cba") `shouldBe` Nothing
		it "should be that: single character parser of the non-ASCII character ʃ works" $
			do
			singleCharParser ['ʃ'] (pack "") `shouldBe` Nothing
			singleCharParser ['ʃ'] (pack "ʃ") `shouldBe` Just (pack "ʃ", pack "")
			singleCharParser ['ʃ'] (pack "ʃʃ") `shouldBe` Just (pack "ʃ", pack "ʃ")
			singleCharParser ['ʃ'] (pack "ʃʃʃ") `shouldBe` Just (pack "ʃ", pack "ʃʃ")
			singleCharParser ['ʃ'] (pack "ʃʃa") `shouldBe` Just (pack "ʃ", pack "ʃa")
			singleCharParser ['ʃ'] (pack "bbb") `shouldBe` Nothing
		it "should be that: single character parser of the non-ASCII character ʃ and the ASCII character q works" $
			do
			singleCharParser ['ʃ', 'q'] (pack "ʃ") `shouldBe` Just (pack "ʃ", pack "")
			singleCharParser ['ʃ', 'q'] (pack "q") `shouldBe` Just (pack "q", pack "")
			singleCharParser ['q', 'ʃ'] (pack "ʃʃ") `shouldBe` Just (pack "ʃ", pack "ʃ")
			singleCharParser ['ʃ', 'q'] (pack "qqʃ") `shouldBe` Just (pack "q", pack "qʃ")
			singleCharParser ['ʃ', 'q'] (pack "nnn") `shouldBe` Nothing



thenParserSpec :: Spec
thenParserSpec =
	describe "then-parser" $ do
		it "should be that: combining two single character parsers, parses two characters in same order" $ do
			thenParser (singleCharParser ['a']) (singleCharParser ['b']) (pack "abc") `shouldBe` Just(pack "ab", pack "c")
		it "should be that: combining two single character parsers, does not parse two characters in opposite order" $ do
			thenParser (singleCharParser ['a']) (singleCharParser ['b']) (pack "bac") `shouldBe` Nothing
		it "should be that: combining two single character parsers, parses two characters" $ do
			thenParser (singleCharParser ['m']) (singleCharParser ['m']) (pack "mmc") `shouldBe` Just(pack "mm", pack "c")

manyParserSpec :: Spec
manyParserSpec =
	describe "many parser" $ do
		it "should be that: a many-parser on one character fails on an empty string." $ do
			manyParser (singleCharParser ['a']) (pack "") `shouldBe` Nothing
		it "should be that: a many-parser on the space character fails on an empty string." $ do
			manyParser (singleCharParser [' ']) (pack "") `shouldBe` Nothing
		it "should be that: a many-parser on one character succeeds parsing when the character is the same." $ do
			manyParser (singleCharParser ['a']) (pack "a") `shouldBe` Just (pack "a", pack "")
			manyParser (singleCharParser ['b']) (pack "b") `shouldBe` Just (pack "b", pack "")
			manyParser (singleCharParser ['3']) (pack "3") `shouldBe` Just (pack "3", pack "")
		it "should be that: a many-parser on one character fails when parsing a string that does not start with that character." $ do
			manyParser (singleCharParser ['a']) (pack "baa") `shouldBe` Nothing
			manyParser (singleCharParser ['z']) (pack "az") `shouldBe` Nothing
		it "should be that: a many-parser on one characters succeeds on a string that starts with only that character" $ do
			manyParser (singleCharParser ['f']) (pack "fff") `shouldBe` Just (pack "fff", pack "")
			manyParser (singleCharParser ['f']) (pack "fffa") `shouldBe` Just (pack "fff", pack "a")
			manyParser (singleCharParser ['d']) (pack "ddrst") `shouldBe` Just (pack "dd", pack "rst")

orParserSpec :: Spec
orParserSpec =
	describe "or-parser" $ do
	it "parses \"aaaf\" successfully" $ do
		orParser (manyParser (singleCharParser ['f'])) (manyParser (singleCharParser ['a'])) (pack "aaaf") `shouldBe` Just (pack "aaa", pack "f")
	it "parses \"ffffa\" successfully" $ do
		orParser (singleCharParser ['f']) (manyParser $ singleCharParser ['a']) (pack "ffffa") `shouldBe` Just (pack "f", pack "fffa")
	it "fails to parse \"qu\" " $ do
		orParser (singleCharParser ['f']) (manyParser $ singleCharParser ['a']) (pack "qu") `shouldBe` Nothing
	it "does not ignore spaces " $ do
		orParser (singleCharParser ['f']) (manyParser $ singleCharParser ['a']) (pack " ffffa") `shouldBe` Nothing
		orParser (singleCharParser ['f']) (manyParser $ singleCharParser ['a']) (pack "   ffffa") `shouldBe` Nothing
	it "does not ignore tabs " $ do
		orParser (singleCharParser ['f']) (manyParser $ singleCharParser ['a']) (pack "\tffffa") `shouldBe` Nothing
		orParser (singleCharParser ['f']) (manyParser $ singleCharParser ['a']) (pack "\t\t\t\tffffa") `shouldBe` Nothing
	it "does not ignore new lines " $ do
		orParser (singleCharParser ['f']) (manyParser $ singleCharParser ['a']) (pack "\nffffa") `shouldBe` Nothing
		orParser (singleCharParser ['f']) (manyParser $ singleCharParser ['a']) (pack "\n\nffffa") `shouldBe` Nothing
	it "parse failure case" $ do
		orParser (manyParser $ singleCharParser ['a', 'b']) (manyParser $ singleCharParser ['k']) (pack "ttttnnn") `shouldBe` Nothing

optionalParserSpec :: Spec
optionalParserSpec =
	describe "optional parser" $ do
	it "parses the empty string without consuming input when given a single character parser" $ do
		optionalParser (singleCharParser ['a']) (pack "") `shouldBe` Just (pack "", pack "")
	it "does not ignore a space character" $ do
		optionalParser (singleCharParser ['b']) (pack " ") `shouldBe` Just (pack "", pack " ")
	it "does not ignore a tab character" $ do
		optionalParser (singleCharParser ['c']) (pack "\t") `shouldBe` Just (pack "", pack "\t")
	it "does not ignore a new line character" $ do
		optionalParser (singleCharParser ['d']) (pack "\n") `shouldBe` Just (pack "", pack "\n")
	it "parses \"aaaf\" successfully" $ do
		optionalParser (manyParser (singleCharParser ['a'])) (pack "aaaf") `shouldBe` Just (pack "aaa", pack "f")
	it "parses \"bbb\" successfully without consuming any input" $ do
		optionalParser (manyParser (singleCharParser ['a'])) (pack "bbb") `shouldBe` Just (pack "", pack "bbb")
	it "parses \"f\" successfully" $ do
		optionalParser (singleCharParser ['f']) (pack "f") `shouldBe` Just(pack "f", pack "")
	it "does parse \"d\" when expecting an \"f\", but does not consume \"d\"." $ do
		optionalParser (singleCharParser ['f']) (pack "d") `shouldBe` Just(pack "", pack "d")