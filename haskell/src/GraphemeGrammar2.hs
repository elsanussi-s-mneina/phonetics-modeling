-- | This module will replace some functions in the
-- GraphemeGrammar module. The problem with those functions,
-- is that there are much better ways to parse text.
-- Specifically, a better way is to use small parse functions.
-- This module will take that approach of parsing IPA text
-- by using very small functions, and building bigger functions
-- from those smaller ones.
-- The approach in GraphemeGrammar was too adhoc. Although it
-- worked it was not easy to extend or understand.
-- Once the functions in here can parse IPA text,
-- we will replace some of the functions in GraphemeGrammar,
-- with those in GraphemeGrammar2, eventually merging both.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module GraphemeGrammar2 where

import Relude((==), (<>), Char, Maybe(Just, Nothing), Text,
              elem, otherwise)
import qualified Data.Text as T

{- Context free grammar of IPA (incomplete, but good enough to start)

We want something like this:

BaseDigraph -> BaseCharacter | BaseCharacter TieCharacter BaseCharacter
TieCharacter ->  ͜  | ͡
BaseCharacter -> a | b | c | t | s
VoicingDiacritic -> ̥ | ̊ | ̯ |  ̆
Diacritic -> ʰ
SecondaryArticulationDiacritic ->  ʷ | ʲ | ˤ | ˠ
-}

baseCharacters :: [Char]
baseCharacters = ['a', 'b', 'c', 't', 's']


secondaryArticulationDiacritics :: [Char]
secondaryArticulationDiacritics = ['ʷ', 'ʲ', 'ˤ', 'ˠ']

-- | Uses one parser on the text,
--   then uses the next parser on the remaining
--   text from the first parse.
thenParser
  :: (Text -> Maybe (Text, Text))
  -> (Text -> Maybe (Text, Text))
  -> Text
  -> Maybe (Text, Text)
thenParser firstParser secondParser text =
  case firstParser text of
    Nothing -> Nothing
    Just (parsed, rest)
            -> case secondParser rest of
                 Nothing -> Nothing
                 Just (parsed2, rest2) -> Just (parsed <> parsed2, rest2)


-- | changes a parser by repeating it an indefinite number
--   of times.
--   So a parser that parses only "a", will parse "aaaaa".
--   A parser that parses only "@", will parse "@@@@", "@@@@@" and
--   so on.
manyParser
  :: (Text -> Maybe (Text, Text))
  -> Text
  -> Maybe (Text, Text)
manyParser subParser text =
  case subParser text of
    Nothing -> Nothing
    Just (parsed, rest)
            -> case manyParser subParser rest of
                  Nothing -> Just (parsed, rest)
                  Just (parsed2, rest2) -> Just (parsed <> parsed2, rest2)

-- | This implements the
-- rule expressed in (BNF) grammar as:
-- digraph -> baseCharacter tieBarCharacter baseCharacter
digraphParser
  :: Text
  -> Maybe (Text, Text)
digraphParser =
  baseCharacterParser `thenParser` tieBarParser `thenParser` baseCharacterParser



tieBarParser
  :: Text
  -> Maybe (Text, Text)
tieBarParser = singleCharParser ['͜', '͡']



phonemeParser
  :: Text
  -> Maybe (Text, Text)
phonemeParser text =
  case baseCharacterParser text of
    Nothing
      -> Nothing
    Just (parsed1, rest1)
      -> case secondaryArticulationDiacriticParser rest1 of
           Nothing
             -> Just (parsed1, rest1)
           Just (parsed2, rest2) -> Just (parsed1 <> parsed2, rest2)


singleCharParser
  :: [Char]
  -> Text
  -> Maybe (Text, Text)
singleCharParser charList text
  | T.length text == 0 = Nothing
  | (T.index text 0) `elem` charList = Just (T.take 1 text, T.drop 1 text)
  | otherwise = Nothing


secondaryArticulationDiacriticParser
  :: Text
  -> Maybe (Text, Text)
secondaryArticulationDiacriticParser =
  singleCharParser secondaryArticulationDiacritics


baseCharacterParser
  :: Text
  -> Maybe (Text, Text)
baseCharacterParser =
  singleCharParser baseCharacters
