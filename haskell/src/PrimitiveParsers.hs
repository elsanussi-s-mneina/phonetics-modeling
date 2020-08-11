{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module PrimitiveParsers where

import Relude(Text, Char, Maybe(Just, Nothing), (<>), (&&), elem, not, otherwise)

import qualified Data.Text as T

singleCharParser
  :: [Char]
  -> Text
  -> Maybe (Text, Text)
singleCharParser charList text
  | not (T.null text) && (T.index text 0) `elem` charList = Just (T.take 1 text, T.drop 1 text)
  | otherwise = Nothing


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


-- | Combines parsers by using one or the other.
--   The first parser that succeeds is used.
orParser
  :: (Text -> Maybe (Text, Text))
  -> (Text -> Maybe (Text, Text))
  -> Text
  -> Maybe (Text, Text)
orParser firstParser secondParser text =
  case firstParser text of
    Nothing -> secondParser text
    Just result -> Just result

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

-- | changes a parser by making it never return Nothing,
--   that is it makes the parser optional.
optionalParser
  :: (Text -> Maybe (Text, Text))
  -> Text
  -> Maybe (Text, Text)
optionalParser subParser text =
  case subParser text of
    Nothing -> Just ("", text) -- Return text unconsumed.
    Just result -> Just result
