{-# LANGUAGE NoImplicitPrelude #-}
module PrimitiveParsers where

import Relude(Text, Char, Maybe(Just, Nothing), (<>), (==), elem, otherwise)

import qualified Data.Text as T

singleCharParser
  :: [Char]
  -> Text
  -> Maybe (Text, Text)
singleCharParser charList text
  | T.length text == 0 = Nothing
  | (T.index text 0) `elem` charList = Just (T.take 1 text, T.drop 1 text)
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


-- | combines parsers by using one or the other.
orParser
  :: (Text -> Maybe (Text, Text))
  -> (Text -> Maybe (Text, Text))
  -> Text
  -> Maybe (Text, Text)
orParser firstParser secondParser text =
  case firstParser text of
    Nothing -> case secondParser text of
                      Nothing -> Nothing
                      Just (parsed, rest) -> Just (parsed, rest)
    Just (parsed, rest) -> Just (parsed, rest)

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
