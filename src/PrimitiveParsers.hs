module PrimitiveParsers where

import Prelude (Char, Maybe(..), (<>), elem, otherwise)

import Data.Text (Text, empty, drop, index, null, take)

-- | Given a list of characters,
--   and given a piece of text:
--   Consumes the first character in the
--   piece of text if that character is
--   on the list,
--   otherwise it fails to parse.
singleCharParser
  :: [Char]
  -> Text
  -> Maybe (Text, Text)
singleCharParser charList text
  | null text
    = Nothing
  | (index text 0) `elem` charList
    = Just (take 1 text, drop 1 text)
  | otherwise
    = Nothing

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
            -> if null parsed
                 then Nothing
                 else
                   case manyParser subParser rest of
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
    Nothing -> Just (empty, text) -- Return text unconsumed.
    Just result -> Just result
