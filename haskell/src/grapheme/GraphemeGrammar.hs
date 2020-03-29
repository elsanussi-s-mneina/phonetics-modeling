module GraphemeGrammar (isAscender, isDescender, isExponential, isDiacriticAbove, isDiacriticBelow) where

import Prelude (Bool(True, False), elem)

import InternationalPhoneticAlphabet (IPAText)

exponentials :: [IPAText]
exponentials = ["ʰ" , "ʷ" , "ʲ" , "ˠ" , "ˤ" , "ⁿ" , "ˡ"]

{-|
Whether an IPA character is written above the base line
and to the right of the previous character,
like how exponents of a power are written
in mathematical notation.
|-}
isExponential :: IPAText -> Bool
isExponential character = character `elem` exponentials
{-|
Whether a diacritic goes above
the character it is placed on.
|-}
isDiacriticAbove :: IPAText -> Bool
isDiacriticAbove "̊" = True
isDiacriticAbove  _  = False

{-|
Whether a diacritic goes below
the character which it is placed on.
|-}
isDiacriticBelow :: IPAText -> Bool
isDiacriticBelow "̥" = True
isDiacriticBelow  _  = False


{-|
Whether a character (but not a diacritic)
takes up space
below the imaginary horizontal line
on which it is written.

This could be useful later for determining
where to put diacritics so that
they are readable.
|-}
ascenders :: [IPAText]
ascenders =
  ["b", "t", "d", "k", "ʔ", "f", "θ", "ð", "ħ", "ʕ", "h", "ɦ", "ɬ", "l", "ʎ",
  "ʘ", "ɓ", "ǀ", "ɗ", "ǃ", "ǂ", "ɠ", "ʄ", "ǁ", "ʛ", "ɺ", "ʢ", "ʡ", "ɤ", "ʈ", "ɖ",
  "ɸ", "β", "ʃ", "ɮ", "ɭ", "ɧ"]


isAscender :: IPAText -> Bool
isAscender character = character `elem` ascenders

descenders :: [IPAText]
descenders =
  ["p", "ɟ", "g", "q", "ɱ", "ɽ", "ʒ", "ʂ", "ʐ", "ç", "ʝ", "ɣ", "χ", "ɻ", "j",
   "ɰ", "ɥ", "y", "ɳ", "ɲ", "ʈ", "ɖ", "ɸ", "β", "ʃ", "ɮ", "ɭ", "ɧ"]


{-|
Whether a character (but not a diacritic)
takes up space
below the imaginary horizontal line
on which it is written.

This could be useful later for determining
where to put diacritics so that
they are readable.
|-}
isDescender :: IPAText -> Bool
isDescender character = character `elem` descenders
