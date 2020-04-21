{-# LANGUAGE UnicodeSyntax #-}

module GraphemeGrammar 
  ( isAscender, isDescender, isExponential
  , isDiacriticAbove, isDiacriticBelow
  , preventProhibitedCombination
  ) where

import Prelude (Bool(True, False))
import Prelude.Unicode ((∈), (⧺), (∧))

import InternationalPhoneticAlphabet (IPAText)

exponentials ∷ [IPAText]
exponentials = ["ʰ" , "ʷ" , "ʲ" , "ˠ" , "ˤ" , "ⁿ" , "ˡ"]

{-|
Whether an IPA character is written above the base line
and to the right of the previous character,
like how exponents of a power are written
in mathematical notation.
|-}
isExponential ∷ IPAText → Bool
isExponential character = character ∈ exponentials
{-|
Whether a diacritic goes above
the character it is placed on.
|-}
isDiacriticAbove ∷ IPAText → Bool
isDiacriticAbove "̊" = True
isDiacriticAbove  _  = False

{-|
Whether a diacritic goes below
the character which it is placed on.
|-}
isDiacriticBelow ∷ IPAText → Bool
isDiacriticBelow "̥" = True
isDiacriticBelow  _  = False

{-|
When given a diacritic that goes above,
replaces it with one that goes below,
and has the same meaning.
otherwise does nothing.
  |-}
lowerDiacritic ∷ IPAText → IPAText
lowerDiacritic "̊" = "̥"
lowerDiacritic x  = x


{-|
When given a diacritic that goes below,
replaces it with one that goes below, and
has the same meaning;
otherwise it does nothing. 
  |-}
raiseDiacritic ∷ IPAText → IPAText
raiseDiacritic "̥" = "̊"
raiseDiacritic x  = x


{-|
Whether a character (but not a diacritic)
takes up space
below the imaginary horizontal line
on which it is written.

This could be useful later for determining
where to put diacritics so that
they are readable.
|-}
ascenders ∷ [IPAText]
ascenders =
  ["b", "t", "d", "k", "ʔ", "f", "θ", "ð", "ħ", "ʕ", "h", "ɦ", "ɬ", "l", "ʎ",
  "ʘ", "ɓ", "ǀ", "ɗ", "ǃ", "ǂ", "ɠ", "ʄ", "ǁ", "ʛ", "ɺ", "ʢ", "ʡ", "ɤ", "ʈ", "ɖ",
  "ɸ", "β", "ʃ", "ɮ", "ɭ", "ɧ"]


isAscender ∷ IPAText → Bool
isAscender character = character ∈ ascenders

descenders ∷ [IPAText]
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
isDescender ∷ IPAText → Bool
isDescender character = character ∈ descenders


{-|
Prevent placement of diacrtic's below a full-width
character,
when doing so would likely make the result
difficult to read, whenever there is another
diacrtiic with the same meaning, but can go above.
And vice-versa (above - below).

Only support the voiceless diacritic so far.
  |-}
preventProhibitedCombination :: IPAText → IPAText
preventProhibitedCombination [] = []
preventProhibitedCombination [y] = [y]
preventProhibitedCombination noChange@(x:y:z) = 
  if isAscender [x] ∧ isDiacriticAbove [y]
  then [x] ⧺ lowerDiacritic [y] ⧺ z
  else if isDescender [x] ∧ isDiacriticBelow [y]
  then [x] ⧺ raiseDiacritic [y] ⧺ z
  else noChange