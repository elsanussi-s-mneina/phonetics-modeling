module InternationalPhoneticAlphabet3 where


data Grapheme
  = SuperscriptLatinSmallLetterH 
  | SuperscriptLatinSmallLeterW
  | LatinSmallLetterB

data IPAFeature
  = Aspiration
  | Bilabial


-- we should use a Set, since duplicates
-- have no meaning. For now we will leave it as a list.

-- So a sequence of segments in IPA like
-- [bat] would be modelled as:
-- a list of sets, where the first set
-- models the segment [b], and the second set
-- models the segments [a], and the third set 
-- models the segment [t].
-- Diacritics like superscript h,
-- would take a set, and change its contents
-- so for example, a superscript h, would
-- take the last set in the sequence of segments
-- and add an aspiration feature (and remove
-- any features that conflict with that feature).

-- Note these features will not be SPE features.
-- They are just features so we will be able
-- to fully calculate from IPA graphemes to internal. 

aspirate :: [IPAFeature] -> [IPAFeature]
aspirate features = appendIPAFeature Aspiration features


asFunctionOnSegment :: Grapheme -> ([IPAFeature] -> [IPAFeature])
asFunctionOnSegment SuperscriptLatinSmallLetterH = aspirate


asFunctionOnSegmentSequence :: Grapheme -> ([[IPAFeature]] -> [[IPAFeature]])
asFunctionOnSegmentSequence LatinSmallLetterB = (++ [[Bilabial]]) -- adds