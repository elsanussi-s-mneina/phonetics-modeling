-- | This module defines vowel lengths.
module Types.VowelLength where

import Prelude (Eq)

data VowelLength
	= NormalLength | Long | HalfLong | ExtraShort
	deriving Eq
