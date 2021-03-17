-- | This module defines the level of lip rounding.
module Types.Rounding where

import Prelude (Eq)

data Rounding
	= Rounded
	| Unrounded
	deriving Eq
