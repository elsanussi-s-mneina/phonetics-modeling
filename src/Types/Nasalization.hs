-- | This module defines the types of nasalization.
-- A sound is nasalized if air escapes through the nose.
-- A sound is oral if air escapes through the mouth.
module Types.Nasalization where

import Prelude (Eq)

data Nasalization
	= Oral
	| Nasalized
	deriving Eq
