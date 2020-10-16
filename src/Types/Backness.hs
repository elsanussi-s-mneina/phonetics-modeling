module Types.Backness where

import Prelude (Eq)

data Backness
	= Front
	| Central
	| Back
	deriving Eq
