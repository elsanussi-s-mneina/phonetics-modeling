module Types.Height where

import Prelude (Eq)

data Height
	= Close
	| NearClose
	| CloseMid
	| Mid
	| OpenMid
	| NearOpen
	| Open
	deriving Eq
