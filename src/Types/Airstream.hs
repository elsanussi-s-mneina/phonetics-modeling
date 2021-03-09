-- | This module defines airstream mechanisms.
module Types.Airstream where

import Prelude (Eq)

data Airstream
	= PulmonicEgressive
	| Click
	| Implosive
	deriving Eq
