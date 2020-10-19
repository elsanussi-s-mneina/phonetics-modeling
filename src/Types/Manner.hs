module Types.Manner where

import Prelude (Eq)

data Manner
	= Plosive
	| Nasal
	| Trill
	| TapOrFlap
	| Approximant
	| Fricative
	| Affricate
	| LateralFricative
	| LateralApproximant
	| LateralFlap
	| Lateral -- ^ We need this one for the lateral click.
	deriving Eq
