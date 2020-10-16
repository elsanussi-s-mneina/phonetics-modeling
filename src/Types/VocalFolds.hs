module Types.VocalFolds where

import Prelude (Eq)

data VocalFolds
	= Voiced
	| Voiceless
	| VoicedAspirated
	| VoicelessAspirated
	| CreakyVoiced
	deriving Eq
