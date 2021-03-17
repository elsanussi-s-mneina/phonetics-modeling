-- | This module defines the type of vocal fold vibration.
module Types.VocalFolds where

import Prelude (Eq)

data VocalFolds
	= Voiced
	| Voiceless
	| VoicedAspirated
	| VoicelessAspirated
	| CreakyVoiced
	deriving Eq
