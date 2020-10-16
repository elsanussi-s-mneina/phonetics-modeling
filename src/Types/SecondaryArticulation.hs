module Types.SecondaryArticulation where

import Prelude (Eq)

data SecondaryArticulation
	= Normal | Labialized | Palatalized | Velarized | Pharyngealized
	deriving Eq
