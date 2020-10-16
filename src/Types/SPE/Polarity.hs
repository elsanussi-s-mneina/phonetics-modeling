module Types.SPE.Polarity where
import           Prelude  (Eq)


{-|
 Represents the '+' (plus) or '-' (minus)
 of a binary feature. e.g. [+sonorant],
 [-sonorant]
-}
data Polarity 
	= Plus | Minus
	deriving Eq
