{-# LANGUAGE UnicodeSyntax #-}
-- Base Unicode Symbols package lacked 
-- a single character replacement for the <> operator,
-- so I added it in this module. This module closely
-- follows the template of the source code inside
-- Base Unicode Symbols package.
-- I made sure that the operator precedence
-- of <> operator and its replacement are the same.


module MyLocal_Data_Semigroup_Unicode ((◇))
  where

import Prelude ()
import Data.Semigroup ( Semigroup, (<>))

-------------------------------------------------------------------------------
-- Fixities
-------------------------------------------------------------------------------

infixr 6 ◇


-------------------------------------------------------------------------------
-- Symbols
-------------------------------------------------------------------------------

{-|
(&#x25C7;) = '<>'
U+25C7, WHITE DIAMOND
-}
(◇) ∷ Semigroup α ⇒ α → α → α
(◇) = (<>)
{-# INLINE (◇) #-}
