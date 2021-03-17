-- This module represents a collection of phonemes or phonetes.
-- In linguistics, when comparing the sounds of different languages
-- linguists often refer to a language's phoneme inventory.
-- A phoneme inventory is a list of sounds in a language,
-- and the relations between them (especially equality).

-- Because we have not implemented the distinction between phonemes,
-- and phonetes, we call our type Phonet inventory instead of phoneme inventory,
-- although
-- this is not a word used by linguists.
module Types.PhonetInventory where
import Prelude ()
import Types.Phonet ( Phonet(..) )


newtype PhonetInventory = PhonetInventory [Phonet]
