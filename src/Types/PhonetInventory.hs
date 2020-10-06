module Types.PhonetInventory where
import Prelude ()
import Types.Phonet ( Phonet(..) )


newtype PhonetInventory = PhonetInventory [Phonet]
