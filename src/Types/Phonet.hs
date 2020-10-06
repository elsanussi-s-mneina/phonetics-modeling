module Types.Phonet where

import Prelude (Eq)

import Types.Airstream ( Airstream(..))
import Types.Backness ( Backness(..) )
import Types.Height ( Height(..) )
import Types.Manner ( Manner(..) )
import Types.Place ( Place(..) )
import Types.Rounding ( Rounding(..) )
import Types.SecondaryArticulation ( SecondaryArticulation(..) )
import Types.VocalFolds ( VocalFolds(..) )
import Types.VowelLength ( VowelLength(..) )


-- | The data type Phonet, represents a linguistics
-- phoneme or phonete.
-- It can be a consonant, or a vowel.
-- A consonant is specified by
--    the configuration of the vocal folds,
--    the place of articulation,
--    the manner of articulation, and
--    an airstream mechanism.
-- A vowel is specified by
--    the height   (height of the tongue),
--    the backness (how far back in the mouth),
--    the rounding (rounding of lips), and
--    the configuration of the vocal folds.
data Phonet = Consonant !VocalFolds
                        !Place   -- ^ Place of articulation
                        !Manner  -- ^ Manner of articulation
                        !Airstream
                        !SecondaryArticulation
            | Vowel !Height
                    !Backness
                    !Rounding
                    !VocalFolds
                    !VowelLength
                    deriving Eq
