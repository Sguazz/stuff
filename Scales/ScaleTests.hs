{-# LANGUAGE TemplateHaskell #-}

module ScaleTests where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.All

import Scales hiding (main)

----------------------
-- QuickCheck setup --
----------------------

instance Arbitrary Note where
    arbitrary = oneof $ map return [C ..]

instance Arbitrary Mode where
    arbitrary = oneof $ map return [Ionian ..]

------------------
-- Actual tests --
------------------

prop_modulate :: Mode -> Key -> Mode -> Bool
prop_modulate m1 k1 m2 = sortedMajor m2 k1 == sortedMajor m1 k2
  where k2 = findModulation m1 k1 m2

prop_transpose :: Mode -> Key -> Bool
prop_transpose m k = all (== sortedMajor m k) scales
  where scales = map sortedMajor' $ tops $ relativeModes m k

-------------
-- Helpers --
-------------

sortedScale :: Scale -> Mode -> Key -> [Note]
sortedScale s m k = sort . tops $ scaleNotes s m k

sortedMajor = sortedScale Major

sortedMajor' (k, m) = sortedMajor m k

-----------------------
-- A very wacky Main --
-----------------------

return []
runTests = $quickCheckAll

main = runTests
