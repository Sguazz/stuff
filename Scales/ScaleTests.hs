{-# LANGUAGE TemplateHaskell #-}

module ScaleTests where

import Data.List
import Test.QuickCheck
import Test.QuickCheck.All

import Scales hiding (main)

----------------------
-- QuickCheck setup --
----------------------

instance Arbitrary Scale where
    arbitrary = oneof $ map return [Major, Pentatonic, Blues]

instance Arbitrary Mode where
    arbitrary = oneof $ map return [Ionian ..]

instance Arbitrary Note where
    arbitrary = oneof $ map return [C ..]

------------------
-- Actual tests --
------------------

-- Scales and stuff

prop_modulate :: Scale -> Mode -> Key -> Mode -> Bool
prop_modulate s m1 k1 m2 = sortedScale s m2 k1 == sortedScale s m1 k2
  where k2 = findModulation m1 k1 m2

prop_transpose :: Scale -> Mode -> Key -> Bool
prop_transpose s m k = all (== sortedScale s m k) scales
  where scales = map (sortedScale' s) . tops $ relativeModes m k

-- Layout and stuff

prop_padding :: [String] -> Bool
prop_padding as = all ((== l) . length) $ padList as
  where l = maximum . map length $ as

-------------
-- Helpers --
-------------

sortedScale :: Scale -> Mode -> Key -> [Note]
sortedScale s m k = sort . tops $ scaleNotes s m k

sortedScale' :: Scale -> (Key, Mode) -> [Note]
sortedScale' s (k, m) = sortedScale s m k

-----------------------
-- A very wacky Main --
-----------------------

return []
runTests = $quickCheckAll

main = runTests
