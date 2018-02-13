module Ch18_7_ChapterExercises_Test where

import Ch18_7_ChapterExercises
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq

test :: IO ()
test = quickBatch $ monad (NopeDotJpg :: Nope (Int, Int, Int))
