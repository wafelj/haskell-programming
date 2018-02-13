module Ch18_7_ChapterExercises_Test where

import Ch18_7_ChapterExercises
import Test.QuickCheck
import Test.QuickCheck.Checkers

test :: IO ()
test = quickBatch (monad NopeDotJpg)
