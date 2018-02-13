module Ch18_7_ChapterExercises_Test where

import Ch18_7_ChapterExercises
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


--1
instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq


--2
instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ PLeft a),
               (1, return $ PRight b)]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

test :: IO ()
test = do
  quickBatch $ monad (NopeDotJpg :: Nope (Int, Int, Int))
  quickBatch $ functor (PLeft ('a', 'b', 'c')
                            :: PhhhbbtttEither Int (Char, Char, Char))
  quickBatch $ applicative (PLeft ('a', 'b', 'c')
                            :: PhhhbbtttEither Int (Char, Char, Char))
  quickBatch $ monad (PLeft ('a', 'b', 'c')
                      :: PhhhbbtttEither Int (Char, Char, Char))
