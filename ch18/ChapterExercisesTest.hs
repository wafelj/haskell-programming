module ChapterExercisesTest where

import ChapterExercises
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


--3
instance Arbitrary a => Arbitrary (Identity a)  where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where (=-=) = eq


--4
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    return $ Cons a Nil

instance Eq a => EqProp (List a) where (=-=) = eq


test :: IO ()
test = do
  quickBatch $ monad (NopeDotJpg :: Nope (Int, Int, Int))
  quickBatch $ functor (PLeft ('a', 'b', 'c')
                            :: PhhhbbtttEither Int (Char, Char, Char))
  quickBatch $ applicative (PLeft ('a', 'b', 'c')
                            :: PhhhbbtttEither Int (Char, Char, Char))
  quickBatch $ monad (PLeft ('a', 'b', 'c')
                      :: PhhhbbtttEither Int (Char, Char, Char))
  quickBatch $ functor (Identity ('a', 'b', 'c'))
  quickBatch $ applicative (Identity ('a', 'b', 'c'))
  quickBatch $ monad (Identity ('a', 'b', 'c'))
  quickBatch $ functor (Cons ('a', 'b', 'c') Nil)
  quickBatch $ applicative (Cons ('a', 'b', 'c') Nil)
  quickBatch $ monad (Cons ('a', 'b', 'c') Nil)
  quickBatch $ functor (Cons ('a', 'b', 'c') Nil)
