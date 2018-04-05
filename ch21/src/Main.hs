module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Identity
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where (=-=) = eq

-- Constant
newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

instance (Eq a, Eq b) => EqProp (Constant a b) where (=-=) = eq


-- Maybe
data Optional a =
    Nada
  | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [Nada, Yep a]

instance (Eq a) => EqProp (Optional a) where (=-=) = eq

main :: IO ()
main = do
  let triggerIdentity :: Identity (Int, Int, [Int])
      triggerIdentity = undefined
  quickBatch (traversable triggerIdentity)

  let triggerConstant :: (Constant Int) (Int, Int, [Int])
      triggerConstant = undefined
  quickBatch (traversable triggerConstant)

  let triggerOptional :: Optional (Int, Int, [Int])
      triggerOptional = undefined
  quickBatch (traversable triggerOptional)
