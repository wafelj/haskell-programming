module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative

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


-- List
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a l) = f a `mappend` foldMap f l

instance Traversable List where
  traverse f = foldr consf $ pure Nil
    where consf a b = liftA2 Cons (f a) b

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized go
    where go 0 = return Nil
          go n = do
            xs <- go $ n - 1
            a  <- arbitrary
            return $ Cons a xs

instance (Eq a) => EqProp (List a) where (=-=) = eq


-- Three
data Three a b c =
  Three a b c
  deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c )
        => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-- Pair
data Pair a b =
  Pair a b
  deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance ( Arbitrary a
         , Arbitrary b )
        => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance (Eq a, Eq b) => EqProp (Pair a b) where (=-=) = eq

-- Big
data Big a b =
  Big a b b
  deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance Foldable (Big a) where
  foldMap f (Big a b1 b2) = f b1 `mappend` f b2

instance Traversable (Big a) where
  traverse f (Big a b1 b2) = Big a <$> f b1 <*> f b2

instance ( Arbitrary a
         , Arbitrary b )
        => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Big a b1 b2

instance (Eq a, Eq b) => EqProp (Big a b) where (=-=) = eq

-- S
data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
        => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary

instance ( Applicative n
         , Testable (n Property)
         , EqProp a )
        => EqProp (S n a) where
  (S x y) =-= (S p q) =
        (property $ (=-=) <$> x <*> p)
    .&. (y =-= q)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na `mappend` f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a


-- Tree
data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node t1 a t2) = Node (f <$> t1) (f a) (f <$> t2)

-- foldMap is a bit easier
-- and looks more natural,
-- but you can do foldr too
-- for extra credit.
instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t1 a t2) = foldMap f t1 `mappend` f a `mappend` foldMap f t2

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node t1 a t2) = Node <$> traverse f t1 <*> f a <*> traverse f t2

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = treeGen

treeGen :: Arbitrary a => Gen (Tree a)
treeGen = do
  t1 <- treeGen
  t2 <- treeGen
  a  <- arbitrary
  elements [ Leaf a
           , Empty
           , Node t1 a t2
           ]

instance (Eq a) => EqProp (Tree a) where (=-=) = eq


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

  let triggerList :: List (Int, Int, [Int])
      triggerList = undefined
  quickBatch (traversable triggerList)

  let triggerThree :: (Three Int Int) (Int, Int, [Int])
      triggerThree = undefined
  quickBatch (traversable triggerThree)

  let triggerPair :: (Pair Int) (Int, Int, [Int])
      triggerPair = undefined
  quickBatch (traversable triggerPair)

  let triggerBig :: (Big Int) (Int, Int, [Int])
      triggerBig = undefined
  quickBatch (traversable triggerBig)

  let triggerS :: (S []) (Int, Int, [Int])
      triggerS = undefined
  quickBatch (traversable triggerS)

  let triggerTree = undefined
  quickBatch $ traversable (triggerTree :: Tree (Int, Int, [Int]))
