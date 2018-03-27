module Ch20_5_LibraryFunctions where

import Data.Monoid

--1
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . (foldMap Sum)

--2
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . (foldMap Product)

--3
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . (foldMap $ Any . (==x))

--4
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr min' Nothing
  where min' a Nothing  = Just a
        min' a (Just b) = Just $ min a b

--5
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr max' Nothing
  where max' a Nothing  = Just a
        max' a (Just b) = Just $ max a b

--6
null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

--7
length' :: (Foldable t) => t a -> Int
length' = getSum . (foldMap (\_ -> Sum 1))

--8
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

--9
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

--10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> f a `mappend` b) mempty
