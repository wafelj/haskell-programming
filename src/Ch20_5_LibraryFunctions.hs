module Ch20_5_LibraryFunctions where

import Data.Monoid

--1
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . (foldMap Sum)
