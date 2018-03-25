import Data.List

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr func False
  where func a = (f a ||)

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr func False
  where func a = ((a == e) ||)

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = any (== e)

myReverse :: [a] -> [a]
myReverse = foldl' (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr func []
  where func a = (f a :)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr func []
  where func a
          | f a = (a :)
          | otherwise = id

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr func []
  where func a = (f a ++)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id 

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl' func x xs
    where func a b
            | f a b == GT = a
            | otherwise   = b

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl' func x xs
    where func a b
            | f a b == LT = a
            | otherwise   = b
