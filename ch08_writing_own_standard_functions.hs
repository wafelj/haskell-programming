myOr :: [Bool] -> Bool
myOr [] = False
myOr (True:_)   = True
myOr (False:xs) = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
    | f x       = True
    | otherwise = myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs)
    | e == x    = True
    | otherwise = myElem e xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = any (== e)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b] 
squishMap f xs = squish (map f xs)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = myExtremeBy GT

myExtremeBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a 
myExtremeBy _   _ (x:[]) = x
myExtremeBy ord f (x:xs)
  | f x y == ord = x
  | otherwise   = y
  where y = myExtremeBy ord f xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myExtremeBy LT

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
