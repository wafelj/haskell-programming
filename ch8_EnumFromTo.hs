eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering
       -> Ordering
       -> [Ordering]
eftOrd = eft

eft :: (Enum a, Eq a, Ord a) => a -> a -> [a]
eft f t
  | t < f  = []
  | f == t = [f]
  | otherwise = f : eft (succ f) t

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft
