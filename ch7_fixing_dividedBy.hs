data DividedResult =
    Result (Integer, Integer)
  | DividedByZero
  deriving Show

dividedBy :: Integer -> Integer -> DividedResult
dividedBy n d
  | d == 0          = DividedByZero
  | n >= 0 && d > 0 = Result $ go n d 0 id
  | n <  0 && d < 0 = Result $ go n d 0 id
  | otherwise       = Result $ go n d 0 negate
  where go n d count op
          | n <= 0 && n > d = (count, n)
          | n >= 0 && n < d = (count, n)
          | otherwise =
            go (n - op d) d (count + op 1) op
