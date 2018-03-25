import Data.Char

-- 1
isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xxs@(x:xs) (y:ys)
  | x == y    = isSubseqOf xs ys
  | otherwise = isSubseqOf xxs ys

-- 2
capitalizeWords :: String
                -> [(String, String)]
capitalizeWords = map f . words
  where f w@(x:xs) = (w, (toUpper x) : xs)
