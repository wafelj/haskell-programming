import Data.Char (isSpace)

-- 1
myWords :: String -> [String]
myWords "" = []
myWords s = word : myWords rest
    where word = takeWhile (not.isSpace) s
          rest = (dropWhile isSpace) . (dropWhile (not.isSpace)) $ s

-- 2
myWords' :: String -> Char -> [String]
myWords' "" _ = []
myWords' s c  = word : myWords' rest c
    where word = takeWhile (/= c) s
          rest = (dropWhile (== c)) . (dropWhile (/= c)) $ s
