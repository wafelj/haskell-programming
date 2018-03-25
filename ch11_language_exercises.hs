import Data.Char

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

-- not finished
capitalizeParagraph :: String -> String
capitalizeParagraph = concat . map capitalizeWord . splitOn '.'

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d s = takeWhile (/= d) s : splitOn d (dropWhile (/= d) s)
