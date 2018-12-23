notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

replaceThe :: String -> String
replaceThe = unwords . map replace . words
  where replace w =
          case notThe w of
            Nothing -> "a"
            Just w  -> w

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = count . words
  where count ("the":w:ws) = count (w:ws) +
          (if w !! 0 `elem` vowels
           then 1
           else 0)
        count (_:ws)       = count ws
        count []           = 0

vowels :: String
vowels = "aeiou"

countVowels :: String -> Integer
countVowels = toInteger . length . filter (`elem` vowels)

count :: String -> String -> Integer
count letters = toInteger . length . filter (`elem` letters)

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s
  | v > toInteger (length s) - v = Nothing
  | otherwise                    = Just (Word' s)
  where v = count vowels s
