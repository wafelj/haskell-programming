import Data.Char (isUpper, toLower)
import Data.List
import Data.Maybe (fromJust)

data Key = Key Digit String

keys :: [Key]
keys = [
  Key '1' "1",
  Key '2' "abc2",
  Key '3' "def3",
  Key '4' "ghi4",
  Key '5' "jkl5",
  Key '6' "mno6",
  Key '7' "pqrs7",
  Key '8' "tuv8",
  Key '9' "wxyz9",
  Key '0' " 0",
  Key '*' "^*",
  Key '#' ".,#"]

convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: [Key]
            -> Char
            -> [(Digit, Presses)]
reverseTaps keys c
  | isUpper c = [calcPresses keys caps,
                 calcPresses keys lower]
  | otherwise = [calcPresses keys c]
      where caps = '^'
            lower = toLower c

calcPresses :: [Key] -> Char -> (Digit, Presses)
calcPresses keys c = 
  case findKey keys c of
    (Key digit chars) ->
      (digit, (1+) $ fromJust $ elemIndex c chars)

findKey :: [Key] -> Char -> Key
findKey keys c =
  let cmp (Key _ chars) = c `elem` chars in
    fromJust $ find cmp keys

cellPhonesDead :: [Key]
               -> String
               -> [(Digit, Presses)]
cellPhonesDead keys = concat . map (reverseTaps keys)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

mostPopularLetter :: String -> (Char, Int)
mostPopularLetter s = (longestSubstr !! 0, length longestSubstr)
  where longestSubstr = (!! 0) . reverse . sortOn length . group . sort $ s

mostCostlyLetter :: String -> (Char, Presses)
mostCostlyLetter s =
  let tuple = mostPopularLetter s
      c     = fst tuple
      n     = snd tuple
  in (c, n * (fingerTaps . reverseTaps keys $ c))

coolestLtr :: [String] -> Char
coolestLtr = fst . mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord s = (reverse . sort . group . sort . words . intercalate " " $ s) !! 0 !! 0
