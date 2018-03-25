stops  = "pbtdkg"
vowels = "aeiou"

gen :: String -> String -> [String]
gen stops vowels = [s1:v:[s2] | s1 <- stops, s2 <- stops, v <- vowels]

gen' :: String -> String -> [String]
gen' stops vowels = ['p':v:[s] | s <- stops, v <- vowels]

-- gen'' is similar to gen
