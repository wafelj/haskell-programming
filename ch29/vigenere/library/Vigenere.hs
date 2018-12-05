module Vigenere (encrypt, decrypt) where

import Data.Char

encrypt :: String -> String -> String
encrypt keyword message = 
  map encryptChar $ constructPairs message keyword

decrypt :: String -> String -> String
decrypt keyword message =
  map decryptChar $ constructPairs message keyword

constructPairs :: String -> String -> [(Char, Char)]
constructPairs message keyword = 
  zip message (concat . repeat $ keyword)

encryptChar :: (Char, Char) -> Char
encryptChar = substituteChar (+)

decryptChar :: (Char, Char) -> Char
decryptChar = substituteChar (-)

substituteChar :: (Int -> Int -> Int) -> (Char, Char) -> Char
substituteChar op (c, k) = 
  intToChar $ 
  (charToInt c `op` charToInt k) `mod` alphabetLength

alphabetLength :: Int
alphabetLength = 26

charToInt :: Char -> Int
charToInt c = mod (ord c) (ord firstChar)

intToChar :: Int -> Char
intToChar i = chr $ i + ord firstChar

firstChar :: Char
firstChar = 'A'
