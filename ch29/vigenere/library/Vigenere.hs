module Vigenere (encrypt, decrypt) where

import Data.Char

data Direction = LeftDir | RightDir deriving Show

substitute :: Direction -> Int -> Char -> Char
substitute _ _ ' ' = ' '
substitute d s c   = chr . denorm . (flip mod 26) . (op d s) . norm . ord $ c
    where norm   = flip mod (ord 'A')
          denorm = (+ ord 'A')
          op RightDir = (+)
          op LeftDir = (-)

encrypt :: String -> String -> String
encrypt keyword message = map f pairs
  where f (x, k) = substitute RightDir (getOffset k) x
        pairs = zip message (concat . repeat $ keyword)
        getOffset k = mod (ord k) (ord 'A')

decrypt = undefined
