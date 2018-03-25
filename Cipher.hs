module Cipher where

import Data.Char

data Direction = LeftDir | RightDir deriving Show

transform :: Direction -> Int -> String -> String
transform _ _ []     = []
transform d s (x:xs) = substitute d s x : transform d s xs

substitute :: Direction -> Int -> Char -> Char
substitute _ _ ' ' = ' '
substitute d s c   = chr . denorm . (flip mod 26) . (op d s) . norm . ord $ c
    where norm   = flip mod (ord 'A')
          denorm = (+ ord 'A')
          op RightDir = (+)
          op LeftDir = (-)

ceasar :: Int -> String -> String
ceasar = transform RightDir

unceasar :: Int -> String -> String
unceasar = transform LeftDir

vigenere :: String -> String -> String
vigenere ks xs = map f pairs
  where f (x, k) = substitute RightDir (getOffset k) x
        pairs = zip xs (concat . repeat $ ks)
        getOffset k = mod (ord k) (ord 'A')

main :: IO ()
main = do
  putStrLn "enter text to encode with ceasar: "
  s <- getLine
  putStrLn "enter offset"
  offset <- getLine
  putStrLn ("result: " ++ ceasar (read offset :: Int) s)
