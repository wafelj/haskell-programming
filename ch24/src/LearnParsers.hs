module LearnParsers where

import Text.Trifecta
import Control.Monad (foldM)

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

string123 :: Parser String
string123 = string "1" >> string "12" >> string "123" >> stop

charString :: String -> Parser Char
charString s = foldM (\a b -> char b) 'a' s

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testParseString :: Parser String -> IO ()
testParseString p =
  print $ parseString p mempty "112123"

pNL s =
  putStrLn ('\n' : s)

main = do
  pNL "stop: "
  testParse stop
  pNL "one: "
  testParse one
  pNL "one': "
  testParse one'
  pNL "oneTwo: "
  testParse oneTwo
  pNL "oneTwo': "
  testParse oneTwo'
  pNL "string123': "
  testParseString string123
  pNL "charString: "
  testParse $ charString "123"
