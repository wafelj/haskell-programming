module ParseInteger where

import Text.Trifecta
import Data.Char (digitToInt)
import Data.Foldable (foldl')

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

base10Integer :: Parser Integer
base10Integer = do
  digits <- some parseDigit
  return $ toInteger $ foldl' (\b d -> digitToInt d + (10 * b)) 0 digits

base10Integer' :: Parser Integer
base10Integer' = do
  m <- optional (char '-')
  case m of
    Just _  -> negate <$> base10Integer
    Nothing -> base10Integer
