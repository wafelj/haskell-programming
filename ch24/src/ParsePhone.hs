module ParsePhone where

import Text.Trifecta
import Control.Applicative
import Control.Monad (replicateM)

-- aka area code
type Operator = String
type LineNumber = String

data PhoneNumber =
  PhoneNumber Operator LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  string "+386(0)" <|> string "+386" <|> string "0"
  operator <- replicateM 2 $ token digit
  lineNum  <- replicateM 6 $ token digit
  eof
  return $ PhoneNumber ('0' : operator) lineNum
