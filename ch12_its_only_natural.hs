import Data.Maybe

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i == 0 = Just Zero
  | i < 0  = Nothing
  | otherwise = Just $ Succ $ fromJust $ integerToNat $ i - 1
