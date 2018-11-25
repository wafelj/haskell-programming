module DList where

newtype DList a = DL { unDL :: [a] -> [a] }

instance Show a => Show (DList a) where
  show l = show $ toList l

empty :: DList a
empty = DL $ \_ -> []

singleton :: a -> DList a
singleton a = DL $ \_ -> [a]

toList :: DList a -> [a]
toList dl = unDL dl $ []

infixr `cons`
cons      :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL ((++ [x]) . unDL xs)

append :: DList a -> DList a -> DList a
append xs ys = DL $ (++ (toList ys)) . unDL xs
