data List a = 
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) as = append (f <$> as) (fs <*> as)

append :: List a -> List a -> List a 
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys
