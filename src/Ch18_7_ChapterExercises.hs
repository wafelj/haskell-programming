module Ch18_7_ChapterExercises where

-- 1
data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  _ >>= _  = NopeDotJpg


--2
data PhhhbbtttEither b a =
    PLeft a
  | PRight b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where 
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a) = PLeft $ f a

instance Applicative (PhhhbbtttEither b) where
  pure a = PLeft a
  PRight f <*> _ = PRight f
  PLeft f <*> l = fmap f l

instance Monad (PhhhbbtttEither b) where
  return = pure
  PRight b >>= _ = PRight b
  PLeft a >>= f = f a

--3
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  return = pure
  Identity a >>= f = f a


--4
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  fs <*> as = flatten (fmap (\f -> fmap f as) fs)
  
flatten :: List (List a) -> List a
flatten (Cons a as) = concat' a (flatten as)
flatten _           = Nil

concat' :: List a -> List a -> List a
concat' Nil bs = bs
concat' as Nil = as
concat' (Cons a as) bs = 
  Cons a (concat' as bs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  l   >>= f = flatten $ fmap f l
