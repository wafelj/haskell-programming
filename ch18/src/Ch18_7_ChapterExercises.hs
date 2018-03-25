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



--1
j :: Monad m => m (m a) -> m a
j = (>>= id)

--2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

--3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb

--4
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

--5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh []     _ = return []
meh (a:as) f = l2 (:) (f a) (meh as f)

--6
flipType :: Monad m => [m a] -> m [a]
flipType a = meh a id
