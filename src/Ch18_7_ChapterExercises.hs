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
