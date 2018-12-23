{-# LANGUAGE FlexibleInstances #-}

--1
data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor $ f b
  fmap _ Finance   = Finance

--2
newtype K a b =
  K a

instance Functor (K a) where
  fmap _ (K a) = K a

--3
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

-- should remind you of an
-- instance you've written before
instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip $ K (f b)

--4
data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

--11
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g)    = Read $ f . g
