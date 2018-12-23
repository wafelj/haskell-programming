--1
data Constant a b =
  Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

--2
data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

--3
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

--4
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ _ b) = f b

--5
data Four' a b = Four' a b b b

instance Foldable (Four' a)  where
  foldMap f (Four' _ _ _ b) = f b
