{-# LANGUAGE InstanceSigs #-}

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g)
    = Moi $ \s -> let (a, s1) = g s
                  in (f a, s1)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ (,) a

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi (\s -> (let (h, s1) = f s
                    (a, s2) = g s1
                in (h a, s2)))

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g =
    Moi (\s -> (let (a, s1) = f s
                in (runMoi $ g a) s1))


-- Chapter Exercises

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put s = Moi $ \_ -> ((), s)

exec :: Moi s a -> s -> s
exec (Moi sa) = snd . sa

eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
