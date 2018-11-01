newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance (Functor m)
      => Functor (StateT s m) where
    fmap f (StateT sma) = StateT $ (fmap . fmap) (\(a, s) -> (f a, s)) sma

instance (Monad m)
      => Applicative (StateT s m) where
  pure a = StateT $ (\s -> return (a, s))
  
  -- first try, wrong
  -- (StateT smtf) <*> (StateT smta) =
  --   StateT $ \s ->
  --     let mf = (fst <$> (smtf s))
  --         ma = (fst <$> (smta s))
  --         mb = mf <*> ma
  --     in (flip (,) s) <$> mb

  StateT g <*> StateT h = StateT $ \s -> do
    (f, s')  <- g s
    (a, s'') <- h s
    return (f a, s'')

instance (Monad m)
      => Monad (StateT s m) where
   return = pure

   (StateT sma) >>= f = StateT $ \s -> do
     (a, s') <- sma s
     runStateT (f a) $ s'
