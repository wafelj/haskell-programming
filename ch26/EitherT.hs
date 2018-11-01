newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m
      => Functor (EitherT e m) where
  fmap f (EitherT me) = EitherT $ (fmap . fmap) f me

instance Applicative m
      => Applicative (EitherT e m) where
  pure = EitherT . pure . pure

  (EitherT mef) <*> (EitherT mea) = 
    EitherT $ ((<*>) <$> mef) <*> mea

instance Monad m
      => Monad (EitherT e m) where
  return = pure

  (EitherT me) >>= f = 
    EitherT $ do
      ei <- me
      case ei of
        Left e  -> return $ Left e
        Right a -> runEitherT $ f a

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT me) = EitherT $ swapEither <$> me

swapEither :: Either e a -> Either a e
swapEither (Left e)  = Right e
swapEither (Right a) = Left a

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT fa fb (EitherT me) = do
  e <- me
  case e of 
    Left a  -> fa a
    Right b -> fb b
