import Data.Monoid

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a
      => Applicative (Constant a) where
  pure a = Constant mempty
  (<*>) (Constant a) (Constant a') = Constant (a `mappend` a')

x :: Constant String Int
x = pure 1
