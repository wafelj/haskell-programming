import Test.QuickCheck
import Test.QuickCheck.Function

--1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

--2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return $ Pair a1 a2

--3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a  b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) 
       => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

-- common
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                       f a
                    -> Fun a b
                    -> Fun b c
                    -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap g (fmap f x)) == (fmap (g . f) x)

type IntToInt = Fun Int Int

main :: IO ()
main = do
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck (functorCompose :: Identity Int -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorIdentity :: Pair Int -> Bool)
  quickCheck (functorCompose :: Pair Int -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorIdentity :: Two Int Int -> Bool)
  quickCheck (functorCompose :: Three Int Int Int -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorIdentity :: Three Int Int Int -> Bool)
  quickCheck (functorCompose :: Two Int Int -> IntToInt -> IntToInt -> Bool)
