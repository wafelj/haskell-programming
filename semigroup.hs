import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe (promote)

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
  a <> _ = a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityAssoc =
  Identity String -> Identity String -> Identity String -> Bool

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc =
  Two String String -> Two String String -> Two String String -> Bool

-- 4 and 5: same as 3

-- 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _                             = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return $ BoolConj b

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

-- 7: same as 6

-- 8
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd b) <> _ = Snd b
  _ <> b       = b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ Fst a)
              , (1, return $ Snd b) ]

type OrAssoc =
  Or String String -> Or String String -> Or String String -> Bool

-- 9
newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show _ = "function!!!"

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \n -> f n <> g n

instance (CoArbitrary a, Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- promote (\a -> coarbitrary a arbitrary)
    return $ Combine f

type CombineAssoc = 
     Combine Int (Sum Int)
  -> Combine Int (Sum Int)
  -> Combine Int (Sum Int)
  -> Int
  -> Bool

combineSemigroupAssoc :: (Eq b, Semigroup b)
   => Combine a b 
   -> Combine a b
   -> Combine a b
   -> a
   -> Bool
combineSemigroupAssoc a b c n =
  (unCombine (a <> (b <> c)) $ n) == (unCombine ((a <> b) <> c) $ n)

-- common
semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (combineSemigroupAssoc :: CombineAssoc)
