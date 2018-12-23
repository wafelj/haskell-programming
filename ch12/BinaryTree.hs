data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a))
       -> a
       -> BinaryTree b
unfold f a =
  case f a of
    Nothing -> Leaf
    Just (a1, b, a2) -> Node (unfold f a1) b (unfold f a2)

treeBuilder :: Integer -> BinaryTree Integer
treeBuilder n = unfold f 0
  where f i
          | i == n    = Nothing
          | otherwise = Just (i+1, i, i+1)
