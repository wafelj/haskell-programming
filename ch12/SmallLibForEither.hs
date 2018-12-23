lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Left a) = (a:)
        f _        = id

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Right b) = (b:)
        f _        = id

partitionEithers' :: [Either a b]
                  -> ([a], [b])
partitionEithers' l = (lefts' l, rights' l)

eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' _ (Left a)  = Nothing

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' fl _ (Left a)  = fl a
either' _ fr (Right b) = fr b
