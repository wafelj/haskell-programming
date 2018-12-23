isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee b _ Nothing  = b

fromMaybe :: a -> Maybe a -> a
fromMaybe a ma = mayybee a id ma

fromJust' :: Maybe a -> a
fromJust' (Just a) = a
fromJust' Nothing  = error "Nothing"

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes' :: [Maybe a] -> [a]
catMaybes' = map fromJust' . filter isJust

flipMaybe :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe ms =
  if Nothing `elem` ms
  then Nothing
  else Just $ catMaybes' ms
