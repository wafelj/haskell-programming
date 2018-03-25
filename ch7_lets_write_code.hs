-- 1
tensDigit :: Integral a => a -> a
tensDigit x = mod (x `div` 10) 10

hunsD :: Integral a => a -> a
hunsD x = mod (x `div` 100) 10

getDigit :: Integral a => a -> a -> a
getDigit i x = mod (x `div` 10^i) 10

-- 2
foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y b =
    case b of
      True  -> x
      False -> y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y b
    | b == True = x
    | otherwise = y

-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
