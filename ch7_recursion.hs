sumNums :: (Eq a, Num a) => a -> a
sumNums 0 = 0
sumNums n = n + sumNums (n-1)

mult :: (Integral a) => a -> a -> a
mult _ 0 = 0
mult x y = x + mult x (y-1)
