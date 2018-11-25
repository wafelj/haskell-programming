import Criterion.Main
import Data.Maybe
import Queue

main :: IO ()
main = defaultMain [
    bench "list" (whnf benchList 1000)
  , bench "queue" (whnf benchQueue 1000)
  ]

benchList :: Int -> [Int]
benchList i = go i [1..5000]
  where go 0 q = q
        go n q = go (n-1) (init $ n:q)

benchQueue :: Int -> Queue Int
benchQueue i = go i (Queue [1..2500] [5000,4999..2501])
  where go 0 q = q
        go n q = go (n-1) $ snd (fromJust (pop $ push n q))
