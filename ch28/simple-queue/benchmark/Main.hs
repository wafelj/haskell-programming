import Criterion.Main
import Data.Maybe
import Data.Sequence
import Queue

main :: IO ()
main = do
  let reps = 1000
  defaultMain [
      bench "list" (nf benchList reps)
    , bench "queue" (nf benchQueue reps)
    , bench "seq" (nf benchSequence reps)
    ]

benchList :: Int -> [Int]
benchList i = go i [1..50000]
        where go 0 q = q
              go n q = go (n-1) (init $ n:q)

benchQueue :: Int -> Queue Int
benchQueue i = go i (Queue [1..25000] [50000,49999..2501])
         where go 0 q = q
               go n q = go (n-1) $ snd (fromJust (pop $ push n q))

benchSequence :: Int -> Seq Int
benchSequence i = go i (fromList [1..50000])
            where go 0 s = s
                  go n s = go (n-1) (seqInit (n <| s))
                  seqInit s = case viewr s of
                                EmptyR  -> empty
                                s' :> _ -> s'
