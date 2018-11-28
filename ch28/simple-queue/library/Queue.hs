module Queue where

import Control.DeepSeq

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

instance NFData a => NFData (Queue a) where
  rnf (Queue enq deq) = rnf enq `seq` rnf deq

push :: a -> Queue a -> Queue a
push a (Queue enq deq) = Queue (a:enq) deq

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue []  [])    = Nothing
pop (Queue enq [])    = Just (a, Queue [] enq')
                          where (a:enq') = reverse enq
pop (Queue enq (a:deq)) = Just (a, Queue enq deq)
