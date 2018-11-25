module Queue where

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push = undefined

pop :: Queue a -> Maybe (a, Queue a)
pop = undefined
