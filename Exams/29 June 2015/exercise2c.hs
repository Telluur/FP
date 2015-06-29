module Exercise2c where

import Exercise2a

zoekMax :: Tree -> Int
zoekMax (Leaf x) = x
zoekMax (Node l x r) = maximum [zoekMax l, x, zoekMax r]
