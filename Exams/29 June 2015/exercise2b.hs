module Exercise2b where

import Exercise2a

count :: Tree -> Int -> Int
count (Leaf v) x
  | v == x = 1
  | otherwise = 0
count (Node l v r) x
  | v == x = 1 + subcount
  | otherwise = subcount
  where
    subcount = count l x + count r x

{-
count testTree 1 -> 4
count testTree 2 -> 0
count testTree 3 -> 3
count testTree 5 -> 3
count testTree 8 -> 2
count testTree 11 -> 1
-}
