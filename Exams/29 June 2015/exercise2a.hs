module Exercise2a where

data Tree = Node Tree Int Tree | Leaf Int deriving Show

testTree :: Tree
testTree = Node
  (Node (Leaf 11) 5 (Node (Leaf 1) 3 (Leaf 8)))
  1
  (Node (Node (Leaf 1) 5 (Leaf 3)) 1 (Node (Leaf 8) 3 (Leaf 5)))

{-
        11
    5
            1
        3
            8
1
            1
        5
            3
    1
            8
        3
            5
-}
