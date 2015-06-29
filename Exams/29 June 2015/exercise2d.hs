module Exercise2d where

import Exercise2a
import Data.List

minstensMed :: Tree -> Tree
minstensMed tree = modifyTree tree (< med) 0
  where
    med = mediaan $ treeToList tree

modifyTree :: Tree -> (Int -> Bool) -> Int -> Tree
modifyTree (Leaf x) f z
  | f x = Leaf z
  | otherwise = Leaf x
modifyTree (Node l x r) f z
  | f x = Node l' z r'
  | otherwise = Node l' x r'
  where
    l' = modifyTree l f z
    r' = modifyTree r f z

treeToList :: Tree -> [Int]
treeToList (Leaf x) = [x]
treeToList (Node l x r) = treeToList l ++ [x] ++ treeToList r

mediaan :: [Int] -> Int
mediaan xs = mediaan' $ sort xs

mediaan' :: [Int] -> Int
mediaan' [] = error "Empty list"
mediaan' [x] = x
mediaan' [x,_] = x
mediaan' xs = mediaan' $ tail $ init xs
