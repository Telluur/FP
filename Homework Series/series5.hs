module Series5 where

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import FPPrac.Trees
import Data.Char

--Ugly tree structure, but this allows for easy pattern matching agains colour. More bulky, less boilerplate.
data Tree = Tree Colour TreeNode
    deriving (Show)
data Colour = Red | Black | Grey
    deriving (Show)
data TreeNode = Node Tree Int Tree | Leaf
    deriving (Show)

pp :: Tree -> RBTree
pp (Tree Red (Node l n r)) = RBNode NodeRed (show n) [pp l , pp r]
pp (Tree Black (Node l n r)) = RBNode NodeBlack (show n) [pp l , pp r]
pp (Tree Grey (Node l n r)) = RBNode NodeGrey (show n) [pp l , pp r]
pp (Tree Red (Leaf)) = RBNode NodeRed [] []
pp (Tree Black (Leaf)) = RBNode NodeBlack [] []
pp (Tree Grey (Leaf)) = RBNode NodeGrey [] []

insert :: Tree -> Int -> Tree
insert (Tree c (Node l n r)) i
    | i < n = Tree c (Node (insert l i) n r)
    | i > n = Tree c (Node l n (insert r i))
    | otherwise = error $ "Found duplicate value when trying to insert: " ++ (show i)
insert (Tree _ (Leaf)) i = Tree Red (Node (leaf) i (leaf))
    where
    leaf = Tree Black (Leaf)

rootToBlack :: Tree -> Tree
rootToBlack (Tree Red node) = Tree Black node
rootToBlack (Tree Grey node) = Tree Black node
rootToBlack tree@(Tree Black node) = tree

colourFlip :: Tree -> Tree
colourFlip tree@(Tree Black (Node (Tree Red (Node (ll) il (lr))) i (Tree Red (Node (rl) ir (rr)))))
    | any (isRed) [ll, lr, rl, rr] = Tree Red (Node (Tree Black (Node (ll) il (lr))) i (Tree Black (Node (rl) ir (rr))))
    | otherwise = tree
colourFlip tree = tree

rebalance :: Tree -> Tree
--Welcome to pattern matching hell.
rebalance (Tree Black (Node (Tree Red (Node (Tree Red (Node lll ll llr)) l (lr))) x r)) = Tree Black (Node (Tree Red (Node (lll) ll (llr))) l (Tree Red (Node (lr) x (r))))
rebalance (Tree Black (Node (Tree Red (Node ll l (Tree Red (Node (lrl) lr (lrr))))) x r)) = Tree Black (Node (Tree Red (Node ll l lrl)) lr (Tree Red (Node lrr x r)))
rebalance (Tree Black (Node l x (Tree Red (Node rl r (Tree Red (Node rrl rr rrr)))))) = Tree Black (Node (Tree Red (Node l x rl)) r (Tree Red (Node rrl rr rrr)))
rebalance (Tree Black (Node l x (Tree Red (Node (Tree Red (Node rll rl rlr)) r rr)))) = Tree Black (Node (Tree Red (Node l x rll)) rl (Tree Red (Node rlr r rr)))
rebalance tree = tree --No pattern match found.

balance :: Tree -> Tree
balance (Tree c (Node l x r)) = rebalance $ colourFlip $ Tree c (Node (balance l) x (balance r))
balance t = t --Stops recursive balance call at leaves.

balancedInsert :: Tree -> Int -> Tree
balancedInsert t i = rootToBlack $ balance $ insert t i

leftmostValue :: Tree -> Int
leftmostValue (Tree c (Node (Tree _ (Leaf)) n _)) = n
leftmostValue (Tree c (Node l _ _)) = leftmostValue l
leftmostValue (Tree _ (Leaf)) = error "I Dun Goofed?"

removeLeftmostNode :: Tree -> Tree
removeLeftmostNode (Tree Red (Node (Tree _ (Leaf)) _ r)) = r
removeLeftmostNode (Tree Black (Node (Tree _ (Leaf)) _ r@(Tree Red _))) = r
removeLeftmostNode (Tree Black (Node (Tree _ (Leaf)) x (Tree _ (Leaf)))) = Tree Grey (Leaf)
removeLeftmostNode (Tree c (Node l@(Tree _ (Node _ _ _)) x r)) = Tree c (Node (removeLeftmostNode l) x r)

greyColourFlip :: Tree -> Tree
--Now screening in this theater: Pattern Matchting Hell Pt2.
--Case a and a mirror
greyColourFlip (Tree Black (Node (Tree Grey g) p (Tree Black (Node (Tree Black l) s (Tree Black r))))) = Tree Grey (Node (Tree Black g) p (Tree Red (Node (Tree Black l) s (Tree Black r))))
greyColourFlip (Tree Black (Node (Tree Black (Node (Tree Black l) s (Tree Black r))) p (Tree Grey g))) = Tree Grey (Node (Tree Red (Node (Tree Black l) s (Tree Black r))) p (Tree Black g))
--Case b and b mirror
greyColourFlip (Tree cp (Node (Tree Grey g) p (Tree Black (Node (Tree Red (Node (Tree Black a) l (Tree Black b))) s r)))) = Tree cp (Node (Tree Black (Node (Tree Black g) p (Tree Black a))) l (Tree Black (Node (Tree Black b) s r)))
greyColourFlip (Tree cp (Node (Tree Black (Node (Tree Red (Node (Tree Black a) l (Tree Black b))) s r)) p (Tree Grey g))) = Tree cp (Node (Tree Black (Node (Tree Black b) s r)) l (Tree Black (Node (Tree Black g) p (Tree Black a))))
--Case c and c mirror
greyColourFlip (Tree Red (Node (Tree Grey g) p (Tree Black (Node (Tree Black l) s (Tree cr r))))) = Tree Black (Node (Tree Red (Node (Tree Black g) p (Tree Black l))) s (Tree cr r))
greyColourFlip (Tree Red (Node (Tree Black (Node (Tree Black l) s (Tree cr r))) p (Tree Grey g))) = Tree Black (Node (Tree cr r) s (Tree Red (Node (Tree Black g) p (Tree Black l))))
--Case d and d mirror
greyColourFlip (Tree Black (Node (Tree Grey g) p (Tree Black (Node (Tree Black l) s (Tree Red r))))) = Tree Black (Node (Tree Black (Node (Tree Black g) p (Tree Black l))) s (Tree Black r))
greyColourFlip (Tree Black (Node (Tree Black (Node (Tree Black l) s (Tree Red r))) p (Tree Grey g))) = Tree Black (Node (Tree Black r) s (Tree Black (Node (Tree Black g) p (Tree Black l))))
--Case e and e mirror
greyColourFlip (Tree Black (Node (Tree Grey g) p (Tree Red (Node (Tree Black l) s (Tree Black r))))) = Tree Black (Node (greyColourFlip(Tree Red (Node (Tree Grey g) p (Tree Black l)))) s (Tree Black r))
greyColourFlip (Tree Black (Node (Tree Red (Node (Tree Black l) s (Tree Black r))) p (Tree Grey g))) = Tree Black (Node (Tree Black r) s (greyColourFlip(Tree Red (Node (Tree Grey g) p (Tree Black l)))))
greyColourFlip (Tree Red (Node (Tree Grey (Leaf)) x (Tree Black (Leaf)))) = Tree Red (Node (Tree Black (Leaf)) x (Tree Black (Leaf)))  --Extra Case in case the nodes beneath an e case are leaves. (We won't be able to fix the grey leaf with cases b or c)
greyColourFlip t = t
gcf t = greyColourFlip t

greyRebalance :: Tree -> Tree
greyRebalance t@(Tree _ (Leaf)) = t
greyRebalance (Tree c (Node l x r)) = greyColourFlip $ Tree c (Node (greyRebalance l) x (greyRebalance r))

delete :: Tree -> Int -> Tree
delete tree i = rootToBlack $ delete' tree i

delete' :: Tree -> Int -> Tree
delete' (Tree Red (Node (Tree Black Leaf) x (Tree Black Leaf))) i
    | i == x = Tree Black (Leaf)
    | otherwise = error $ "Value " ++ (show i) ++ " not found."
delete' (Tree Black (Node (Tree Black Leaf) x (Tree Black Leaf))) i
    | i == x = Tree Grey Leaf
    | otherwise = error $ "Value " ++ (show i) ++ " not found."
delete' (Tree Black (Node (Tree Red l) x (Tree Black (Leaf)))) i
    | i < x = Tree Black (Node (delete' (Tree Red l) i) x (Tree Black (Leaf)))
    | i == x = Tree Black l
    | otherwise = error $ "Value " ++ (show i) ++ " not found."
delete' (Tree cx (Node l x r)) i
    | i < x = greyRebalance $ Tree cx (Node (delete' l i) x r)
    | i > x = greyRebalance $ Tree cx (Node l x (delete' r i))
    | i == x = greyRebalance $ Tree cx (Node l (leftmostValue r) (removeLeftmostNode r))

--Helper functions
isRed :: Tree -> Bool
isRed (Tree Red _) = True
isRed _ = False

balancedListInsert :: Tree -> [Int] -> Tree
balancedListInsert tree list = foldl (balancedInsert) tree list

createTree :: [Int] -> Tree
createTree list = balancedListInsert (Tree Black (Leaf)) list

deleteList :: Tree -> [Int] -> Tree
deleteList tree list = foldl (delete) tree list
