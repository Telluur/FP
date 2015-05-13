{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import FPPrac.Trees
import Data.Char

--Ugly tree structure, but this allows us to pattern match against node colour only: (Tree Colour "Whatever").
data Tree = Tree Colour TreeNode
data Colour = Red | Black | Grey
	deriving (Eq)
data TreeNode = Node Tree Int Tree | Leaf

pp :: Tree -> RBTree
pp (Tree Red (Node l n r)) = RBNode NodeRed (show n) [pp l , pp r]
pp (Tree Black (Node l n r)) = RBNode NodeBlack (show n) [pp l , pp r]
pp (Tree Grey (Node l n r)) = RBNode NodeGrey (show n) [pp l , pp r]
pp (Tree Red (Leaf)) = RBNode NodeRed [] []
pp (Tree Black (Leaf)) = RBNode NodeBlack [] []
pp (Tree Grey (Leaf)) = RBNode NodeGrey [] []


insert :: Int -> Tree -> Tree
insert i (Tree c (Node l n r))
	| i < n = Tree c (Node (insert i l) n r)
	| i > n = Tree c (Node l n (insert i r))
	| otherwise = error $ "Found duplicate value when trying to insert: " ++ (show i)
insert i (Tree _ (Leaf)) = Tree Red (Node (leaf) i (leaf))
	where
	leaf = Tree Black (Leaf)

rootToBlack :: Tree -> Tree
rootToBlack (Tree Red node) = Tree Black node
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


balancedInsert :: Int -> Tree -> Tree
balancedInsert i t = rootToBlack $ balance $ insert i t
bi i t = balancedInsert i t


--Helper functions
isRed :: Tree -> Bool
isRed (Tree Red _) = True
isRed _ = False

--Test Trees
--showRBTreeList $ map pp [tt, tt1, tt2, tt3]
tt :: Tree
tt = Tree Black (Node (Tree Black (Node (Tree Red (Node (leaf) 3 (leaf))) 7 (Tree Red (Node (leaf) 10 (leaf))))) 15 (Tree Black (Node (leaf) 25 (leaf))))
 where leaf = Tree Black Leaf

tt1 :: Tree
tt1 = balancedInsert 9 tt

tt2 :: Tree
tt2 = balancedInsert 8 tt1

tt3 :: Tree
tt3 = balancedInsert 11 tt2
