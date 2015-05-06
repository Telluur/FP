{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import FPPrac.Trees
import Data.Char
import Data.List

--1
--a
data Tree1a = Leaf1a Int
	| Node1a Int Tree1a Tree1a
	deriving(Show)

tree1a = Node1a 1 (Node1a 2 (Node1a 4 (Leaf1a 8) (Leaf1a 9)) (Node1a 5 (Leaf1a 10) (Leaf1a 11))) (Node1a 3 (Node1a 6 (Leaf1a 12) (Leaf1a 13)) (Node1a 7 (Leaf1a 14) (Leaf1a 15)))

pp1a :: Tree1a -> RoseTree
pp1a (Leaf1a n) = RoseNode (show n) []
pp1a (Node1a n l r) = RoseNode (show n) $ [(pp1a l)] ++ [(pp1a r)]

--b
data Tree1b = Leaf1b (Int, Int)
	| Node1b (Int, Int) Tree1b Tree1b
	deriving(Show)

tree1b = Node1b (1,2) (Node1b (3,4) (Leaf1b (7,8)) (Leaf1b (9,10))) (Node1b (5,6) (Leaf1b (11,12)) (Leaf1b (13,14)))

pp1b :: Tree1b -> RoseTree
pp1b (Leaf1b n) = RoseNode (show n) []
pp1b (Node1b n l r) = RoseNode (show n) $ [(pp1b l)] ++ [(pp1b r)]

--c
data Tree1c = Leaf1c Int
	| Node1c Tree1c Tree1c
	deriving(Show)

tree1c = Node1c (Node1c (Leaf1c 1) (Leaf1c 2)) (Node1c (Leaf1c 3) (Leaf1c 4))

pp1c :: Tree1c -> RoseTree
pp1c (Leaf1c n) = RoseNode (show n) []
pp1c (Node1c l r) = RoseNode [] $ [(pp1c l)] ++ [(pp1c r)]

--d
data Tree1d = Leaf1d (Int, Int)
	| Node1d [Tree1d]
	deriving(Show)

tree1d = Node1d [Node1d [Leaf1d (1,2)], Node1d [Leaf1d (3,4), Leaf1d (5,6)], Node1d [Leaf1d (7,8), Leaf1d (9,10), Leaf1d(11,12)]]
	
pp1d :: Tree1d -> RoseTree
pp1d (Leaf1d n) = RoseNode (show n) []
pp1d (Node1d list) = RoseNode [] $ map pp1d list

--2
--a
treeAdd :: Int -> Tree1a -> Tree1a
treeAdd x (Leaf1a n) = Leaf1a (n+x)
treeAdd x (Node1a n l r) = Node1a (n+x) (treeAdd x l) (treeAdd x r)

--b
treeSquare :: Tree1a -> Tree1a
treeSquare (Leaf1a n) = Leaf1a (n^2)
treeSquare (Node1a n l r) = Node1a (n^2) (treeSquare l) (treeSquare r)

--c
mapTree :: (Int -> Int) -> Tree1a -> Tree1a
mapTree f (Leaf1a n) = Leaf1a $ f n
mapTree f (Node1a n l r) = Node1a (f n) (mapTree f l) (mapTree f r)

mapTreeAdd :: Int -> Tree1a -> Tree1a
mapTreeAdd x tree = mapTree (+x) tree

mapTreeSquare :: Tree1a -> Tree1a
mapTreeSquare tree = mapTree (^2) tree

--d
{-
Tree1a kent geen 2-tuples?
Implementatie voor (Tree1b -> Tree1a) waar de waardes van de tuples bij elkaar worden opgeteld.
-}
--showRoseTreeList [(pp1b tree1b), (pp1a $ addNode tree1b)]
addNode :: Tree1b -> Tree1a
addNode (Leaf1b (a,b)) = Leaf1a $ a+b
addNode (Node1b (a,b) l r) = Node1a (a+b) (addNode l) (addNode r)

--e
--showRoseTreeList [(pp1b tree1b), (pp1a $ mapTree2eAdd tree1b), (pp1a $ mapTree2eMult tree1b)]
mapTree2e :: ((Int,Int) -> Int) -> Tree1b -> Tree1a
mapTree2e f (Leaf1b tuple) = Leaf1a $ f tuple
mapTree2e f (Node1b tuple l r) = Node1a (f tuple) (mapTree2e f l) (mapTree2e f r)

mapTree2eMult :: Tree1b -> Tree1a
mapTree2eMult tree = mapTree2e (\(a,b) -> a*b) tree

mapTree2eAdd :: Tree1b -> Tree1a
mapTree2eAdd tree = mapTree2e (\(a,b) -> a+b) tree

--3
--a
--showRoseTreeList [(pp1a tree1a), (pp1a $ binMirror tree1a), (pp1a $ binMirror $ binMirror tree1a)]
binMirror :: Tree1a -> Tree1a
binMirror (Leaf1a n) = Leaf1a n
binMirror (Node1a n l r) = Node1a n (binMirror r) (binMirror l)

--b
--showRoseTreeList [(pp1b tree1b), (pp1b $ binMirrorTuples tree1b), (pp1b $ binMirrorTuples $ binMirrorTuples tree1b)]
binMirrorTuples :: Tree1b -> Tree1b
binMirrorTuples (Leaf1b (a,b)) = Leaf1b (b,a)
binMirrorTuples (Node1b (a,b) l r) = Node1b (b,a) (binMirrorTuples r) (binMirrorTuples l)

--4
{-
Tree1c has by definition only numbers at the leaves. The exercise says the opposite. Implemented a tree type Tree4 to accomodate.
-}
data Tree4 = Leaf4
	| Node4 Int Tree4 Tree4
	deriving(Show)

tree4 = Node4 8 (Node4 3 (Node4 1 (Leaf4) (Leaf4)) (Node4 6 (Node4 4 (Leaf4) (Leaf4)) (Node4 7 (Leaf4) (Leaf4)))) (Node4 10 (Leaf4) (Node4 14 (Node4 13 (Leaf4) (Leaf4)) (Leaf4)))
list4 :: [Int]
list4 = [8, 10, 15, 12, 20, 25, 17]

pp4 :: Tree4 -> RoseTree
pp4 (Leaf4) = RoseNode [] []
pp4 (Node4 n l r) = RoseNode (show n) $ [(pp4 l)] ++ [(pp4 r)]

mirrorTree4 :: Tree4 -> Tree4
mirrorTree4 (Leaf4) = Leaf4
mirrorTree4 (Node4 n l r) = Node4 n r l

--a
insertTree :: Int -> Tree4 -> Tree4
insertTree x (Leaf4) = Node4 x (Leaf4) (Leaf4)
insertTree x (Node4 n l r)
	| x <= n = Node4 n (insertTree x l) r
	| otherwise = Node4 n l (insertTree x r)

--b
-- showRoseTree $ pp4 $ makeTreeR list4
makeTreeR' :: [Int] -> Tree4 -> Tree4
makeTreeR' [] tree = tree
makeTreeR' (x:xs) tree = makeTreeR' xs (insertTree x tree)

makeTreeR :: [Int] -> Tree4
makeTreeR list = makeTreeR' list Leaf4

-- showRoseTree $ pp4 $ makeTreeHO list4
makeTreeHO :: [Int] -> Tree4
makeTreeHO list = foldr insertTree Leaf4 list

--c
makeList :: Tree4 -> [Int]
makeList (Leaf4) =  []
makeList (Node4 n l r) = [] ++ (makeList l) ++ [n] ++ (makeList r)

--d
--sortListR/HO list4
sortListR :: [Int] -> [Int]
sortListR list = makeList $ makeTreeR list 

sortListHO :: [Int] -> [Int]
sortListHO list = makeList $ makeTreeHO list 

--e
--showRoseTreeList [(pp4 tree4), (pp4 $ mirrorTree4 tree4), (pp4 $ sortTreeR $ mirrorTree4 tree4), (pp4 $ sortTreeHO $ mirrorTree4 tree4)] 
sortTreeR :: Tree4 -> Tree4
sortTreeR tree = makeTreeR $ makeList tree

sortTreeHO :: Tree4 -> Tree4
sortTreeHO tree = makeTreeHO $ makeList tree

--5
--showRoseTreeList [(pp4 tree4), (pp4 $ subTreeAt tree4 3), (pp4 $ subTreeAt tree4 10)]
--subTreeAt tree4 5
subTreeAt :: Tree4 -> Int -> Tree4
subTreeAt (Leaf4) x = error "Not in tree"
subTreeAt (Node4 n l r) x
	| x == n = Node4 n l r
	| x <= n = subTreeAt l x
	| otherwise = subTreeAt r x

--6
--showRoseTreeList [(pp4 tree4), (pp4 $ cutOffAt tree4 2), (pp4 $ cutOffAt tree4 1), (pp4 $ cutOffAt tree4 0)]
cutOffAt' :: Tree4 -> Int -> Int -> Tree4
cutOffAt' (Leaf4) cut depth = Leaf4
cutOffAt' tree@(Node4 n l r) cut depth
	| depth == cut = Node4 n Leaf4 Leaf4
	| depth < cut= Node4 n (cutOffAt' l cut (depth + 1)) (cutOffAt' r cut (depth + 1))
	| otherwise = error "Fix yo params..."

cutOffAt :: Tree4 -> Int -> Tree4
cutOffAt tree cut = cutOffAt' tree cut 0

--7
--a
--showRoseTreeList [(pp1a tree1a), (pp1a $ replace tree1a "rrl" 100)]
--replace tree1a "rrrr" 100
--replace tree1a "rfl" 100
replace :: Tree1a -> [Char] -> Int -> Tree1a
replace (Leaf1a n) [] x = Leaf1a x
replace (Leaf1a n) _ _ = error "Path not in tree"
replace (Node1a n l r) [] x = Node1a x l r
replace (Node1a n l r) (y:ys) x
	| y == 'l' = Node1a n (replace l ys x) r
	| y == 'r' = Node1a n l (replace r ys x)
	| otherwise = error "Invalid path"

--b
--showRoseTreeList [(pp1a tree1a), (pp1a $ subTree tree1a "rr")]
--subTree tree1a "rrrr"
--subTree tree1a "rfl"
subTree :: Tree1a -> [Char] -> Tree1a
subTree leaf@(Leaf1a n) [] = leaf
subTree leaf@(Leaf1a n) _ = error "Path not in tree"
subTree node@(Node1a n l r) [] = node
subTree node@(Node1a n l r) (x:xs)
	| x == 'l' = subTree l xs
	| x == 'r' = subTree r xs
	| otherwise = error "Invalid path"

--8
{-
Using Tree4 instead, as the makeList functions etc also use this type.
-}
--a
--showRoseTreeList [(pp4 baltree1), (pp4 baltree2), (pp4 ubaltree)]
baltree1 = Node4 15 (Node4 10 (Node4 8 (Leaf4) (Leaf4)) (Node4 12 (Leaf4) (Leaf4))) (Node4 20 (Node4 17 (Leaf4) (Leaf4))(Node4 25 (Leaf4) (Leaf4)))
baltree2 = Node4 15 (Node4 10 (Node4 8 (Leaf4) (Leaf4)) (Node4 12 (Leaf4) (Leaf4))) (Node4 20 (Node4 17 (Leaf4) (Leaf4))(Node4 25 (Leaf4) (Node4 30 (Leaf4) (Leaf4))))
ubaltree = Node4 15 (Node4 10 (Node4 8 (Leaf4) (Leaf4)) (Node4 12 (Leaf4) (Leaf4))) (Node4 20 (Node4 17 (Leaf4) (Leaf4))(Node4 25 (Leaf4) (Node4 30 (Leaf4) (Node4 35 (Leaf4) (Leaf4)))))
ubalextr = Node4 15 (Node4 14 (Node4 13 (Node4 12 (Node4 11 (Node4 10 (Node4 9 (Node4 8 (Node4 7 (Leaf4)(Leaf4))(Leaf4))(Leaf4))(Leaf4))(Leaf4))(Leaf4))(Leaf4))(Leaf4)) (Node4 16 (Leaf4) (Node4 17 (Leaf4) (Node4 18 (Leaf4) (Node4 19 (Node4 29 (Leaf4) (Node4 30 (Leaf4) (Node4 31 (Leaf4) (Node4 32 (Leaf4) (Node4 33 (Leaf4) (Node4 34 (Leaf4) (Node4 35 (Leaf4) (Node4 36 (Leaf4) (Node4 37 (Leaf4) (Leaf4)))))))))) (Node4 20 (Leaf4) (Node4 21 (Leaf4) (Node4 22 (Leaf4) (Node4 23 (Leaf4) (Node4 24 (Leaf4) (Node4 25 (Leaf4) (Node4 26 (Leaf4) (Node4 27 (Leaf4) (Node4 28 (Leaf4) (Leaf4))))))))))))))

isBalanced :: Tree4 -> Bool
isBalanced leaf@(Leaf4) = True
isBalanced node@(Node4 n l r) = (difference <= 1) && (isBalanced l) && (isBalanced r)
	where difference = abs $ (height l) - (height r)

height :: Tree4 -> Int
height (Leaf4) = 0
height (Node4 _ l r) = 1 + max (height l) (height r)

--b
--showRoseTreeList [(pp4 tree4), (pp4 $ balance tree4), (pp4 ubaltree), (pp4 $ balance ubaltree)]
--showRoseTreeList [(pp4 ubalextr), (pp4 $ balance' $ sort $ makeList ubalextr)]
balance' :: [Int] -> Tree4
balance' [x] = Node4 x (Leaf4) (Leaf4)
balance' [x,y] = Node4 y (Node4 x (Leaf4) (Leaf4)) (Leaf4)
balance' list = Node4 im (balance' il) (balance' ir)
	where
	im = list !! n 
	il = take n list
	ir = drop (n+1) list
	n = length list `div` 2

balance :: Tree4 -> Tree4
balance tree = balance' $ makeList tree