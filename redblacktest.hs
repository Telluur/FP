module RedBlackTest where
import Series5
import FPPrac.Trees

--Insertion and deletion
test :: IO ()
test = showRBTreeList $ map pp [tt, tt1, tt2, tt3, tt2', tt1', tt']

tt :: Tree
tt = Tree Black (Node (Tree Black (Node (Tree Red (Node (leaf) 3 (leaf))) 7 (Tree Red (Node (leaf) 10 (leaf))))) 15 (Tree Black (Node (leaf) 25 (leaf))))
 where leaf = Tree Black Leaf
tt1 :: Tree
tt1 = balancedInsert tt 9
tt2 :: Tree
tt2 = balancedInsert tt1 8
tt3 :: Tree
tt3 = balancedInsert tt2 11
tt4 :: Tree
tt4 = balancedInsert tt3 12
tt5 :: Tree
tt5 = balancedInsert tt4 13
tt2' :: Tree
tt2' = delete tt3 11
tt1' :: Tree
tt1' = delete tt2' 8
tt' :: Tree
tt' = delete tt1' 9

bigtree :: Tree
bigtree = Tree Black (Node (Tree Black (Node (Tree Black (Node (Tree Black Leaf) 2 (Tree Red (Node (Tree Black Leaf) 3 (Tree Black Leaf))))) 4 (Tree Black (Node (Tree Red (Node (Tree Black Leaf) 5 (Tree Black Leaf))) 6 (Tree Black Leaf))))) 7 (Tree Black (Node (Tree Black (Node (Tree Black Leaf) 8 (Tree Red (Node (Tree Black Leaf) 11 (Tree Black Leaf))))) 16 (Tree Red (Node (Tree Black (Node (Tree Red (Node (Tree Black Leaf) 18 (Tree Black Leaf))) 19 (Tree Black Leaf))) 24 (Tree Black(Node (Tree Red (Node (Tree Black Leaf) 25 (Tree Black Leaf))) 29 (Tree Red (Node (Tree Black Leaf) 33 (Tree Black Leaf))))))))))

--Test the grey colour flips (Including the mirror ones).
-- invars a/b1/b2/c1/c2/d/e1/e2
invars :: Tree -> IO()
invars tree = showRBTreeList $ map pp [tree, gcf tree, treem, gcf treem] where treem = mirror tree

mirror :: Tree -> Tree
mirror (Tree c (Node l x r)) = Tree c (Node r x l)

a :: Tree
a = Tree Black (Node (Tree Grey g) p (Tree Black (Node (Tree Black l) s (Tree  Black r))))
	where
	p = 10
	g = Leaf
	s = 15
	l = Leaf
	r = Leaf

b' :: Colour -> Colour -> TreeNode -> Tree
b' cp cr r = Tree cp (Node (Tree Grey g) p (Tree Black (Node (Tree Red (Node (Tree Black a) l (Tree Black b))) s (Tree cr r))))
	where
	g = Leaf
	p = 10
	s = 15
	l = 12
	a = Leaf
	b = Leaf

b1 :: Tree
b1 = b'  Black Black Leaf
b2 :: Tree
b2 = b' Red Red (Node (Tree Black Leaf) 20 (Tree Black Leaf))

c' :: Colour -> TreeNode -> Tree
c' cr r = Tree Red (Node (Tree Grey g) p (Tree Black (Node (Tree Black l) s (Tree cr r))))
  where
  p = 10
  g = Leaf
  s = 20
  l = Leaf

c1 :: Tree
c1 = c' Black Leaf
c2 :: Tree
c2 = c' Red (Node (Tree Black Leaf) 30 (Tree Black Leaf))

d :: Tree
d = Tree Black (Node (Tree Grey g) p (Tree Black (Node (Tree Black l) s (Tree Red (Node (Tree Black Leaf) r (Tree Black Leaf))))))
  where
  p = 20
  g = Leaf
  s = 30
  l = Leaf
  r = 40

e' :: TreeNode -> TreeNode -> Tree
e' l r = Tree Black (Node (Tree Grey g) p (Tree Red (Node (Tree Black l) s (Tree Black r))))
  where
  p = 20
  g = Leaf
  s = 30

e1 :: Tree
e1 = e' Leaf Leaf
e2 :: Tree
e2 = e' l r
  where
  l = Node (Tree Red (Node (Tree Black Leaf) 22 (Tree Black Leaf))) 25 (Tree Red (Node (Tree Black Leaf) 28 (Tree Black Leaf)))
  r = Node (Tree Red (Node (Tree Black Leaf) 32 (Tree Black Leaf))) 35 (Tree Red (Node (Tree Black Leaf) 39 (Tree Black Leaf)))
