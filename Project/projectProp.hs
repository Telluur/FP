module ProjectProp where

data Clause = Fact Identifier
  | Rule Identifier [Identifier]
  deriving (Show)

type Query = [Char]
type Identifier = [Char]
type Program = [Clause]

myprogram :: Program
myprogram = [a0, a1, a2, b0, b1, b2, c0, c1]

a0 :: Clause
a0 = Fact "a0"

a1 :: Clause
a1 = Fact "a1"

a2 :: Clause
a2 = Fact "a2"

b0 :: Clause
b0 = Rule "b0" ["a0", "a1"]

b1 :: Clause
b1 = Rule "b1" ["a1", "a2"]

b2 :: Clause
b2 = Rule "b2" ["a1", "a2", "d"]

c0 :: Clause
c0 = Rule "c0" ["b0", "b1"]

c1 :: Clause
c1 = Rule "c1" ["b0", "b1", "b2"]

evalProp :: Query -> Program -> Bool
evalProp q p = match p p q

{-
There is probably a better way to iterate over an algabraic data structure...
-}
match :: Program -> Program -> Identifier -> Bool
match c (f@(Fact id):xs) q
  | q == id = True
  | otherwise = match c xs q
match c (r@(Rule id ids):xs) q
  | q == id = foldl (&&) True $ map (match c c) ids
  | otherwise = match c xs q
match _ _ _ = False
