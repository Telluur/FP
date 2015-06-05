module ProjectProp where

data Clause = Fact Identifier
  | Rule Identifier [Identifier]
  deriving (Show)

type Query = [Char]
type Identifier = [Char]
type Program = [Clause]

evalProp :: Query -> Program -> Bool
evalProp q p = match p p q

{-
There is probably a better way to iterate over an algabraic data structure...
-}
match :: Program -> Program -> Identifier -> Bool
match c ((Fact ident):xs) q
  | q == ident = True
  | otherwise = match c xs q
match c ((Rule ident idents):xs) q
  | q == ident = foldl (&&) True $ map (match c c) idents
  | otherwise = match c xs q
match _ _ _ = False
