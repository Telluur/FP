module Exercise2d where

import Exercise2a

type Path = String

subboom :: Tree -> Path -> Tree
subboom t@(Leaf _) [] = t
subboom (Leaf _) _ = error "Invalid path"
subboom t@Node{} [] = t
subboom (Node l _ r) (p:ps)
  | p == 'l' = subboom l ps
  | p == 'r' = subboom r ps
  | otherwise = error "Invalid path"
