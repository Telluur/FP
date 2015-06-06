module TestMulti where

import ProjectMulti

myprogram :: Program
myprogram = [wj, wb, wm, wi, wc, mjb, mjm, mji, fbb, fbm, c0, c1, so, si]
mp = myprogram
wj :: Clause
wj = Fact ("women", ["juliana"])
wb :: Clause
wb = Fact ("women", ["beatrix"])
wm :: Clause
wm = Fact ("women", ["margriet"])
wi :: Clause
wi = Fact ("women", ["irene"])
wc :: Clause
wc = Fact ("women", ["christina"])
mjb :: Clause
mjb = Fact ("mother", ["juliana","beatrix"])
mjm :: Clause
mjm = Fact ("mother", ["juliana","margriet"])
mji :: Clause
mji = Fact ("mother", ["juliana","irene"])
fbb :: Clause
fbb = Fact ("father", ["bernhard", "beatrix"])
fbm :: Clause
fbm = Fact ("father", ["bernhard", "margriet"])
c0 :: Clause
c0 = Rule ("child", [V "K", V "O"]) [("mother", [V "O", V "K"])]
c1 :: Clause
c1 = Rule ("child", [V "K", V "O"]) [("father", [V "O", V "K"])]
so :: Clause
so = Rule ("son", [V "Z", V "O"]) [("child", [V "Z", V "O"]), ("man", [V "Z"])]
si :: Clause
si = Rule ("sister", [V "X", V "Y"]) [("child", [V "X", V "O"]), ("woman", [V "X"]), ("child", [V "Y", V "O"])]

{-
Example queries

See data structure in ProjectMulti.hs
-}
q1 :: Query
q1 = Q [("sister", [C "beatrix", C "margriet"])]
q2 :: Query
q2 = Q [("sister", [C "breatrix", V "X"])]
q3 :: Query
q3 = Q [("sister", [V "X", V "Y"])]
q4 :: Query
q4 = Q [("woman", [C "juliana"])]


{-
Test idividual functions
-}

{-
Standardization
Q: sister(X,Y). = q3
R: sister(X,Y) := child(X,O),woman(X),child(Y,O). = si
-}
standard :: Clause
standard = standardize si q3

{-
unification (and Standardization) of
Q: sister(X,Y). = q3
R: sister(X,Y) := child(X,O),woman(X),child(Y,O). = si
-}
ur1@(uq1, ub1) = unifyS si q3 [] --Last argument is empty, cause of recursive purposes, this first call is always an empty list.

{-
The next step is another unification, namely with one of the child rules.
Q: child(X~,O),woman(X~),child(Y~,O).
R: child(K,O) := mother(O,K). = c0
-}

ur2@(uq2, ub2) = unifyS c0 uq1 ub1 --uq1 and ub1 are results from previous queries
