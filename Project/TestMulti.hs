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
si = Rule ("sister", [V "X", V "Y"]) [("child", [V "X", V "O"]), ("child", [V "Y", V "O"]), ("woman", [V "X"])]

q1 :: Query
q1 = Q [("sister", [C "beatrix", C "margriet"])]
q2 :: Query
q2 = Q [("sister", [V "X", C "margriet"])]
