module TestProp where
import ProjectProp


{-
Example Program
-}
myprogram :: Program
myprogram = [a0, a1, a2, b0, b1, b2, c0, c1]
mp = myprogram
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

{-
Example Queries
-}
qc0 :: Bool
qc0 = evalProp "c0" myprogram
qc1 :: Bool
qc1 = evalProp "c1" myprogram
