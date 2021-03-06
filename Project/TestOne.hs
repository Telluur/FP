module TestOne where
import ProjectOne

{-
The program as displayed in the instructions.

use: prettyProgramPrint :: Program -> IO() or its alias ppp :: Program -> IO()
to print the program to io in a prolog style.
-}
myprogram :: Program
myprogram = [pa, pb, pc, qa, qb, rX]
mp = myprogram
pa :: Clause
pa = Fact ("p", "a")
pb :: Clause
pb = Fact ("p", "b")
pc :: Clause
pc = Fact ("p", "c")
qa :: Clause
qa = Fact ("q", "a")
qb :: Clause
qb = Fact ("q", "b")
rX :: Clause
rX = Rule ("r", Variable "X") [("p", Variable "X"),("q", Variable "X")]

{-
Example queries

A query MUST be in the form of a Clause Rule, eg:
For a true/false query:
- Rule ("r", Constant "a") []

For a substitution
- Rule ("r", Variable "X") []

Yes, every query has a redundant empty list at the end, this makes recursion in the implementation way easier.
As a matter of fact, the list at the end is completely ignored anyway.

Also, use the printResult function to get a proper representation of the result. evalOne will return internal structures.
-}

qra :: IO()
qra = printResult $ evalOne myprogram (Rule ("r", Constant "a") [])
qrb :: IO()
qrb = printResult $ evalOne myprogram (Rule ("r", Constant "b") [])
qrc :: IO()
qrc = printResult $ evalOne myprogram (Rule ("r", Constant "c") [])
qrx :: IO()
qrx = printResult $ evalOne myprogram (Rule ("r", Variable "X") [])
qry :: IO()
qry = printResult $ evalOne myprogram (Rule ("r", Variable "Y") [])
