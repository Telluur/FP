{-# LANGUAGE MultiWayIf #-}
module ProjectMulti where

{-
Data structures
-}
data Clause = Fact (Predicate, [(Identifier)])
  | Rule (Predicate, [Argument]) [(Predicate, [Argument])]

data Query = Q [(Predicate, [Argument])]
data QueryType = S | P

data Argument = C Identifier | V Identifier deriving (Eq)

type Identifier = [Char]
type Predicate = [Char]
type Program = [Clause]
type Substitutions = [(Identifier, Identifier)]

{-
Pretty printing
-}
instance Show Clause where
  show (Fact (predi, idents)) = predi ++ "(" ++ (init (foldl (++) "" $ map (\i -> (i ++ ",")) idents)) ++ "). "
  show (Rule tup xs) = (tupletostring tup) ++ " := " ++ (init (foldl (++) "" (map ((\s -> (s ++ ",")) . tupletostring) xs))) ++ "."

instance Show Query where
  show (Q list) = (init (foldl (++) "" (map ((\s -> (s ++ ",")) . tupletostring) list))) ++ "."

instance Show Argument where
  show (V ident) = ident
  show (C ident) = ident

tupletostring :: (Predicate, [Argument]) -> String
tupletostring (predi, args) = predi ++ "(" ++ (argstostring args) ++ ")"

argstostring :: [Argument] -> String
argstostring args = init $ foldl (++) "" $ map showident args
  where
    showident (V i) = i ++ ","
    showident (C i) = i ++ ","

prettyProgramPrint :: Program -> IO()
prettyProgramPrint clauses = putStrLn $ foldl (++) "\n" $ map ((++ "\n") . show) clauses
ppp :: Program -> IO()
ppp p = prettyProgramPrint p

printResult :: (Either Bool [Substitutions]) -> IO()
printResult (Left bool) = putStrLn $ show bool
printResult (Right []) = putStrLn "No substitutions found."
--printResult (Right subs) = putStrLn $ foldl (++) "\n" $ map (\(a,b) -> "[" ++ a ++ ":=" ++ b ++ "]\n") subs

{-
Main function
-}
evalMulti :: Program -> Query -> (Either Bool [Substitutions])
evalMulti _ q
  | gotVars q = Left True --substitution query
  | otherwise = Left False --proof
  where
    gotVars (Q xs) = foldl (||) False $ map check $ foldl (++) [] $ map (\(_,a) -> a) xs
      where
        check (V _) = True
        check (C _) = False

{-
Internal help functions
-}
containsVars :: (Predicate, [Argument]) -> Bool
containsVars t@(_, args) = foldl (||) False $ map (check) args
  where
    check (V _) = True
    check (C _) = False


evalS :: Program -> Program -> Query -> Substitutions -> [Substitutions]
evalS _ _ (Q []) s = [s] --Base case, if query empty, return subs
evalS _ [] _ _ = [] --No more solutions found and query not empty := false
evalS pc p@((Fact c@(predi, idents)):cs) q@(Q (a@(qpredi, qargs):as)) s = if
  | (predi == qpredi) && ((length idents) == (length qargs)) -> if --check for predicate/arglength mismatch
    | containsVars a -> [s]--query contains vars, substitution needed.
    | idents == qidents -> (evalS pc pc (Q as) s) ++ (evalS pc cs (Q as) s) --A match!, lefthandside: deeper in search tree, righthandside, find more branches.
    | otherwise -> evalS pc cs q s --no vars and no match
  | otherwise -> evalS pc cs q s --predicate or arglength mismatch
  where
    qidents = map rewrite qargs
    rewrite (C ci) = ci
    rewrite (V vi) = vi
evalS pc p@((Rule c@(predi, args) l):cs) q@(Q (a@(qpredi, qargs):as)) s = if
  | (predi == qpredi) && ((length args) == (length qargs)) -> if --check for predicate/arglength mismatch
    | qvars || pvars -> [s] --Either query or rule contains rules, substitution needed.
    | args == qargs -> (evalS pc pc (Q as) s) ++ (evalS pc cs (Q as) s) --constant type match!: deeper in search tree, righthandside, find more branches.
    | otherwise -> evalS pc cs q s --no vars and no match
  | otherwise -> evalS pc cs q s --predicate or arglength mismatch
  where
    qvars = containsVars a
    pvars = containsVars c
