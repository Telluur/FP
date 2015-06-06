{-# LANGUAGE MultiWayIf #-}
module ProjectMulti where

{-
Data structures
-}
data Clause = Fact (Predicate, [Identifier])
  | Rule (Predicate, [Argument]) [(Predicate, [Argument])]
  --deriving (Show)

data Query = Q [(Predicate, [Argument])]-- deriving (Show)

data Argument = C Identifier | V Identifier deriving (Eq)

type Identifier = String
type Predicate = String
type Program = [Clause]
type Substitution = (Argument, Argument)
type Substitutions = [Substitution]

{-
Pretty printing
-}
instance Show Clause where
  show (Fact (predi, idents)) = predi ++ "(" ++ init (foldl (++) "" $ map (++ ",") idents) ++ "). "
  show (Rule tup xs) = tupletostring tup ++ " := " ++ init (foldl (++) "" (map ((++ ",") . tupletostring) xs)) ++ "."

instance Show Query where
  show (Q list) = init (foldl (++) "" (map ((++ ",") . tupletostring) list)) ++ "."

instance Show Argument where
  show (V ident) = ident
  show (C ident) = ident


tupletostring :: (Predicate, [Argument]) -> String
tupletostring (predi, args) = predi ++ "(" ++ argstostring args ++ ")"

argstostring :: [Argument] -> String
argstostring args = init $ foldl (++) "" $ map showident args
  where
    showident i@(V _) = show i ++ ","
    showident i@(C _) = show i ++ ","

prettyProgramPrint :: Program -> IO()
prettyProgramPrint clauses = putStrLn $ foldl (++) "\n" $ map ((++ "\n") . show) clauses
ppp :: Program -> IO()
ppp p = prettyProgramPrint p

{-
Main function

We make a distinction between substituion queries and proof queries.
the first guard will handle substitutions, the second proofs.

evalP does not yet exist as a function, but is very similair to evalS,
instead of substitutions it will return true if a proof in that particulair branch is found,
and false when unification is not possible or no matching facts have been found.
Ultimately, this list of bools will be folded with an boolean or,
and thus return true if there exists a branch that yields true, and false if none of the branches yield true.
-}
evalMulti :: Program -> Query -> (Either Bool [Substitutions])
evalMulti p q
  | gotVars q = Right $ evalS p p q [] --substitution query, use evalS
  | otherwise = Left False --proof, use evalP
  where
    gotVars (Q xs) = foldl (||) False $ map check $ foldl (++) [] $ map (\(_,a) -> a) xs
      where
        check (V _) = True
        check (C _) = False

{-
Internal help functions
-}


{-
evalS finds all substitutions possible for a query.
It does this by searching through the WHOLE search tree,
(When a match is found, deepen that subtree while also looking for other possible subtrees.)
-}
evalS :: Program -> Program -> Query -> Substitutions -> [Substitutions]
evalS _ _ (Q []) s = [s] --Base case, if query empty, return subs
evalS _ [] _ _ = [] --No more solutions found and query not empty := false
evalS pc (p@(Fact c@(predi, idents)):cs) q@(Q (a@(qpredi, qargs):as)) s = if
  | (predi == qpredi) && ((length idents) == (length qargs)) -> if --check for predicate/arglength mismatch
    | containsVars a -> if --query contains vars, unification/substitution needed.
      | ql == [] -> [] --Could not unify, return result found.
      | otherwise -> evalS pc pc newq news  --re-Eval unified query
    | idents == qidents -> (evalS pc pc (Q as) s) ++ (evalS pc cs (Q as) s) --A match!, lefthandside: deeper in search tree, righthandside, find more branches.
    | otherwise -> evalS pc cs q s --no vars and no match
  | otherwise -> evalS pc cs q s --predicate or arglength mismatch
  where
    qidents = map rewrite qargs
    rewrite (C ci) = ci
    rewrite (V vi) = vi
    (newq@(Q ql), news) = unifyS p q s
evalS pc (p@(Rule c@(predi, args) l):cs) q@(Q (a@(qpredi, qargs):as)) s = if
  | (predi == qpredi) && ((length args) == (length qargs)) -> if --check for predicate/arglength mismatch
    | qvars || pvars -> if --Either query or rule contains vars, unification/substitution needed.
      | ql == [] -> [] --Could not unify, no result found.
      | otherwise -> evalS pc pc newq news  --re-Eval unified query
    | args == qargs -> (evalS pc pc (Q as) s) ++ (evalS pc cs (Q as) s) --constant type match!: deeper in search tree, righthandside, find more branches.
    | otherwise -> evalS pc cs q s --no vars and no match
  | otherwise -> evalS pc cs q s --predicate or arglength mismatch
  where
    qvars = containsVars a
    pvars = containsVars c
    (newq@(Q ql), news) = unifyS p q s

{-
Unifies a query with a matching clause.
Rules need to be standardized, Facts skip this step.
-}
unifyS :: Clause -> Query -> Substitutions -> (Query, Substitutions)
unifyS c@(Rule (predi, args) l) z@(Q (q@(qpredi, qargs):qs)) os = unifier (standardize c z) z os
unifyS c@(Fact (predi, idents)) z@(Q (q@(qpredi, qargs):qs)) os = unifier c z os

{-
This function applies the calculated general substitution from the function "unifier'" to the
  - right hand side of the rule
  - tail of the current query
and appends the tail to the rules' right hand side.

since this function only unifies (and thus always returns a query), we can signal the eval function that unification was impossible with an empty query.
-}
unifier :: Clause -> Query -> Substitutions -> (Query, Substitutions)
unifier c@(Rule (predi, args) l) z@(Q (q@(qpredi, qargs):qs)) os
  | unic == uniq = ((Q (qh ++ qt)), (os ++ s'))
  | otherwise = (Q [], []) --TODO failure case, handled higher
  where
    (unic, uniq, s) = unifier' 0 (args, qargs, []) --calculates the general unifier
    s' = deleteCToC s
    qh = unifier'replace s' l
    qt = unifier'replace s' qs
unifier f@(Fact (predi, idents)) z@(Q (q@(qpredi, qargs):qs)) os
  | unic == uniq = (Q ((predi,uniq) : qt), (os ++ s'))
  | otherwise = (Q [], []) --TODO failure case, handled higer
  where
    args = map (\ i -> C i) idents
    (unic, uniq, s) = unifier' 0 (args, qargs, []) --calculates the general unifier
    s' = deleteCToC s
    qt = unifier'replace s' qs

{-
Calculates the general substitution.
It does this by iterting (length args) times,
this does produce a side effect: the substitutions may contain (Constant "x" -> Constant "x") maps.
However, these can be simply filtered away later as they do not influence the actual unification.
-}
unifier' :: Int -> ([Argument], [Argument], Substitutions) -> ([Argument], [Argument], Substitutions)
unifier' iteration r@(c, q, subs)
  | iteration >= l = r
  | otherwise = unifier' (iteration + 1) (c', q', (subs ++ [s]))
  where
    l = length q
    s = unifier'match (c !! iteration) (q !! iteration)
    c' = map (subswap s) c --apply new substitution on rule
    q' = map (subswap s) q --apply new substitution on query

--help function that maps the substitions to the (predicate, arguments) tuples.
unifier'replace :: Substitutions -> [(Predicate, [Argument])] -> [(Predicate, [Argument])]
unifier'replace [] q = q
unifier'replace (x:xs) q = unifier'replace xs q'
  where
    q' = map (\(a, args) -> (a, map (subswap x) args)) q

--Help function that determines the actual substitutions based on their type; variable or constant.
unifier'match :: Argument -> Argument -> (Argument, Argument)
unifier'match c@(C _) (C _) = (c, c) --moch sub, will fail or pass later
unifier'match c@(C _) v@(V _) = (v, c)
unifier'match c@(V _) v@(C _) = (c, v)
unifier'match c@(V _) v@(V _) = (v, c)

{-
standardizes a clause based on the presented query,
It does this by adding to the overlapping variable identifiers an `~`.
-}
standardize :: Clause -> Query -> Clause
standardize c@(Rule (predi, args) l) (Q ((qpredi, qargs):qs)) --Only rules can be standardizes, facts must be branched unstead.
  | predi /= qpredi = error "Expected equal predicates while standardizing" --predicates must be equal, failsafe: should never be reached.
  | otherwise = foldl (standardize'swap) c qargs
standardize _ _ = error "1 Unexpected case" --facts need not be standardized, should never be reached.

standardize'swap :: Clause -> Argument -> Clause
standardize'swap c@(Rule (predi, args) l) q@(V qarg) = (Rule (predi, args') l')
  where
    args' = map (rep q) args
    l' = map(\(a,b) -> (a, (map (rep q) b))) l
    rep (V qa) (V va)
      | va == qa = (V (va ++ "~"))
      | otherwise = (V va)
    rep _ ca = ca
standardize'swap c@(Rule (predi, args) l) q@(C qarg) = c
standardize'swap _ _ = error "2 Unexpected case" --facts need not be standardized, should never be reached.

{-
more general help functions, to prevent duplicate code.
-}
subswap :: Substitution -> Argument -> Argument
subswap (old, new) arg
  | old == arg = new
  | otherwise = arg

deleteCToC :: Substitutions -> Substitutions
deleteCToC [] = []
deleteCToC ((C _, C _):xs) = deleteCToC xs
deleteCToC (x:xs) = x : deleteCToC xs

containsVars :: (Predicate, [Argument]) -> Bool
containsVars t@(_, args) = foldl (||) False $ map (check) args
  where
    check (V _) = True
    check (C _) = False
