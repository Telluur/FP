module ProjectOne where

{-
Data structures
-}
data Clause = Fact (Predicate, Identifier)
  | Rule (Predicate, Argument) [(Predicate, Argument)]
  deriving (Eq)

data Argument = Variable Identifier | Constant Identifier deriving (Eq)

type Query = Clause
type Identifier = [Char]
type Predicate = [Char]
type Program = [Clause]
type Substitutions = (Identifier, Identifier)

{-
pretty Printing
-}
instance Show Clause where
  show (Fact (predi, ident)) = predi ++ "(" ++ ident ++ "). "
  show (Rule (predi, argument) []) = predi ++ "(" ++ (show argument) ++ ")" --Support for printing substitution query
  show (Rule (predi, argument) xs) = predi ++ "(" ++ (show argument) ++ ") := " ++ ((init $ foldl (++) "" $ map (\(a,b) -> (a ++ "(" ++ show(b) ++ "),")) xs) ++ ".")

instance Show Argument where
  show (Variable ident) = ident
  show (Constant ident) = ident

prettyProgramPrint :: Program -> IO()
prettyProgramPrint clauses = putStrLn $ foldl (++) "\n" $ map ((++ "\n") . show) clauses
ppp :: Program -> IO()
ppp p = prettyProgramPrint p

printResult :: (Either Bool [Substitutions]) -> IO()
printResult (Left bool) = putStrLn $ show bool
printResult (Right []) = putStrLn "No substitutions found."
printResult (Right subs) = putStrLn $ foldl (++) "\n" $ map (\(a,b) -> "[" ++ a ++ ":=" ++ b ++ "]\n") subs


{-
Evaluation function
-}
evalOne :: Program -> Query -> (Either Bool [Substitutions])
--simple proof, list in query should be empty.
evalOne p q@(Rule (_, (Constant _)) _) = Left $ prove p p q
--display for wich values of the var is True, list in query should be empty.
evalOne p q@(Rule (_, (Variable ident)) _) = Right $ map (\(Rule (predi, Constant i) _) -> (ident, i)) $ removeDuplicates $ foldl (++) [] $ map (substitutions p p) $ subRules p q
--we do not allow queries in the form of Clause Facts.
evalOne _ (Fact (predi, ident)) = error "Please rewrite query as a Clause Rule"


{-
Internal functions
-}
prove :: Program -> Program -> Query -> Bool
prove c ((Fact (predi, ident)):xs) q@(Rule (qpredi, Constant qident) _) -- compares 'x:pred(const)' with 'q:pred(const)'
  | (predi == qpredi) && (ident == qident) = True
  | otherwise = prove c xs q
prove c ((Rule (predi, (Constant ident)) clauses):xs) q@(Rule (qpredi, Constant qident) _) --compares 'x:pred(const) := ...' with 'q:pred(const)'
  | (predi == qpredi) && (ident == qident) = foldl (&&) True $ map ((prove c c) . (\(a,b) -> Rule (a, b) [])) clauses
  | otherwise = prove c xs q
prove c (x@(Rule (predi, (Variable ident)) clauses):xs) (Rule (qpredi, Constant qident) _) --compares 'x:pred(var) := ...' with 'q:pred(const)', thus performing substitution
  | predi == qpredi = foldl (&&) True $ map ((prove c c) . (\(a,b) -> Rule (a, b) [])) clauses'
    where
      (Rule (_, (Constant _)) clauses') = substitute x qident
prove _ _ _ = False

--Makes sure that all identifiers that are constants will evaluate. eg: Query: r(Y) while the program only holds r(X)
subRules :: Program -> Query -> [Query]
subRules (x@(Rule (predi, Variable ident) _):xs) q@(Rule (qpredi, Variable qident) _)
  | predi == qpredi = (varSubstitute x qident) : subRules xs q
  | otherwise = subRules xs q
subRules (x:xs) q = subRules xs q
subRules [] _ = []

--Help function for subRules, as the property that the rules must remain variables is preseverd.
varSubstitute :: Clause -> Identifier -> Clause
varSubstitute (Rule x@(_, Variable _) xs) ident = Rule (varSubstitute' ident x) $ map (varSubstitute' ident) xs

varSubstitute' :: Identifier -> (Predicate, Argument) -> (Predicate, Argument)
varSubstitute' ident (predi, Variable vari) = (predi, Variable ident) --Variable in tuple, sub.
varSubstitute' _ o@(predi, Constant _) = o -- Constant in tuple, do not sub.

--Iterates over the possible subtitution queries, if eval to true, add to result list. This can yield duplicates.
substitutions :: Program -> Program -> Query -> [Clause]
substitutions c (x@(Fact f@(predi, ident)):xs) q@(Rule (qpredi, Variable qident) _)
  | result == True = (Rule (qpredi, Constant ident) []) : substitutions c xs q
  | otherwise = substitutions c xs q
  where
    q' = substitute q ident
    result = prove c c q'
substitutions c (x:xs) q = substitutions c xs q
substitutions _ [] _ = []


--Substituts the variables with constants
substitute :: Clause -> Identifier -> Clause
substitute (Rule x@(_, Variable _) xs) ident = Rule (substitute' ident x) $ map (substitute' ident) xs

substitute' :: Identifier -> (Predicate, Argument) -> (Predicate, Argument)
substitute' ident (predi, Variable vari) = (predi, Constant ident) --Variable in tuple, sub.
substitute' _ o@(predi, Constant _) = o -- Constant in tuple, do not sub.

--General help function
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs
