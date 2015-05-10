{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import FPPrac.Trees
import Data.Char
import Data.List

--1
--a
data BinTree a b = Leaf b
	| Node (BinTree a b) a (BinTree a b)
	deriving (Show)

data Unit = Unit
instance Show Unit where
	show Unit = ""

type Tree1a = BinTree Int Int
type Tree1b = BinTree (Int, Int) (Int, Int)
type Tree1c = BinTree Int Unit
	
--b
t1a :: Tree1a
t1a = Node (Node (Node (Leaf 4) 3 (Leaf 4)) 2 (Node (Leaf 4) 3 (Leaf 4))) 1 (Node (Node (Leaf 4) 3 (Leaf 4)) 2 (Node (Leaf 4) 3 (Leaf 4)))

t1b :: Tree1b
t1b = Node (Node (Node (Leaf (4,4)) (3,3) (Leaf (4,4))) (2,2) (Node (Leaf (4,4)) (3,3) (Leaf (4,4)))) (1,1) (Node (Node (Leaf (4,4)) (3,3) (Leaf (4,4))) (2,2) (Node (Leaf (4,4)) (3,3) (Leaf (4,4))))

t1c :: Tree1c
t1c = Node (Node (Node (Leaf Unit) 3 (Leaf Unit)) 2 (Node (Leaf Unit) 3 (Leaf Unit))) 1 (Node (Node (Leaf Unit) 3 (Leaf Unit)) 2 (Node (Leaf Unit) 3 (Leaf Unit)))

--c
--showRoseTreeList [pp t1a, pp t1b, pp t1c]
pp :: (Show a, Show b) => BinTree a b -> RoseTree
pp (Leaf a) = RoseNode (show a) []
pp (Node l a r) = RoseNode (show a) [pp l, pp r]

showBinTree :: (Show a, Show b) => BinTree a b -> IO ()
showBinTree t = showRoseTree $ pp t

showBinTreeList :: (Show a, Show b) => [BinTree a b] -> IO ()
showBinTreeList l = showRoseTreeList $ map pp l

--2
--Tokenize
data Token = LB 
	| RB 
	| OP String
	| NUM Float
	| VAR String
	deriving (Show)
	
tokenize :: String -> [Token]
tokenize [] = []
tokenize string@(x:xs) 
	| x == '(' = LB : tokenize xs
	| x == ')' = RB : tokenize xs
	| x `elem` "+-*/^=<>" = (OP [x]) : tokenize xs
	| x == ' ' = tokenize xs --ignore whitespace
	| isDigit x = (NUM (read number :: Float)) : tokenize r0 --number must begin with a digit, else read won't parse to a float
	| x == '~' = (NUM $ 0 - (read negNumber :: Float)) : tokenize r1 --ugly way to make negative, but it works :D
	| isAlpha x = (VAR word) : tokenize r2
	where 
	(number, r0) = getNumberPart string
	(negNumber, r1) = getNegNumberPart xs
	(word, r2) = getWord string

--helper functions
getNumberPart :: String -> (String, String)
getNumberPart string = (takeWhile isNumberPart string, dropWhile isNumberPart string)

isNumberPart :: Char -> Bool
isNumberPart x = x `elem` "0123456789."

getNegNumberPart :: String -> (String, String)
getNegNumberPart string@(x:xs) 
	| isDigit x = (takeWhile isNumberPart string, dropWhile isNumberPart string) --makes sure that leading number is a digit as ".0123" will not parse in a float.
	| otherwise = error "Negative number unparsable: Must start with a leading zero."

getWord :: String -> (String, String)
getWord string = (takeWhile isAlpha string, dropWhile isAlpha string)
	
--Parse
data S = E | O	

parse :: String -> BinTree Token (Either Token Float)
parse string = fst $ parse' $ tokenize string

parse' :: [Token] -> (BinTree Token (Either Token Float), [Token])
parse' ((LB):xs) = (Node ll o rl, r3) 
	where 
	(ll, r0) = parse' xs
	((Node _ o _), r1) = parse' r0
	(rl, r2) = parse' r1
	r3 = parse'matchRB r2 --Makes sure a closing bracket follows.
parse' ((NUM x):xs) = (Leaf $ Right x, xs)
parse' ((VAR x):xs) = (Leaf $ Left (VAR x), xs)
parse' ((OP x):xs) = (Node (Leaf $ Left LB) (OP x) (Leaf $ Left LB), xs)
parse' (x:xs) = error "Invalid expression: Invalid token in sequence."


parse'matchRB :: [Token] -> [Token]
parse'matchRB ((RB):xs) = xs
parse'matchRB _ = error "Invalid expression: No matching closing bracket found."


--display
parseAndShowExpr :: String -> IO ()
parseAndShowExpr e = showBinTree $ parse e
pase e = parseAndShowExpr e

parseAndShowExprList :: [String] -> IO ()
parseAndShowExprList el = showBinTreeList $ map (parse) el
pasel el = parseAndShowExprList el

e0 = "((1.5/3) = (1+~2))"

	
