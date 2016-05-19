module FP_Core where

import FPPrac.Trees

-- ========================================================================
-- Data definitions

type Stack  = [Int]
type Heap = [Int]
type Variable = Int

data Op     = Add | Mul | Sub
            deriving Show

data Instr  = PushConst Int
            | PushAddr Int
            | Store Int
            | Calc Op
            | PushPC
            | EndRep
            | EndProg
            deriving Show

data Tick = Tick

data Expr   = Const Int                   -- for constants
            | Addr Int
            | BinExpr Op Expr Expr        -- for ``binary expressions''
            deriving Show

data Stmnt  = Assign Int Expr
            | Repeat Expr [Stmnt]
            deriving Show


-- ========================================================================
-- Processor functions

(<~) :: [a] -> (Int, a) -> [a]
xs <~ (i,a) = take i xs ++ [a] ++ drop (i+1) xs
                -- Put value a on position i in list xs

alu :: Num a => Op -> a -> a -> a
alu op = case op of
                Add -> (+)
                Mul -> (*)
                Sub -> (-)

core :: [Instr] -> (Int, Int, Heap, Stack) -> Tick -> (Int, Int, Heap, Stack)

core instrs (pc, sp, heap, stack) _ =  case instrs!!pc of
        PushConst n     -> (pc+1, sp+1, heap, stack <~ (sp,n))
        PushAddr a      -> (pc+1, sp+1, heap, stack <~ (sp,v))
            where
                v = heap!!a
        Store a         -> (pc+1, sp-1, heap <~ (a,v), stack)
            where
                v = stack!!(sp-1)
        Calc op         -> (pc+1, sp-1, heap, stack <~ (sp-2,v))
            where
                v = alu op (stack!!(sp-2)) (stack!!(sp-1))
        PushPC          -> (pc+1, sp+1, heap, stack <~ (sp,pc))
        EndRep          | i > 1     -> (pc', sp, heap, stack <~ (sp-2,i-1))
                        | otherwise -> (pc+1, sp-2, heap, stack)
            where
                pc' = stack!!(sp-1) + 1
                i   = stack!!(sp-2)
        EndProg         -> (-1, sp, heap, stack)


-- ========================================================================
-- Code Compilation

class Code a where
    codeGen :: a -> [Instr]
    myToRoseTree :: a -> RoseTree

instance Code Expr where
    codeGen (Const e0) = [PushConst e0]
    codeGen (Addr e0) = [PushAddr e0]
    codeGen (BinExpr e0 e1 e2) = codeGen e1 ++ codeGen e2 ++ [Calc e0]

    myToRoseTree (Const x) = RoseNode (show x) []
    myToRoseTree (Addr x) = RoseNode ("Loc|" ++ show x) []
    myToRoseTree (BinExpr op l r) = RoseNode (show op) $ map myToRoseTree [l, r]

instance Code Stmnt where
    codeGen (Assign e0 e1) = codeGen e1 ++ [Store e0]
    codeGen (Repeat e0 e1) = codeGen e0 ++ [PushPC] ++ concatMap codeGen e1 ++ [EndRep]

    myToRoseTree (Assign e0 e1) = RoseNode "Assign" [RoseNode ("Loc|" ++ show e0) [], myToRoseTree e1]
    myToRoseTree (Repeat e0 e1) = RoseNode "Repeat" $ myToRoseTree e0 : map myToRoseTree e1

showStmnts :: [Stmnt] -> IO()
showStmnts x = showRoseTree $ RoseNode "Program" $ map myToRoseTree x

compile :: Code a => [a] -> [Instr]
compile x = concatMap codeGen x ++ [EndProg]


-- ========================================================================
-- Testing Framework
clock :: [Tick]
clock = repeat Tick

emptyStack :: [Int]
emptyStack = replicate 8 0

emptyHeap :: [Int]
emptyHeap = replicate 8 0

exec :: Code a => [a] -> IO ()
exec i = putStr
       $ unlines
       $ map show
       $ takeWhile (\(pc,_,_,_) -> pc /= -1)
       $ scanl (core $ compile i) (0,0,emptyHeap,emptyStack) clock


-- ========================================================================
-- Tests
-- example Program for expression: (((2*10) + (3*(4+11))) * (12+5)) = 1105
exprTest :: [Expr]
exprTest = [BinExpr Mul
          (BinExpr Add
              (BinExpr Mul
                  (Const 2)
                  (Const 10))
              (BinExpr Mul
                  (Const 3)
                  (BinExpr Add
                      (Const 4)
                      (Const 11))))
          (BinExpr Add
              (Const 12)
              (Const 5))]

-- example program which stores ints x and y in memory, multiplies them, and stores them in heap 7
assignTest :: Int -> Int -> [Stmnt]
assignTest x y = [  Assign 0 (Const x),
                    Assign 1 (Const y),
                    Assign 7 (BinExpr Mul (Addr 0) (Addr 1))]

-- example program which demonstrates nested repeat statements, stores result of nested addition in heap 7 (=2*x*y)
nestedTest :: Int -> Int -> [Stmnt]
nestedTest x y = [  Assign 7 (Const 0),
                    Repeat (Const x) [
                        Repeat (Const y) [
                            Assign 7 (BinExpr Add (Addr 7) (Const 1))]],
                    Repeat (Const x) [
                        Repeat (Const y) [
                            Assign 7 (BinExpr Add (Addr 7) (Const 1))]]]

-- example program which demonstrates the repeat statement implementing fibbionacci.
fib :: Int -> [Stmnt]
fib n   | n < 2 = error "Function argument should be >= 2"
        | otherwise = [
            Assign 0 (Const n),  -- Fibbionaci function that delivers the n'th (n >= 2) fib number in heap 7 (Starting from 0,1,1). fib 10 = 34
            Assign 1 (Const 0),  -- Makes sure that the right values are on the heap from the start
            Assign 2 (Const 1),
            Assign 7 (Const 1),
            Repeat (BinExpr Sub (Addr 0) (Const 2)) [
                Assign 7 (BinExpr Add (Addr 1) (Addr 2)),
                Assign 1 (Addr 2),
                Assign 2 (Addr 7)]]
