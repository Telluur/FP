module FP_Core where

import FPPrac.Trees

{-
Extension of CoreIntro.hs:
- instructions as program *in* the processor,
- stack now is list of fixed length,i
- clock added
- program counter + stack pointer added,
- instruction EndProg added,
- update operaton (<~) added,
-}

-- ========================================================================

type Stack  = [Int]
type Heap = [Int]

data Op     = Add | Mul | Sub
            deriving Show

data Instr  = PushConst Int
            | PushAddr Int
            | Store Int
            | Calc Op
            | EndProg
            deriving Show

data Tick = Tick

data Expr = Const Int                   -- for constants
          | BinExpr Op Expr Expr        -- for ``binary expressions''

data Stmnt = Assign Int Expr


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


core :: [Instr] -> (Int,Int,Heap,Stack) -> Tick -> (Int,Int,Heap,Stack)

core instrs (pc,sp,heap,stack) tick =  case instrs!!pc of
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
        EndProg         -> (-1, sp, heap, stack)

-- ========================================================================
-- example Program for expression: (((2*10) + (3*(4+11))) * (12+5))

-- Tree of this expression of type Expr (result of parsing):
expr :: Expr
expr = BinExpr Mul
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
              (Const 5))

-- The program that results in the value of the expression (1105):
prog :: [Instr]
prog = [ PushConst 2
       , PushConst 10
       , Calc Mul
       , PushConst 3
       , PushConst 4
       , PushConst 11
       , Calc Add
       , Calc Mul
       , Calc Add
       , PushConst 12
       , PushConst 5
       , Calc Add
       , Calc Mul
       , EndProg
       ]

-- Testing
clock :: [Tick]
clock      = repeat Tick

emptyStack :: [Int]
emptyStack = replicate 8 0

emptyHeap :: [Int]
emptyHeap = replicate 8 0

transform :: Expr -> RoseTree
transform (Const x) = RoseNode (show x) []
transform (BinExpr op l r) = RoseNode (show op) $ map transform [l, r]

class Code a where
    codeGen :: a -> [Instr]

instance Code Expr where
    codeGen (Const e0) = [PushConst e0]
    codeGen (BinExpr e0 e1 e2) = codeGen e1 ++ codeGen e2 ++ [Calc e0]

code :: Expr -> [Instr]
code x = codeGen x ++ [EndProg]

test :: IO ()
test       = putStr
           $ unlines
           $ map show
           $ takeWhile (\(pc,_,_,_) -> pc /= -1)
           $ scanl (core prog) (0,0,emptyHeap,emptyStack) clock

exec :: Expr -> IO ()
exec i =    putStr
            $ unlines
            $ map show
            $ takeWhile (\(pc,_,_,_) -> pc /= -1)
            $ scanl (core $ code i) (0,0,emptyHeap,emptyStack) clock
