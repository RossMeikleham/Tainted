module TaintedExample where

import Data.Tainted
import Control.Monad

data Expr = 
      Number (Tainted Int)
    | Add Expr Expr
    
    deriving Show

pure1   = Number (Clean 3)
pure2   = Number (Clean 7)
impure1 = Number (Dirty 5)

expr1 = Add pure1 pure2
expr2 = Add impure1 pure1
expr3 = Add pure1 (Add impure1 pure2) 

--Evaluate expression as much as Possible
evalExpr :: Expr -> Expr
evalExpr (Number n) = Number n
evalExpr (Add e1 e2) = 
    case (evalExpr e1, evalExpr e2) of
        (Number i, Number j) -> Number $ liftM2 (+) i j
        (x, y) -> Add x y

reducedExpr1 = evalExpr expr1
reducedExpr2 = evalExpr expr2
reducedExpr3 = evalExpr expr3
