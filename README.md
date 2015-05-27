# Tainted
[![travis](https://travis-ci.org/RossMeikleham/Tainted.svg?branch=master)](https://travis-ci.org/RossMeikleham/Tainted) [![hackage](https://img.shields.io/badge/Hackage-v0.1.0.2-orange.svg)](http://hackage.haskell.org/package/Tainted)

Tainted type, and associated operations 

A Tainted type contains either a clean or dirty value. Values which are
clean stay clean as long as an operation performed on them results
in a clean value. If combined with a dirty value, the value becomes
tainted as dirty and remains that way through further operations.
This is similar to the Maybe monad except once a dirty state has been
reached, calculations can still be performed on the value contained within.
   
This package contains implementations of the Tainted Monad,
TaintedT (the Tainted Monad Transformer), and some examples.


One use case is evaluating whether expressions are pure from multiple
sources combining impure and pure values. This can be useful for
type checking to enforce purity in certain areas. As soon as an 
impure part of an expression is reached it taints the entire
expression as impure.

A simple example given here is a expression evaluator which is given
values from different sources which are marked as pure or impure.

```Haskell
module TaintExample where

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
```

Evaluating expr1:
```Haskell
Number (Clean 10)
```
Adding 2 clean values 7 and 3 gives a clean value, clean
values haven't become tainted

Evaluating expr2:
```Haskell
Number (Dirty 8)
```
Adding a clean value 3 and dirty value 5 taints the expression as dirty
so the expression evaluates to dirty value of 8


Evaluating expr3:
```Haskell
Number (Dirty 15)
```
This shows the propogation of dirty states, as the inner expression
evaluates to a dirty value, then added with a clean value still
gives a dirty value.


