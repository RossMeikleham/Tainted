module TaintTransformerExample where

import Data.Tainted
import Control.Monad.Trans.Tainted
import Control.Monad.State

type TaintedState a b = TaintedT (State a) b

doStuff :: Int -> (TaintedState Int Int) -> Tainted Int
doStuff n f = evalState (runTainted f) n
    

f :: TaintedState Int Int
f = do
    x <- lift get
    return $ x + 6


g :: TaintedState Int Int
g = do
    x <- lift get
    y <- hoistTainted $ Dirty 6
    return $ x + y


h :: TaintedState Int Int
h = do
    x <- lift get
    lift $ put (x + 1)
    y <- hoistTainted $ Dirty 7
    z <- lift get
    return $ x + y + z


ex1 = doStuff 5 f 
ex2 = doStuff 5 g 
ex3 = doStuff 5 h

