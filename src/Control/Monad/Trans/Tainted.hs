-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad
-- Copyright   :  Ross Meikleham
-- License     :  BSD-style 
-- 
-- Maintainer  :  RossMeikleham@hotmail.co.uk
-- Stability   :  provisional
-- Portability :  portable
--
-- The Tainted type, and associated operations.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Tainted(TaintedT(..), hoistTainted)

where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Data.Tainted

-- | 'TaintedT' is a monad transformed of 'Tainted'
newtype TaintedT m a = TaintedT {runTainted :: m (Tainted a)}


instance Monad m => Functor (TaintedT m) where
    fmap f = TaintedT . fmap (fmap f) . runTainted

instance Monad m => Applicative (TaintedT m) where
    pure = return
    (<*>) = ap


instance Monad m => Monad (TaintedT m) where
    return = TaintedT . return . return
    TaintedT x >>= f = TaintedT $ do
        res <- x
        case res of
            Clean v -> runTainted $ f v
            Dirty t -> do
                    -- Ensure Dirty stays Dirty
                    res <- runTainted $ f t
                    return $ case res of
                        Clean u -> Dirty u 
                        d       -> d
                                

instance MonadTrans TaintedT where
    lift = TaintedT . liftM return


-- | Lift a 'Tainted' into an 'TaintedT'
hoistTainted :: Monad m => Tainted a -> TaintedT m a
hoistTainted = TaintedT . return
