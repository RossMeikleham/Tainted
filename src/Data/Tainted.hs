
{-# LANGUAGE DeriveDataTypeable #-}
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

module Data.Tainted 
    (Tainted(..), isClean, isDirty, cleans, dirtys, partitionTaints)
where

import Control.Monad
import Control.Applicative
import Data.Typeable


-- | The 'Tainted' type encapsulates a value.  A value of type
-- @'Tainted' a@ either contains a "clean" value of type @a@ (represented as @'Clean' a@), 
-- or it contains a "dirty" value of type @a@ (represented as @'Dirty' a@).  
--
-- The 'Tainted' type is also a monad. Once the "dirty" state has been
-- reached, and clean operations performed themselves create a "dirty"
-- value. 
data Tainted a = Dirty a | Clean a
    deriving (Eq, Ord, Read, Show, Typeable)

-- Identity law:
--
-- fmap id (Dirty a) = Dirty (id a) = Dirty a => fmap id = id 
-- 
-- Same proof can be generalized for Clean
--
-- Composition law:
--
-- ((fmap p) . (fmap q)) (Dirty a) = fmap p (Dirty (q a) = (Dirty ((p . q) a)) 
-- = fmap (p . q)
--
-- Same proof can be generalized for Clean
instance Functor Tainted where
    fmap f (Dirty a) = Dirty (f a)
    fmap f (Clean a) = Clean (f a)


-- Proof of correctness comes from Monad correctness
instance Applicative Tainted where
    pure = return
    (<*>) = ap
 
-- Left Identity Law:
--
-- (return x) >>= f = Clean f x  ≡ f x
--
-- Right Identity Law:
--
-- m >>= return ≡ m 
--
-- proof is in definition of the Clean case below for (>>=)
--
-- Associativity Law:
--
-- Case of m being Clean :
-- 
-- LHS: (Clean a >>= f) >>= g = f a >>= g 
-- RHS: Clean a >>= (\x -> f x >>= g) =  f a >>= g
-- LHS = RHS
--
-- Case of m being Dirty:
--
-- LHS: (Dirty a >>= f) >>= g = 
--      g =<< case f a of 
--          (Clean y) -> Dirty y
--          y         -> y
--
-- RHS: Dirty a >>= (\x -> f x >>= g) = 
--      g =<< case f a of
--          (Clean y) -> Dirty y
--          y         -> y    
-- LHS = RHS
instance Monad Tainted where
    return = Clean
    Dirty x  >>= f = case f x of
                        (Clean y) -> Dirty y
                        y -> y 
    Clean x  >>= f = f x


-- ---------------------------------------------------------------------------
-- The Tainted type, and instances


-- | Returns 'True' iff its argument is of the form 'Clean _.
isClean :: Tainted a -> Bool
isClean (Clean _) = True
isClean _ = False


-- | Returns 'True' iff its argument is of the form Dirty _.  
isDirty :: Tainted a -> Bool 
isDirty = not . isClean 


-- | Extracts from a list of 'Tainted' all the 'Clean' elements. 
--   All the 'Clean' elements are extracted in order.
cleans :: [Tainted a] -> [a]
cleans = map extractTaint . filter isClean 


-- | Extracts from a list of 'Tainted' all the 'Dirty' elements.
--   All the 'Dirty' elements are extracted in order.
dirtys :: [Tainted a] -> [a]
dirtys = map extractTaint . filter isDirty


-- | Partitions a list of 'Tainted' into two lists. 
--   All the 'Dirty' elements are extracted, in order, to the first component of the output. 
--   Similarly the 'Clean' elements are extracted to the second component of the output.
partitionTaints :: [Tainted a] -> ([a], [a])
partitionTaints ts = (c, d)
    where c = cleans  ts
          d = dirtys  ts


-- | Extract the value contained in a 'Tainted' type
extractTaint :: Tainted a -> a
extractTaint (Clean a) = a
extractTaint (Dirty a) = a
