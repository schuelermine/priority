{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Priority (Priority(..)) where

import Data.Ordering (ifEQ, flipOrdering)
import GHC.Generics (Generic)

data Priority t = Base t | Higher (Priority t) | Lower (Priority t)
    deriving (Eq, Show, Read, Functor, Generic)

instance Ord b => Ord (Priority b) where
    compare (Lower a)  (Lower b)  = compare a b
    compare (Lower a)  (Base b)   = ifEQ LT $ comparePriorityToBase a b
    compare (Lower a)  (Higher b) = ifEQ LT $ compare a b
    compare (Base a)   (Lower b)  = ifEQ GT $ compareBaseToPriority a b
    compare (Base a)   (Base b)   = compare a b
    compare (Base a)   (Higher b) = ifEQ LT $ compareBaseToPriority a b
    compare (Higher a) (Lower b)  = ifEQ GT $ compare a b
    compare (Higher a) (Base b)   = ifEQ GT $ comparePriorityToBase a b
    compare (Higher a) (Higher b) = compare a b

compareBaseToPriority :: Ord a => a -> Priority a -> Ordering
compareBaseToPriority a b = flipOrdering $ comparePriorityToBase b a

comparePriorityToBase :: Ord a => Priority a -> a -> Ordering
comparePriorityToBase (Lower a)  b = ifEQ LT $ comparePriorityToBase a b
comparePriorityToBase (Base a)   b = compare a b
comparePriorityToBase (Higher a) b = ifEQ GT $ comparePriorityToBase a b
