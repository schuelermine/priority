{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Priority (Priority(..), higher, lower, compareBase) where

import Data.Ordering (ifEQ, flipOrdering)
import GHC.Generics (Generic)

data Priority t = Base t | Higher (Priority t) | Lower (Priority t)
    deriving (Eq, Show, Read, Functor, Generic)

lower :: t -> Priority t
lower a = Lower (Base a)

higher :: t -> Priority t
higher a = Higher (Base a)

instance Ord b => Ord (Priority b) where
    compare   (Lower p)    (Lower q)  = compare p q
    compare x@(Lower p)    (Base b)   = compareBase x b
    compare   (Lower p)    (Higher q) = ifEQ LT $ compare p q
    compare   (Base a)   y@(Lower q)  = flipOrdering $ compareBase y a
    compare   (Base a)     (Base b)   = compare a b
    compare   (Base a)   y@(Higher q) = flipOrdering $ compareBase y a
    compare   (Higher p)   (Lower q)  = ifEQ GT $ compare p q
    compare x@(Higher p)   (Base b)   = compareBase x b
    compare   (Higher p)   (Higher q) = compare p q

compareBase :: Ord t => Priority t -> t -> Ordering
compareBase (Lower p)  b = ifEQ LT (compareBase p b)
compareBase (Base a)   b = compare a b
compareBase (Higher p) b = ifEQ GT (compareBase p b)
