{-# LANGUAGE DeriveFunctor #-}

module Data.Priority (Priority(..), compareBase) where

import Data.Ordering (ifEQ, flipOrdering)

data Priority t = Base t | Higher (Priority t) | Lower (Priority t) deriving (Eq, Show, Read, Functor)

lower :: t -> Priority t
lower a = Lower (Base a)

higher :: t -> Priority t
higher a = Higher (Base a)

instance Ord b => Ord (Priority b) where
    compare (Lower p) (Lower q)   = compare p q
    compare (Lower p) (Base b)    = ifEQ LT (compareBase p b)
    compare (Lower p) (Higher q)  = ifEQ LT (compare p q)
    compare (Base a) (Lower q)    = ifEQ GT (flipOrdering $ compareBase q a)
    compare (Base a) (Base b)     = compare a b
    compare (Base a) (Higher q)   = ifEQ LT (flipOrdering $ compareBase q a) 
    compare (Higher p) (Lower q)  = ifEQ GT (compare p q)
    compare (Higher p) (Base b)   = ifEQ GT (compareBase p b)
    compare (Higher p) (Higher q) = compare p q

compareBase :: Ord t => Priority t -> t -> Ordering
compareBase (Lower p) b  = ifEQ LT (compareBase p b)
compareBase (Base a) b   = compare a b
compareBase (Higher p) b = ifEQ GT (compareBase p b)
