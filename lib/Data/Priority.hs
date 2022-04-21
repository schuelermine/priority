{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Data.Priority (Priority (..)) where

import Data.Bifunctor (second)
import Data.Ordering (flipOrdering, ifEQ)
import GHC.Generics (Generic)
import Data.Coerce

class Adjustable t where
  higher :: t -> t
  lower :: t -> t

data Priority t = Lower (Priority t) | Base t | Higher (Priority t)
  deriving (Show, Read, Functor, Generic)

instance Adjustable (Priority t) where
  higher = Higher
  lower = Lower

data PriorityChange = LowerChange | HigherChange deriving (Eq, Ord)

changeToOrdering :: PriorityChange -> Ordering
changeToOrdering LowerChange = LT
changeToOrdering HigherChange = GT

newtype ChangeList = ChangeList {getCL :: [PriorityChange]}

instance Adjustable ChangeList where
  higher (getCL -> xs) = ChangeList (HigherChange : xs)
  lower (getCL -> xs) = ChangeList (LowerChange : xs)

toChangeList :: Priority t -> (t, ChangeList)
toChangeList (Lower a) = second lower $ toChangeList a
toChangeList (Base a) = (a, ChangeList [])
toChangeList (Higher a) = second higher $ toChangeList a

compareCL :: [PriorityChange] -> [PriorityChange] -> Ordering
compareCL [] [] = EQ
compareCL [] (y:_) = flipOrdering $ changeToOrdering y
compareCL (x:_) [] = changeToOrdering x
compareCL (x:xs) (y:ys) = ifEQ (compareCL xs ys) $ compare x y

instance Ord ChangeList where
  compare x y = compareCL (getCL x) (getCL y)

instance Eq ChangeList where
  x == y = compare x y == EQ

instance Ord t => Ord (Priority t) where
  compare a b = compare (toChangeList a) (toChangeList b)

instance Eq t => Eq (Priority t) where
  a == b = toChangeList a == toChangeList b
