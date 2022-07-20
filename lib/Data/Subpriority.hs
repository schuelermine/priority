{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Subpriority where

import Data.BinaryOffset (BinaryOffset(..))
import Data.Data (Data)
import GHC.Generics (Generic)
import Data.Adjustable (Adjustable(..))
import Control.Arrow (Arrow(second))
import Data.Function (on)

data Subpriority base = Lower (Subpriority base) | Base base | Higher (Subpriority base)
  deriving (Eq, Show, Read, Generic, Data)

lower1 :: base -> Subpriority base
lower1 = Lower . Base

higher1 :: base -> Subpriority base
higher1 = Higher . Base

getBaseOffset :: Subpriority base -> (base, BinaryOffset)
getBaseOffset (Lower a) = second lower $ getBaseOffset a
getBaseOffset (Base a) = (a, BinaryOffset [])
getBaseOffset (Higher a) = second higher $ getBaseOffset a

instance Ord base => Ord (Subpriority base) where
  compare = on compare getBaseOffset

instance Adjustable (Subpriority base) where
  lower = Lower
  higher = Higher
