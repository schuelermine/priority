{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.BinaryOffset (BinaryOffset(..), Direction) where

import Data.Adjustable (Adjustable (..))
import Data.Data (Data)
import Data.Ix (Ix)
import GHC.Generics (Generic)

newtype BinaryOffset = BinaryOffset {getDirectionList :: [Direction]} deriving (Eq, Ord, Read, Show, Generic, Data)

data Direction = Lower | Higher deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show, Generic, Data)

onDirectionList :: ([Direction] -> [Direction]) -> BinaryOffset -> BinaryOffset
onDirectionList f = BinaryOffset . f . getDirectionList

instance Adjustable BinaryOffset where
  higher = onDirectionList (Higher :)
  lower = onDirectionList (Lower :)
