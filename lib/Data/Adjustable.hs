module Data.Adjustable (Adjustable (..)) where

class Adjustable a where
  lower :: a -> a
  higher :: a -> a
