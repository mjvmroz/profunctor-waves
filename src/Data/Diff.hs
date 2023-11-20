module Data.Diff (Diff (..)) where

class Diff a d | a -> d where
  diff :: a -> a -> d
  applyDiff :: d -> a -> a
