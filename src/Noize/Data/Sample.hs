module Noize.Data.Sample (Sample (..)) where

-- | A sample is a value between -1 and 1
newtype Sample = Sample { unSample :: Double } deriving (Eq, Ord, Show)

instance Bounded Sample where
  minBound :: Sample
  minBound = Sample $ -1

  maxBound :: Sample
  maxBound = Sample 1