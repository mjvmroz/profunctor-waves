{-# LANGUAGE InstanceSigs #-}
module Sampler where

import Audio
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Int (Int16)

data SampleContext = SampleContext
  { rate :: Int
  , index :: Int
  , time :: Double
  }

mkSampleContext :: AudioContext -> SampleContext
mkSampleContext ctx = SampleContext ctx.sampleRate 0 0

type Sampler a = Reader SampleContext a

type ShortSampler = Sampler Int16

sample :: AudioContext -> Sampler a -> [a]
sample audioCtx sampler = evalState steps initSampleContext
 where
  initSampleContext = mkSampleContext audioCtx
  steps = sequence $ repeat step
  step = do
    ctx <- get
    let value = runReader sampler ctx
        newIndex = ctx.index + 1
        newTime = fromIntegral newIndex / fromIntegral ctx.rate
    put $ ctx
      { index = newIndex
      , time = newTime
      }
    return value

sampleWeighted :: AudioContext -> WeightedShortSampler -> AudioData
sampleWeighted audioCtx weightedSampler =
  let samples = sample audioCtx weightedSampler.sampler
   in AudioData audioCtx samples

----------------
-- Generators --

sine :: Double -> ShortSampler
sine freq =
  do
    ctx <- ask
    return . round $ fromIntegral (maxBound `div` 2 :: Int16) * sin (2 * pi * freq * ctx.time)

sineW :: Double -> WeightedShortSampler
sineW freq = weighted 1 $ sine freq

square :: Double -> ShortSampler
square freq =
  do
    ctx <- ask
    return $ if sin (2 * pi * freq * ctx.time) > 0 then maxBound else minBound

squareW :: Double -> WeightedShortSampler
squareW freq = weighted 1 $ square freq

sawtooth :: Double -> ShortSampler
sawtooth freq =
  do
    ctx <- ask
    return . round $ fromIntegral (maxBound @Int16) * (ctx.time * freq - fromIntegral @Int16 (floor (ctx.time * freq)))

sawtoothW :: Double -> WeightedShortSampler
sawtoothW freq = weighted 1 $ sawtooth freq

triangle :: Double -> ShortSampler
triangle freq =
  do
    ctx <- ask
    return . round $ fromIntegral (maxBound @Int16) * (abs (ctx.time * freq - fromIntegral @Int16 (floor (ctx.time * freq))) * 2 - 1)

triangleW :: Double -> WeightedShortSampler
triangleW freq = weighted 1 $ triangle freq

-----------------
-- Combinators --

data WeightedShortSampler = WeightedShortSampler
  { weight :: Double
  , sampler :: ShortSampler
  }

instance Monoid WeightedShortSampler where
  mempty :: WeightedShortSampler
  mempty = WeightedShortSampler 0 (return 0)

weighted :: Double -> ShortSampler -> WeightedShortSampler
weighted = WeightedShortSampler

instance Semigroup WeightedShortSampler where
  (WeightedShortSampler w1 s1) <> (WeightedShortSampler w2 s2) = WeightedShortSampler (w1 + w2) combinedSampler
    where
      combinedSampler = do
        v1 <- s1
        v2 <- s2
        return . round $ fromIntegral v1 * w1 / (w1 + w2) + fromIntegral v2 * w2 / (w1 + w2)

withHarmonics :: (Double -> ShortSampler) -> Double -> WeightedShortSampler
withHarmonics mkWave fundamental = mconcat $ map weightedHarmonic [1..5]
  where
    weightedHarmonic :: Int -> WeightedShortSampler
    weightedHarmonic harmonic = 
      let harmonicD = fromIntegral harmonic
       in weighted (1 / harmonicD) $ mkWave (fundamental * harmonicD)

chord :: (Double -> WeightedShortSampler) -> [Double] -> WeightedShortSampler
chord mkWave fundamentals = mconcat $ mkWave <$> fundamentals