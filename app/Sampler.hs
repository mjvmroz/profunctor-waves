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

newtype Wavelength = Wavelength Double
  deriving Show

sine :: Wavelength -> ShortSampler
sine (Wavelength freq) =
  do
    ctx <- ask
    return . round $ fromIntegral (maxBound `div` 2 :: Int16) * sin (2 * pi * freq * ctx.time)

sineW :: Wavelength -> WeightedShortSampler
sineW = weighted 1 . sine

square :: Wavelength -> ShortSampler
square (Wavelength freq) =
  do
    ctx <- ask
    return $ if sin (2 * pi * freq * ctx.time) > 0 then maxBound else minBound

squareW :: Wavelength -> WeightedShortSampler
squareW = weighted 1 . square

sawtooth :: Wavelength -> ShortSampler
sawtooth (Wavelength freq) =
  do
    ctx <- ask
    return . round $ fromIntegral (maxBound @Int16) * (ctx.time * freq - fromIntegral @Int16 (floor (ctx.time * freq)))

sawtoothW :: Wavelength -> WeightedShortSampler
sawtoothW = weighted 1 . sawtooth

triangle :: Wavelength -> ShortSampler
triangle (Wavelength freq) =
  do
    ctx <- ask
    return . round $ fromIntegral (maxBound @Int16) * (abs (ctx.time * freq - fromIntegral @Int16 (floor (ctx.time * freq))) * 2 - 1)

triangleW :: Wavelength -> WeightedShortSampler
triangleW = weighted 1 . triangle

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
  (WeightedShortSampler w1 s1) <> (WeightedShortSampler w2 s2) =
    let
      combinedSampler = do
        v1 <- s1
        v2 <- s2
        return . round $ fromIntegral v1 * w1 / (w1 + w2) + fromIntegral v2 * w2 / (w1 + w2)
    in WeightedShortSampler (w1 + w2) combinedSampler

withHarmonics :: (Wavelength -> ShortSampler) -> Wavelength -> WeightedShortSampler
withHarmonics mkWave (Wavelength fundamental) = mconcat $ map weightedHarmonic [1..5]
  where
    weightedHarmonic :: Int -> WeightedShortSampler
    weightedHarmonic harmonic = 
      let harmonicD = fromIntegral harmonic
       in weighted (1 / harmonicD) . mkWave $ Wavelength (fundamental * harmonicD)

chord :: (Wavelength -> WeightedShortSampler) -> [Wavelength] -> WeightedShortSampler
chord mkWave fundamentals = mconcat $ mkWave <$> fundamentals

-- TODO: Implement this. I feel like I need to add some mechanism for normalizing weights?
-- weightedChord :: (Wavelength -> WeightedShortSampler) -> [(Wavelength, Double)] -> WeightedShortSampler
-- weightedChord mkWave fundamentalWeights = 