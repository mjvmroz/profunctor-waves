module Noize.Sampler where

import Noize.Audio
import Noize.Data.Sample (Sample (..))
import Noize.Data.Physics (Wavelength (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Int (Int16)

data SampleContext = SampleContext
  { rate :: Integer
  , index :: Integer
  , time :: Rational
  }

mkSampleContext :: AudioContext -> SampleContext
mkSampleContext ctx = SampleContext ctx.sampleRate 0 0

type Sampler = Reader SampleContext Sample

sample :: AudioContext -> Sampler -> [Sample]
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

sampleWeighted :: AudioContext -> WeightedSampler -> AudioData
sampleWeighted audioCtx weightedSampler =
  let samples = sample audioCtx weightedSampler.sampler
   in AudioData audioCtx samples

----------------
-- Generators --

sine :: Wavelength -> Sampler
sine (Wavelength freq) =
  do
    ctx <- ask
    let weight = fromIntegral $ maxBound @Int16
        radians = 2 * pi * fromRational (freq * ctx.time)
    return . Sample . sin $ radians

sineW :: Wavelength -> WeightedSampler
sineW = unitWeight . sine

-- square :: Wavelength -> Sampler
-- square (Wavelength freq) =
--   do
--     ctx <- ask
--     return $ if sin (2 * pi * (fromRational $ freq * ctx.time)) > 0
--       then maxBound
--       else minBound

-- squareW :: Wavelength -> WeightedSampler
-- squareW = unitWeight . square

-- sawtooth :: Wavelength -> Sampler
-- sawtooth (Wavelength freq) =
--   do
--     ctx <- ask
--     return . Sample $ fromIntegral (maxBound @Int16) * (ctx.time * freq - fromIntegral @Int16 (floor (ctx.time * freq)))

-- sawtoothW :: Wavelength -> WeightedSampler
-- sawtoothW = unitWeight . sawtooth

-- triangle :: Wavelength -> Sampler
-- triangle (Wavelength freq) =
--   do
--     ctx <- ask
--     return . Sample . round $ fromIntegral (maxBound @Int16) * (abs (ctx.time * freq - fromIntegral @Int16 (floor (ctx.time * freq))) * 2 - 1)

-- triangleW :: Wavelength -> WeightedSampler
-- triangleW = unitWeight . triangle

-----------------
-- Combinators --

-- | Nominal, relative weight for a Sample(r)
newtype SampleWeight = SampleWeight { unSampleWeight :: Rational } deriving (Eq, Ord, Show)

instance Semigroup SampleWeight where
  (<>) :: SampleWeight -> SampleWeight -> SampleWeight
  SampleWeight w1 <> SampleWeight w2 = SampleWeight $ w1 + w2

instance Monoid SampleWeight where
  mempty :: SampleWeight
  mempty = SampleWeight 0

unitWeight :: Sampler -> WeightedSampler
unitWeight = weighted $ SampleWeight 1

data WeightedSampler = WeightedSampler
  { weight :: SampleWeight
  , sampler :: Sampler
  }

instance Monoid WeightedSampler where
  mempty :: WeightedSampler
  mempty = WeightedSampler mempty $ return (Sample 0)

data WeightedSample = WeightedSample
  { weight :: SampleWeight
  , sample :: Sample
  }

weighted :: SampleWeight -> Sampler -> WeightedSampler
weighted = WeightedSampler

instance Semigroup WeightedSampler where
  (<>) :: WeightedSampler -> WeightedSampler -> WeightedSampler
  (WeightedSampler w1 s1) <> (WeightedSampler w2 s2) =
    let
      totalWeight :: SampleWeight
      totalWeight = w1 <> w2

      weightShort :: SampleWeight -> Sample -> Double
      weightShort (SampleWeight w) (Sample s) = s * fromRational w

      normalize :: Double -> Sample
      normalize v = Sample $ v / fromRational totalWeight.unSampleWeight

      combinedSampler = do
        v1 <- s1
        v2 <- s2
        return . normalize $ weightShort w1 v1 + weightShort w2 v2
    in WeightedSampler totalWeight combinedSampler

withHarmonics :: (Wavelength -> Sampler) -> Wavelength -> WeightedSampler
withHarmonics mkWave (Wavelength fundamental) = mconcat $ map weightedHarmonic [1..5]
  where
    weightedHarmonic :: Int -> WeightedSampler
    weightedHarmonic harmonic =
      let harmonicD = fromIntegral harmonic
       in weighted (SampleWeight $ 1 / harmonicD) . mkWave $ Wavelength (fundamental * harmonicD)

chord :: (Wavelength -> WeightedSampler) -> [Wavelength] -> WeightedSampler
chord mkWave fundamentals = mconcat $ mkWave <$> fundamentals

-- TODO: Implement this. I feel like I need to add some mechanism for normalizing weights?
-- weightedChord :: (Wavelength -> WeightedSampler) -> [(Wavelength, Double)] -> WeightedSampler
-- weightedChord mkWave fundamentalWeights = 