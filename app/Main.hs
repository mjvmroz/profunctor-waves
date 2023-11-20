module Main (main) where

import Noize.Audio qualified as Audio
import Noize.Sampler qualified as Sampler
import Noize.Data.Scale.Diatonic

sampler :: Sampler.WeightedSampler
sampler = Sampler.chord Sampler.sineW $ 
  pitchWavelength
  . transpose (Semitones 0)
  . flip Pitch (Octave 3)
  <$> [G, B, D, Gb]

main :: IO ()
main =
  let ctx = Audio.AudioContext 48_000
   in Audio.play $ Sampler.sampleWeighted ctx sampler
