module Main (main) where

import Noize.Audio qualified as Audio
import Noize.Sampler qualified as Sampler
import Noize.Data.Scale.Diatonic

chord :: Chord
chord = major $ Pitch C (Octave 3)

sampler :: Sampler.WeightedSampler
sampler = Sampler.chord Sampler.sineW . fmap pitchWavelength . renderChord $ chord

main :: IO ()
main =
  let ctx = Audio.AudioContext 48_000
   in do
    () <- print . renderChord $ chord
    Audio.play $ Sampler.sampleWeighted ctx sampler
