module Main (main) where

import Audio qualified
import Sampler qualified
import WesternArt qualified

main :: IO ()
main =
  let ctx = Audio.AudioContext 48_000
   in Audio.play $ Sampler.sampleWeighted ctx WesternArt.sampler
