module WesternArt where

import Sampler

data Note = C | Db | D | Eb | E | F | Gb | G | Ab | A | Bb | B deriving (Eq, Ord, Show, Enum, Bounded)
newtype Octave = Octave Int deriving (Eq, Ord, Show)

data Pitch = Pitch Note Octave deriving (Eq, Ord, Show)

frequency :: Pitch -> Wavelength
frequency (Pitch note (Octave octave)) =
 let
    a4Freq = 440
    octaveOffset = (octave - 4) * 12
    noteOffset = fromEnum note - fromEnum A
    semitonesFromA4 = octaveOffset + noteOffset
  in
    Wavelength $ a4Freq * (2 ** (fromIntegral semitonesFromA4 / 12))

sampler :: WeightedShortSampler
sampler = Sampler.chord Sampler.sineW $ frequency . flip Pitch (Octave 4) <$> [G, B, D]
