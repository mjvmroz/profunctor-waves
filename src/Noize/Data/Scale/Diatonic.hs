module Noize.Data.Scale.Diatonic (
  Note (..),
  Octave (..),
  Semitones (..),
  Pitch (..),
  transpose,
  pitchWavelength,
  DiatonicPitch (..),
  DiatonicScaleKind (..),
  DiatonicScale (..)
) where

import Noize.Data.Physics ( Wavelength(..) )

data Note = C | Db | D | Eb | E | F | Gb | G | Ab | A | Bb | B deriving (Eq, Ord, Show, Enum, Bounded)
newtype Octave = Octave { unOctave :: Int } deriving (Eq, Ord, Show)

newtype Semitones = Semitones { unSemitones :: Int } deriving (Eq, Ord, Show)

data Pitch = Pitch
  { note :: Note
  , octave :: Octave
  } deriving (Eq, Ord, Show)

transpose :: Semitones -> Pitch -> Pitch
transpose (Semitones semitones) (Pitch note octave) =
  let
    octaveOffset = semitones `div` 12
    noteOffset = semitones `rem` 12
    newNote = toEnum . flip mod 12 $ noteOffset + fromEnum note
    newOctave = Octave $ octaveOffset + octave.unOctave
   in Pitch newNote newOctave

pitchWavelength :: Pitch -> Wavelength
pitchWavelength (Pitch note (Octave octave)) =
 let
    a4Freq = 440
    octaveOffset = (octave - 4) * 12
    noteOffset = fromEnum note - fromEnum A
    semitonesFromA4 = fromIntegral $ octaveOffset + noteOffset
  in
    Wavelength . toRational $ a4Freq * (2 ** (semitonesFromA4 / 12))

data DiatonicPitch =
  Tonic
  | MinorSecond
  | Second
  | MinorThird
  | Third
  | Fourth
  | DiminishedFifth
  | Fifth
  | MinorSixth
  | Sixth
  | MinorSeventh
  | Seventh
  deriving (Eq, Show)

data DiatonicScaleKind =
  Major
  | Minor
  deriving (Eq, Show)

data DiatonicScale = DiatonicScale
  { kind :: DiatonicScaleKind
  , base :: Note
  }