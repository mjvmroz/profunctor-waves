module Noize.Data.Scale.Diatonic (
  Note (..),
  Octave (..),
  Semitones (..),
  Pitch (..),
  relativeIntervalSemitones,
  transpose,
  applyScaleInterval,
  pitchWavelength,
  simpleScaleInterval,
  ScaleRelation (..),
  IntervalDirection (..),
  IntervalMagnitude (..),
  ScaleInterval (..),
  DiatonicScaleKind (..),
  DiatonicScale (..),
  major,
  major7,
  minor,
  minor7,
  Chord (..),
  renderChord,
  reciprocal,
) where

import Noize.Data.Physics ( Wavelength(..) )
import Data.Group

data Note = C | Db | D | Eb | E | F | Gb | G | Ab | A | Bb | B deriving (Eq, Ord, Show, Enum, Bounded)
newtype Octave = Octave { unOctave :: Int } deriving (Eq, Ord, Show)

newtype Semitones = Semitones { unSemitones :: Int } deriving (Eq, Ord, Show)

instance Semigroup Semitones where
  (<>) (Semitones a) (Semitones b) = Semitones $ a + b

instance Monoid Semitones where
  mempty = Semitones 0

instance Group Semitones where
  invert = Semitones . negate . (.unSemitones)

data Pitch = Pitch
  { note :: Note
  , octave :: Octave
  } deriving (Eq, Ord, Show)

-- FIXME: Octave offsets are bugged
transpose :: Semitones -> Pitch -> Pitch
transpose (Semitones semitones) (Pitch note octave) =
  let
    octaveOffset = semitones `div` 12
    noteOffset = semitones `rem` 12
    newNote = toEnum . (`rem` 12) $ 12 + noteOffset + fromEnum note
    newOctave = Octave $ octaveOffset + octave.unOctave
   in Pitch newNote newOctave

pitchWavelength :: Pitch -> Wavelength
pitchWavelength (Pitch note (Octave octave)) =
 let
    a4Freq :: Double = 440
    octaveOffset = (octave - 4) * 12
    noteOffset = fromEnum note - fromEnum A
    semitonesFromA4 = fromIntegral $ octaveOffset + noteOffset
  in
    Wavelength . toRational $ a4Freq * (2 ** (semitonesFromA4 / 12))

data ScaleRelation =
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
  deriving (Eq, Show, Enum)

reciprocal :: ScaleRelation -> ScaleRelation
reciprocal Tonic = Tonic
reciprocal MinorSecond = Seventh
reciprocal Second = MinorSeventh
reciprocal MinorThird = Sixth
reciprocal Third = MinorSixth
reciprocal Fourth = Fifth
reciprocal DiminishedFifth = DiminishedFifth
reciprocal Fifth = Fourth
reciprocal MinorSixth = Third
reciprocal Sixth = MinorThird
reciprocal MinorSeventh = Second
reciprocal Seventh = MinorSecond

simpleScaleInterval :: ScaleRelation -> ScaleInterval
simpleScaleInterval = ScaleInterval (IntervalMagnitude 1) Up

data IntervalDirection = Up | Down deriving (Eq, Show)

-- TODO: this works, but the enum-int relationship borders on incidental. Might want to just make it explicit.
relativeIntervalSemitones :: ScaleRelation -> Semitones
relativeIntervalSemitones = Semitones . fromEnum

-- | The magnitude of an interval is the number of octaves it spans:
--   - The magnitude of all intervals (i.e. varying on direction and relation dimensions)
--     between some pitch p and itself is 0.
--   - The magnitude of any interval representing the delta between some pitch p and
--     some pitch q is the number of octaves represented by its span in semitones.
--   Valid values are 0 through Infinity, for all the good that will do you.
--   TODO: Make this a newtype with a smart constructor, or replace the int with something safer
newtype IntervalMagnitude = IntervalMagnitude { unIntervalMagnitude :: Int } deriving (Eq, Ord, Show)

data ScaleInterval = ScaleInterval
  { magnitude :: IntervalMagnitude
  , direction :: IntervalDirection
  , relation :: ScaleRelation
  } deriving (Eq, Show)

applyScaleInterval :: ScaleInterval -> Pitch -> Pitch
applyScaleInterval (ScaleInterval (IntervalMagnitude magnitude) direction relation) pitch
  | magnitude == 0 = pitch
  | otherwise = transpose semis pitch
  where
    intervalSemis = relativeIntervalSemitones relation
    magnitudeSemis
      | magnitude == 1 = mempty
      | otherwise = Semitones $ 12 * (magnitude - 1)
    absoluteSemis = intervalSemis <> magnitudeSemis
    semis = case direction of
      Up -> absoluteSemis
      Down -> invert absoluteSemis
  

data DiatonicScaleKind =
  Major
  | Minor
  deriving (Eq, Show)

data DiatonicScale = DiatonicScale
  { kind :: DiatonicScaleKind
  , base :: Note
  }

-- TODO: This should be a set, but I need to define Ord ScaleInterval first
data Chord = Chord { scaleRoot :: Pitch, unChord :: [ScaleInterval] } deriving (Eq, Show)

renderChord :: Chord -> [Pitch]
renderChord (Chord root intervals) =
  applyScaleInterval
  <$> intervals
  <*> [root]

simpleChord :: [ScaleRelation] -> Pitch -> Chord
simpleChord relations root = Chord root $ fmap simpleScaleInterval relations

major :: Pitch -> Chord
major = simpleChord [Tonic, Third, Fifth]

major7 :: Pitch -> Chord
major7 = simpleChord [Tonic, Third, Fifth, Seventh]

minor :: Pitch -> Chord
minor = simpleChord [Tonic, MinorThird, Fifth]

minor7 :: Pitch -> Chord
minor7 = simpleChord [Tonic, MinorThird, Fifth, MinorSeventh]

newtype ChordProgression = ChordProgression { unChordProgression :: [Chord] } deriving (Eq, Show)