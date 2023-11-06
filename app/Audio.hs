module Audio (AudioContext (..), AudioData (..), play) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Int (Int16)
import qualified Data.Vector.Storable.Mutable as V
import SDL hiding (get)

newtype AudioContext = AudioContext
  { sampleRate :: Int
  }

data AudioData = AudioData
  { context :: AudioContext
  , samples :: [Int16]
  }

audioCB :: IORef [Int16] -> AudioFormat sampleType -> V.IOVector sampleType -> IO ()
audioCB samples format buffer =
  case format of
    Signed16BitLEAudio ->
      do
        samples' <- readIORef samples
        let n = V.length buffer
        zipWithM_
          (V.write buffer)
          [0 ..]
          (take n samples')
        writeIORef
          samples
          (drop n samples')
    _ -> error "Unsupported audio format"

play :: AudioData -> IO ()
play (AudioData audioCtx samples) =
  do
    initializeAll
    mutSamples <- newIORef samples
    (device, _) <-
      openAudioDevice
        OpenDeviceSpec
          { SDL.openDeviceFreq =
              Mandate . fromIntegral $ audioCtx.sampleRate
          , SDL.openDeviceFormat =
              Mandate Signed16BitNativeAudio
          , SDL.openDeviceChannels =
              Mandate Mono
          , SDL.openDeviceSamples = 4096 * 2
          , SDL.openDeviceCallback = audioCB mutSamples
          , SDL.openDeviceUsage = ForPlayback
          , SDL.openDeviceName = Nothing
          }
    setAudioDevicePlaybackState device Play
    forever $ threadDelay maxBound