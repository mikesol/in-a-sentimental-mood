module Klank.IASM.Middle where

import Prelude
import Control.Promise (toAffE)
import Data.Array (filter, foldl)
import Data.Array (fold, head, last, range, span)
import Data.Int (toNumber)
import Data.Lens (_1, _2, over, traversed)
import Data.List ((:), List(..))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (class Pos, D1, D2)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, EngineInfo, allpassT_, bandpassT_, decodeAudioDataFromUri, gain', gainT', gainT_', gain_', highpassT_, pannerMonoT_, pannerMono_, panner_, playBuf, playBufT_, playBufWithOffset_, playBuf_, runInBrowser, sinOsc, sinOsc_, speaker, speaker')
import Foreign.Object as O
import Math (cos, pi, pow, sin)
import Test.QuickCheck.Gen (sample)
import Type.Klank.Dev (Buffers, Klank, affable, klank, makeBuffersKeepingCache)

iasmEngineInfo =
  { msBetweenSamples: 100
  , msBetweenPings: 95
  , fastforwardLowerBound: 0.025
  , rewindUpperBound: 2.0
  , initialOffset: 0.5
  , doWebAudio: true
  } ::
    EngineInfo

soundsFull =
  [ Tuple "guitar" 100.0
  , Tuple "voice" 100.0
  ] ::
    Array (Tuple String Number)

fromSoundsFull :: String -> Number
fromSoundsFull i = fromMaybe 0.0 (M.lookup i soundsFullMap)

soundsFullMap :: M.Map String Number
soundsFullMap = M.fromFoldable soundsFull

soundsLicks =
  [] ::
    Array (Tuple String Number)

fromSoundsLicks :: String -> Number
fromSoundsLicks i = fromMaybe 0.0 (M.lookup i soundsLicksMap)

soundsLicksMap :: M.Map String Number
soundsLicksMap = M.fromFoldable soundsLicks

soundsBridge1 =
  [] ::
    Array (Tuple String Number)

fromSoundsBridge1 :: String -> Number
fromSoundsBridge1 i = fromMaybe 0.0 (M.lookup i soundsBridge1Map)

soundsBridge1Map :: M.Map String Number
soundsBridge1Map = M.fromFoldable soundsBridge1

soundsBridge2 =
  [] ::
    Array (Tuple String Number)

fromSoundsBridge2 :: String -> Number
fromSoundsBridge2 i = fromMaybe 0.0 (M.lookup i soundsBridge2Map)

soundsBridge2Map :: M.Map String Number
soundsBridge2Map = M.fromFoldable soundsBridge2

kr = (toNumber iasmEngineInfo.msBetweenSamples) / 1000.0 :: Number

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter Number
epwf p s =
  let
    ht = span ((s >= _) <<< fst) p

    left = fromMaybe (Tuple 0.0 0.0) $ last ht.init

    right =
      fromMaybe
        (maybe (Tuple 10000.0 0.0) (over _1 (_ + 1.0)) $ last p)
        $ head ht.rest
  in
    if (fst right - s) < kr then
      AudioParameter
        { param: (snd right)
        , timeOffset: (fst right - s)
        }
    else
      let
        m = (snd right - snd left) / (fst right - fst left)

        b = (snd right - (m * fst right))
      in
        AudioParameter { param: (m * s + b), timeOffset: 0.0 }

fromCloud :: String -> String
fromCloud s = "https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/" <> s

-- 0 3 5 7 11 14
main :: Klank
main =
  klank
    { buffers =
      makeBuffersKeepingCache
        ( over (traversed <<< _2) fromCloud
            ( ( map
                  ( \i ->
                      let
                        s = fst i
                      in
                        Tuple
                          ("Full-" <> s)
                          ("Full/" <> s <> ".ogg")
                  )
                  soundsFull
              )
                <> ( map
                      ( \i ->
                          let
                            s = fst i
                          in
                            Tuple
                              ("Licks-" <> s <> "-l")
                              ("Licks/" <> s <> ".l.ogg")
                      )
                      soundsLicks
                  )
                <> ( map
                      ( \i ->
                          let
                            s = fst i
                          in
                            Tuple
                              ("Bridge1-" <> s <> "-l")
                              ("Bridge1/" <> s <> ".l.ogg")
                      )
                      soundsBridge1
                  )
                <> ( map
                      ( \i ->
                          let
                            s = fst i
                          in
                            Tuple
                              ("Bridge2-" <> s <> "-l")
                              ("Bridge2/" <> s <> ".l.ogg")
                      )
                      soundsBridge2
                  )
            )
        )
    , run = runInBrowser scene
    , engineInfo = iasmEngineInfo
    }

----
-- util
--
atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

------------------------------
-----------------------------
--------------------------
-- In
type PlayerInOpts
  = { tag :: String
    , pan :: Number -> AudioParameter Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

playerGuitar :: String -> String -> Number -> List (AudioUnit D2)
playerGuitar tag name time =
  let
    len = fromMaybe 0.0 (M.lookup name soundsFullMap)
  in
    if time + kr >= 0.0 && time < len then
      pure
        $ panner_ (tag <> "_panGuitar") 0.0
            ( gainT_' (tag <> "_gainGuitar")
                ((epwf [ Tuple 0.0 1.0, Tuple len 1.0 ]) time)
                (playBufWithOffset_ (tag <> "_playerGuitar") ("Full-" <> name) 1.0 0.0)
            )
    else
      Nil

playerVoice :: String -> String -> Number -> List (AudioUnit D2)
playerVoice tag name time =
  let
    len = fromMaybe 0.0 (M.lookup name soundsFullMap)
  in
    if time + kr >= 0.0 && time < len then
      pure
        $ panner_ (tag <> "_panVoice") 0.0
            ( gainT_' (tag <> "_gainVoice")
                ((epwf [ Tuple 0.0 1.0, Tuple len 1.0 ]) time)
                (playBufWithOffset_ (tag <> "_playerVoice") ("Full-" <> name) 1.0 0.0)
            )
    else
      Nil

conv440 :: Int -> Number
conv440 i = 440.0 * (2.0 `pow` ((toNumber $ 0 + i) / 12.0))

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( zero
            :| fold
                ( map ((#) time)
                    ( [ atT 0.0 $ playerGuitar "Guitar" "guitar" ]
                        <> [ atT 0.0 $ playerVoice "Voice" "voice" ]
                    )
                )
        )
