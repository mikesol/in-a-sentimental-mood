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
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, EngineInfo, allpassT_, bandpassT_, decodeAudioDataFromUri, dynamicsCompressor_, gain', gainT', gainT_, gainT_', gain_', highpassT_, highpass_, pannerMonoT_, pannerMono_, panner_, playBuf, playBufT_, playBufWithOffset_, playBuf_, runInBrowser, sinOsc, sinOsc_, speaker, speaker')
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

data MoodIdx
  = MoodIdx (Tuple String Int) Number

soundsMooRoo =
  [ MoodIdx (Tuple "B5" 6) 5.642448979591837
  , MoodIdx (Tuple "A5" 1) 6.826666666666667
  , MoodIdx (Tuple "F#5" 2) 5.143219954648526
  ] ::
    Array MoodIdx

fromSoundsMooRoo :: String -> Int -> Number
fromSoundsMooRoo s i = fromMaybe 0.0 (M.lookup (s <> "-" <> show i) soundsMooRooMap)

soundsMooRooMap :: M.Map String Number
soundsMooRooMap = M.fromFoldable (map (\(MoodIdx (Tuple x y) b) -> Tuple (x <> "-" <> show y) b) soundsMooRoo)

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
                <> ( map
                      ( \(MoodIdx (Tuple pitch n) _) ->
                          let
                            s = show n
                          in
                            Tuple
                              ("Mood-" <> pitch <> "-" <> s <> "-l")
                              ("Mood/" <> (replace (Pattern "#") (Replacement "%23") pitch) <> "/" <> s <> ".l.ogg")
                      )
                      soundsMooRoo
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

playerGuitar :: String -> String -> Number -> Number -> List (AudioUnit D2)
playerGuitar tag name tos time =
  let
    len = fromMaybe 0.0 (M.lookup name soundsFullMap)
  in
    if time + kr >= 0.0 && time < len then
      pure
        $ panner_ (tag <> "_panGuitar") 0.0
            ( gainT_' (tag <> "_gainGuitar")
                ((epwf [ Tuple 0.0 1.0, Tuple len 1.0 ]) time)
                ( highpass_ "_highpassGuitar" 150.0 1.0
                    (playBufWithOffset_ (tag <> "_playerGuitar") ("Full-" <> name) 1.0 tos)
                )
            )
    else
      Nil

playerHigh :: String -> String -> Number -> Number -> List (AudioUnit D2)
playerHigh tag name dur time =
  if time + kr >= 0.0 && time < dur + 0.5 then
    pure
      $ gainT_' (tag <> "_gainHigh")
          ( ( epwf
                [ Tuple 0.0 1.0
                , Tuple (dur - 0.25) 1.0
                , Tuple dur 0.0
                ]
            )
              time
          )
          ( highpass_ (tag <> "_hpfHigh")
              700.0
              1.0
              (playBufWithOffset_ (tag <> "_playerHigh") name 1.0 0.0)
          )
  else
    Nil

playerRhythm :: String -> Number -> (Number -> AudioParameter Number) -> (Number -> List (AudioUnit D2)) -> Number -> List (AudioUnit D2)
playerRhythm tag len gf aus time =
  if time + kr >= 0.0 && time < len then
    pure (gainT_ (tag <> "_playerRhythm") (gf time) (zero :| (aus time)))
  else
    Nil

highBRhythm_ = (foldl (\{ acc, dur } i -> { acc: acc <> [ Tuple (0.0 + dur) 0.1, Tuple ((i / 2.0) + dur) 1.2 ], dur: dur + i }) { acc: [ Tuple 0.0 0.0 ], dur: 0.1 } [ 0.5, 0.5, 0.5, 0.6, 0.7, 0.9, 1.0, 1.2 ]) :: { acc :: Array (Tuple Number Number), dur :: Number }

highBRhythm = (highBRhythm_.acc <> [ Tuple highBRhythm_.dur 0.0 ]) :: Array (Tuple Number Number)

highB :: String -> Number -> List (AudioUnit D2)
highB tag =
  playerRhythm
    (tag <> "ryt_")
    (highBRhythm_.dur + 1.0)
    (epwf highBRhythm)
    ( playerHigh (tag <> "ply_") "Mood-B5-6-l" 5.642448979591837
    )

playerVoice :: String -> String -> Number -> Number -> List (AudioUnit D2)
playerVoice tag name tos time =
  let
    len = fromMaybe 0.0 (M.lookup name soundsFullMap)
  in
    if time + kr >= 0.0 && time < len then
      pure
        $ panner_ (tag <> "_panVoice") 0.0
            ( gainT_' (tag <> "_gainVoice")
                ((epwf [ Tuple 0.0 1.0, Tuple len 1.0 ]) time)
                ( dynamicsCompressor_ "_compressorVoice" (-24.0) (30.0) (7.0) (0.003) (0.25)
                    ( highpass_ "_highpassVoice" 150.0 1.0
                        (playBufWithOffset_ (tag <> "_playerVoice") ("Full-" <> name) 1.0 tos)
                    )
                )
            )
    else
      Nil

conv440 :: Int -> Number
conv440 i = 440.0 * (2.0 `pow` ((toNumber $ 0 + i) / 12.0))

startAt = 10.0 :: Number

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( zero
            :| fold
                ( map ((#) time)
                    ( [ atT 0.0 $ playerVoice "Voice" "voice" startAt ]
                        <> [ atT 0.0 $ playerGuitar "Guitar" "guitar" startAt ]
                        <> ( map (atT (-1.0 * startAt))
                              [ atT 12.22 $ highB "hb1", atT 13.20 $ highB "hb2" ]
                          )
                    )
                )
        )
