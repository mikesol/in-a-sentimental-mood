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
import Debug.Trace (spy)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, EngineInfo, allpassT_, bandpassT_, decodeAudioDataFromUri, dynamicsCompressor_, gain', gainT', gainT_, gainT_', gain_', highpassT_, highpass_, pannerMonoT_, pannerMono_, panner_, playBuf, playBufT_, playBufWithOffset_, playBuf_, runInBrowser, sinOsc, sinOsc_, speaker, speaker')
import Foreign.Object as O
import Math (cos, pi, pow, sin, abs)
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

soundsHarm =
  [ Tuple "f-sharp" 8.0
  , Tuple "c-sharp" 8.0
  ] ::
    Array (Tuple String Number)

fromSoundsHarm :: String -> Number
fromSoundsHarm i = fromMaybe 0.0 (M.lookup i soundsHarmMap)

soundsHarmMap :: M.Map String Number
soundsHarmMap = M.fromFoldable soundsHarm

soundsLicks =
  [ Tuple "onTheWingsOfEveryKiss" 6.060408163265306
  , Tuple "onTheWingsOfEveryKiss1" 5.944308390022676
  , Tuple "onTheWingsOfEveryKiss2" 6.8382766439909295
  , Tuple "onTheWingsOfEveryKiss3" 6.095238095238095
  , Tuple "onTheWingsOfEveryKiss4" 6.623492063492064
  , Tuple "onTheWingsOfEveryKiss5" 5.996553287981859
  , Tuple "onTheWingsOfEveryKiss6" 6.182312925170068
  , Tuple "onTheWingsOfEveryKiss7" 5.915283446712018
  , Tuple "onTheWingsOfEveryKiss8" 6.362267573696145
  ] ::
    Array (Tuple String Number)

fromSoundsLicks :: String -> Number
fromSoundsLicks i = fromMaybe 0.0 (M.lookup i soundsLicksMap)

soundsLicksMap :: M.Map String Number
soundsLicksMap = M.fromFoldable soundsLicks

soundsLights =
  [ Tuple "b0" 3.0
  , Tuple "b1" 3.0
  , Tuple "b2" 3.0
  , Tuple "b3" 3.0
  , Tuple "b4" 3.0
  , Tuple "e0" 3.0
  , Tuple "e1" 3.0
  , Tuple "e2" 3.0
  , Tuple "g0" 3.0
  , Tuple "g1" 3.0
  , Tuple "g2" 3.0
  , Tuple "c0" 3.0
  , Tuple "c1" 3.0
  , Tuple "c2" 3.0
  ] ::
    Array (Tuple String Number)

fromSoundsLights :: String -> Number
fromSoundsLights i = fromMaybe 0.0 (M.lookup i soundsLightsMap)

soundsLightsMap :: M.Map String Number
soundsLightsMap = M.fromFoldable soundsLights

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
                              ("Harm-" <> s)
                              ("Harm/" <> s <> ".ogg")
                      )
                      soundsHarm
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
                              ("Lights-" <> s <> "-l")
                              ("Lights/" <> s <> ".l.ogg")
                      )
                      soundsLights
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

playerHarm :: String -> String -> Number -> Number -> Number -> List (AudioUnit D2)
playerHarm tag name hp g time =
  let
    len = fromMaybe 0.0 (M.lookup name soundsHarmMap)
  in
    if time + kr >= 0.0 && time < len then
      pure
        $ panner_ (tag <> "_panHarm") 0.0
            ( gainT_' (tag <> "_gainHarm")
                ((epwf [ Tuple 0.0 g, Tuple 1.0 g, Tuple len 0.0 ]) time)
                ( highpass_ "_highpassHarm" hp 1.0
                    (playBufWithOffset_ (tag <> "_playerHarm") ("Harm-" <> name) 1.0 0.0)
                )
            )
    else
      Nil

playerOtw0 :: String -> Number -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerOtw0 tag len hpl hpr pan time =
  if time + kr >= 0.0 && time < len then
    pure
      $ panner_ (tag <> "_panOtw0") (pan)
          ( gainT_' (tag <> "_gainOtw0")
              ((epwf [ Tuple 0.0 0.2, Tuple 3.0 0.6, Tuple 5.0 0.0, Tuple len 0.0 ]) time)
              ( highpassT_ (tag <> "_highpassOtw0")
                  ((epwf [ Tuple 0.0 hpl, Tuple len hpr ]) time)
                  ((epwf [ Tuple 0.0 1.0, Tuple len 1.0 ]) time)
                  (playBufWithOffset_ (tag <> "_playerOtw0") ("Licks-onTheWingsOfEveryKiss6-l") 1.0 0.0)
              )
          )
  else
    Nil

playerOtw1 :: String -> Number -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerOtw1 tag len hpl hpr pan time =
  if time + kr >= 0.0 && time < len then
    pure
      $ panner_ (tag <> "_panOtw1") (pan)
          ( gainT_' (tag <> "_gainOtw1")
              ((epwf [ Tuple 0.0 0.2, Tuple 3.0 0.3, Tuple 5.0 0.0, Tuple len 0.0 ]) time)
              ( highpassT_ (tag <> "_highpassOtw1")
                  ((epwf [ Tuple 0.0 hpl, Tuple len hpr ]) time)
                  ((epwf [ Tuple 0.0 1.0, Tuple len 1.0 ]) time)
                  (playBufWithOffset_ (tag <> "_playerOtw1") ("Licks-onTheWingsOfEveryKiss5-l") 1.0 0.0)
              )
          )
  else
    Nil

playerVoice :: String -> String -> Number -> Number -> List (AudioUnit D2)
playerVoice tag name tos time =
  let
    len = fromMaybe 0.0 (M.lookup name soundsFullMap)
  in
    if time + kr >= 0.0 && time < len then
      pure
        $ panner_ (tag <> "_panVoice") 0.0
            ( gainT_' (tag <> "_gainVoice")
                ((epwf [ Tuple 0.0 0.94, Tuple len 0.94 ]) time)
                ( dynamicsCompressor_ "_compressorVoice" (-24.0) (30.0) (7.0) (0.003) (0.25)
                    ( highpass_ "_highpassVoice" 150.0 1.0
                        (playBufWithOffset_ (tag <> "_playerVoice") ("Full-" <> name) 1.0 tos)
                    )
                )
            )
    else
      Nil

playerLights :: String -> String -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerLights tag' name prate hpf vol time =
  if time + kr >= 0.0 && time < 4.0 then
    let
      tag = tag' <> name
    in
      pure
        $ panner_ (tag <> "_panLights") 0.0
            ( gainT_' (tag <> "_gainLights")
                ((epwf [ Tuple 0.0 0.2, Tuple 0.11 vol, Tuple 1.0 vol, Tuple 3.0 0.0 ]) time)
                ( dynamicsCompressor_ (tag <> "_compressorLights") (-24.0) (30.0) (7.0) (0.003) (0.25)
                    ( highpass_ (tag <> "_highpassLights") hpf 1.0
                        (playBufWithOffset_ (tag <> "_playerLights") (name) prate 0.0)
                    )
                )
            )
  else
    Nil

playerKiss :: String -> Number -> Number -> Number -> List (AudioUnit D2)
playerKiss tag gd hpf time =
  if time + kr >= 0.0 && time < 5.0 then
    pure
      $ panner_ (tag <> "_panKiss") 0.0
          ( gainT_' (tag <> "_gainKiss")
              ((epwf [ Tuple 0.0 0.0, Tuple 0.15 0.0, Tuple 0.3 0.4, Tuple 0.5 0.2, Tuple 1.0 0.0 ]) time)
              ( dynamicsCompressor_ (tag <> "_compressorKiss") (-24.0) (30.0) (7.0) (0.003) (0.25)
                  ( highpass_ (tag <> "_highpassKiss") hpf 1.0
                      (playBufWithOffset_ (tag <> "_playerKiss") ("Licks-onTheWingsOfEveryKiss6-l") 1.0 2.5)
                  )
              )
          )
  else
    Nil

conv440 :: Int -> Number
conv440 i = 440.0 * (2.0 `pow` ((toNumber $ 0 + i) / 12.0))

startAt = 0.0 :: Number

lightsStart = 28.0 :: Number

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
                              ( [ atT 33.1 $ playerHarm "c-sharp0" "c-sharp" 1200.0 0.5 ]
                                  <> [ atT 33.4 $ playerHarm "f-sharp0" "f-sharp" 1850.0 0.3 ]
                                  -- <> [ atT 33.98 $ playerOtw0 "a" 6.182312925170068 900.0 1500.0 (-1.0) ]
                                  
                                  <> [ atT 33.98 $ playerOtw1 "b" 5.996553287981859 900.0 1700.0 (1.0) ]
                                  -- <> [ atT 34.45 $ playerOtw1 "c" 5.996553287981859 1200.0 2500.0 (-1.0) ]
                                  
                                  <> [ atT 34.45 $ playerOtw0 "d" 6.182312925170068 1200.0 2500.0 (-1.0) ]
                                  <> [ atT 35.05 $ playerOtw1 "e" 5.996553287981859 1500.0 3000.0 (1.0) ]
                                  <> [ atT 35.05 $ playerOtw0 "f" 6.182312925170068 1500.0 3000.0 (-1.0) ]
                                  <> (map (\i -> atT (36.5 + (toNumber i * 0.6)) $ playerKiss (show i) (toNumber i * 0.02) (1700.0 + (toNumber i * 200.0))) (range 0 10))
                                  <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.0 + (nf * 0.45)) $ playerLights (show i) "Lights-b3-l" 1.0 (1000.0 + (nf * 200.0)) (0.85 - (abs (nf - 3.0) * 0.05))) (range 0 5))
                                  <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.0 + (nf * 0.5)) $ playerLights (show i) "Lights-g2-l" 1.02 (1400.0 + (nf * 200.0)) (0.65 - (abs (nf - 2.0) * 0.05))) (range 0 4))
                                  <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.1 + (nf * 0.6)) $ playerLights (show i) "Lights-e0-l" 1.0 (1500.0 + (nf * 200.0)) (0.45 - (abs (nf - 2.0) * 0.05))) (range 0 3))
                                  <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.15 + (nf * 0.9)) $ playerLights (show i) "Lights-c2-l" 1.0 (1700.0 + (nf * 200.0)) (0.65 - (nf * 0.05))) (range 0 1))
                              )
                          )
                    )
                )
        )
