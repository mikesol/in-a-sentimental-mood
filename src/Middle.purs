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
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, EngineInfo, allpassT_, bandpassT_, decodeAudioDataFromUri, dynamicsCompressor_, gain', gainT', gainT_, gainT_', gain_', highpassT_, highpass_, pannerMonoT_, pannerMono_, pannerT_, panner_, playBuf, playBufT_, playBufWithOffset_, playBuf_, runInBrowser, sinOsc, sinOsc_, speaker, speaker')
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

soundsRode =
  [ Tuple "dotsBb" 100.0
  , Tuple "dotsD1" 100.0
  , Tuple "dotsD2" 100.0
  , Tuple "dotsF" 100.0
  , Tuple "rodeFill" 100.0
  ] ::
    Array (Tuple String Number)

fromSoundsRode :: String -> Number
fromSoundsRode i = fromMaybe 0.0 (M.lookup i soundsRodeMap)

soundsRodeMap :: M.Map String Number
soundsRodeMap = M.fromFoldable soundsRode

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
  , Tuple "dripsAMelodySoStrangeAndSweet" 6.582857142857143
  , Tuple "guitarFill" 11.6
  , Tuple "andSweet0" 3.722176870748299
  , Tuple "andSweet1" 3.722176870748299
  , Tuple "andSweet2" 3.722176870748299
  , Tuple "andSweet3" 3.722176870748299
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

soundsBridge =
  [ Tuple "rose2" 3.0
  , Tuple "rose3" 3.0
  , Tuple "rose4" 3.0
  , Tuple "seem" 3.0
  , Tuple "seem1" 3.0
  , Tuple "seem2" 3.0
  , Tuple "seem4" 3.0
  , Tuple "seem-A" 3.0
  , Tuple "seemToFall-AesGesF" 3.0
  , Tuple "seemToFall-AGFis" 3.0
  , Tuple "seemToFall-DCBb" 3.0
  , Tuple "rosePetalsSeemToFall-Slow-Glitch" 7.877369614512472
  , Tuple "rosePetalsSeemToFall-Vapid-Glitch" 5.7759637188208615
  , Tuple "rosePetalsSeemToFall-Happy-Glitch" 5.0213151927437645
  , Tuple "rosePetalsSeemToFall-Happier-Glitch" 5.189659863945578
  , Tuple "rosePetalsSeemToFall-GM" 4.138956916099773
  , Tuple "rosePetalsSeemToFallItsAllICouldDreamToCallYouMine" 10.745034013605443
  , Tuple "rosePetalsSeemToFallItsAllICouldDreamToCallYouMine1" 9.804625850340136
  --  , Tuple "rosePetalsSeemToFallItsAllICouldDreamToCallYouMine2" 10.379319727891156
  --  , Tuple "rosePetalsSeemToFallItsAllICouldDreamToCallYouMine3" 11.186213151927438
  --  , Tuple "rosePetalsSeemToFallItsAllICouldDreamToCallYouMine4" 11.400997732426303
  ] ::
    Array (Tuple String Number)

fromSoundsBridge :: String -> Number
fromSoundsBridge i = fromMaybe 0.0 (M.lookup i soundsBridgeMap)

soundsBridgeMap :: M.Map String Number
soundsBridgeMap = M.fromFoldable soundsBridge

soundsBridge2 =
  [ Tuple "mhl-glitch-0" 5.787573696145125
  , Tuple "mhl-glitch-2" 6.30421768707483
  , Tuple "aLight" 1.4164172335600906
  , Tuple "aLight1" 2.3219954648526078
  , Tuple "aLight2" 1.253877551020408
  , Tuple "mhl-glitch-3" 7.22140589569161
  , Tuple "mhl-glitch-4" 2.211700680272109
  , Tuple "mhl-glitch-5" 7.836734693877551
  , Tuple "mhl-glitch-6-softcore" 2.316190476190476
  , Tuple "dididi" 3.297233560090703
  , Tuple "dididiA" 3.8080725623582765
  , Tuple "divine-CA" 2.8792743764172335
  , Tuple "divine-CisA" 2.5541950113378684
  , Tuple "divine-DA" 2.089795918367347
  , Tuple "divine-DC" 2.5541950113378684
  , Tuple "divine-DE" 3.6687528344671203
  , Tuple "divineA-Fis" 2.1884807256235828
  , Tuple "vivivi" 3.6687528344671203
  , Tuple "vivivi1" 5.38702947845805
  , Tuple "myHeartsALighterThingSinceYouMadeThisNightAThingDivine" 10.727619047619047
  ] ::
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
                              ("Rode-" <> s)
                              ("Rode/" <> s <> ".ogg")
                      )
                      soundsRode
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
                              ("Bridge-" <> s <> "-l")
                              ("Bridge/" <> s <> ".l.ogg")
                      )
                      soundsBridge
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
    if time + kr >= 0.0 && time < (len + 1.0) then
      pure
        $ panner_ (tag <> "_panGuitar") 0.0
            ( gainT_' (tag <> "_gainGuitar")
                ((epwf [ Tuple 0.0 0.95, Tuple len 0.95 ]) time)
                ( highpass_ "_highpassGuitar" 150.0 1.0
                    (playBufWithOffset_ (tag <> "_playerGuitar") ("Full-" <> name) 1.0 tos)
                )
            )
    else
      Nil

playerGuitar2 :: String -> Number -> List (AudioUnit D2)
playerGuitar2 tag time =
  if time + kr >= 0.0 && time < (15.0 + 1.0) then
    pure
      $ panner_ (tag <> "_panGuitar2") 0.0
          ( gainT_' (tag <> "_gainGuitar2")
              ((epwf [ Tuple 0.0 0.4, Tuple 0.2 1.0, Tuple 15.0 1.0 ]) time)
              ( highpass_ "_highpassGuitar2" 150.0 1.0
                  (playBuf_ (tag <> "_playerGuitar2") ("Licks-guitarFill-l") 1.0)
              )
          )
  else
    Nil

playerRodeFill :: String -> Number -> List (AudioUnit D2)
playerRodeFill tag time =
  if time + kr >= 0.0 && time < (14.0 + 1.0) then
    pure
      $ panner_ (tag <> "_panRodeFill") 0.0
          ( gainT_' (tag <> "_gainRodeFill")
              ((epwf [ Tuple 0.0 0.4, Tuple 0.2 1.0, Tuple 14.0 1.0 ]) time)
              (playBuf_ (tag <> "_playerRodeFill") ("Rode-rodeFill") 1.0)
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
      $ pannerT_ (tag <> "_panOtw0") ((epwf [ Tuple 0.0 pan, Tuple len ((-1.0) * pan) ]) time)
          ( gainT_' (tag <> "_gainOtw0")
              ((epwf [ Tuple 0.0 0.0, Tuple 1.0 0.0, Tuple 4.5 1.2, Tuple 5.0 0.0, Tuple len 0.0 ]) time)
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
      $ pannerT_ (tag <> "_panOtw1") ((epwf [ Tuple 0.0 pan, Tuple len ((-1.0) * pan) ]) time)
          ( gainT_' (tag <> "_gainOtw1")
              ((epwf [ Tuple 0.0 0.3, Tuple 3.0 1.2, Tuple 5.0 0.0, Tuple len 0.0 ]) time)
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
    if time + kr >= 0.0 && time < (len + 1.0) then
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

playerRose :: String -> String -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerRose tag' name pan hpf vol time =
  if time + kr >= 0.0 && time < 4.0 then
    let
      tag = tag' <> name
    in
      pure
        $ panner_ (tag <> "_panRose") pan
            ( gainT_' (tag <> "_gainRose")
                ((epwf [ Tuple 0.0 0.2, Tuple 0.11 vol, Tuple 1.0 vol, Tuple 3.0 0.0 ]) time)
                ( dynamicsCompressor_ (tag <> "_compressorRose") (-24.0) (30.0) (7.0) (0.003) (0.25)
                    ( highpass_ (tag <> "_highpassRose") hpf 1.0
                        (playBufWithOffset_ (tag <> "_playerRose") (name) 1.0 0.0)
                    )
                )
            )
  else
    Nil

playerRoseLong :: String -> String -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerRoseLong tag' name pan hpf vol time =
  if time + kr >= 0.0 && time < len then
    let
      tag = tag' <> name
    in
      pure
        $ panner_ (tag <> "_panRoseLong") pan
            ( gainT_' (tag <> "_gainRoseLong")
                ((epwf [ Tuple 0.0 0.2, Tuple 0.11 vol, Tuple (len - 0.5) vol, Tuple len 0.0 ]) time)
                ( dynamicsCompressor_ (tag <> "_compressorRoseLong") (-24.0) (30.0) (7.0) (0.003) (0.25)
                    ( highpass_ (tag <> "_highpassRoseLong") hpf 1.0
                        (playBufWithOffset_ (tag <> "_playerRoseLong") ("Bridge-" <> name <> "-l") 1.0 0.0)
                    )
                )
            )
  else
    Nil
  where
  len = (fromSoundsBridge name)

playerSAS :: String -> String -> Number -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerSAS tag' name len pan hpf vol time =
  if time + kr >= 0.0 && time < len then
    let
      tag = tag' <> name
    in
      pure
        $ panner_ (tag <> "_panSAS") pan
            ( gainT_' (tag <> "_gainSAS")
                ((epwf [ Tuple 0.0 0.0, Tuple (0.12) vol, Tuple (len - 0.3) vol, Tuple len 0.0 ]) time)
                ( dynamicsCompressor_ (tag <> "_compressorSAS") (-24.0) (30.0) (7.0) (0.003) (0.25)
                    ( highpass_ (tag <> "_highpassSAS") hpf 1.0
                        (playBufWithOffset_ (tag <> "_playerSAS") (name) 1.0 0.0)
                    )
                )
            )
  else
    Nil

playerDrips :: String -> String -> Number -> Number -> Number -> List (AudioUnit D2)
playerDrips tag' name len hpf time =
  if time + kr >= 0.0 && time < len then
    let
      tag = tag' <> name
    in
      pure
        $ pannerT_ (tag <> "_panDrips") ((epwf [ Tuple 0.0 0.2, Tuple 2.0 (-0.5), Tuple len 0.5 ]) time)
            ( gainT_' (tag <> "_gainDrips")
                ((epwf [ Tuple 0.0 0.0, Tuple 1.0 0.55, Tuple 2.0 0.3, Tuple 3.0 0.6, Tuple 4.5 0.0 ]) time)
                ( dynamicsCompressor_ (tag <> "_compressorDrips") (-24.0) (30.0) (7.0) (0.003) (0.25)
                    ( highpass_ (tag <> "_highpassDrips") hpf 1.0
                        (playBufWithOffset_ (tag <> "_playerDrips") (name) 1.0 0.0)
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

roseMult = 1.2 :: Number

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
                                  -- <> [ atT 32.88 $ playerOtw1 "b" 5.996553287981859 900.0 1700.0 (1.0) ]
                                  
                                  <> [ atT 33.78 $ playerOtw0 "c" 6.182312925170068 1200.0 2500.0 (1.0) ]
                                  <> [ atT 34.18 $ playerOtw0 "d" 6.182312925170068 1200.0 2500.0 (-1.0) ]
                                  <> [ atT 34.45 $ playerOtw1 "e" 5.996553287981859 1500.0 3000.0 (1.0) ]
                                  <> [ atT 35.05 $ playerOtw0 "f" 6.182312925170068 1500.0 3000.0 (-1.0) ]
                                  <> (map (\i -> atT (36.5 + (toNumber i * 0.6)) $ playerKiss (show i) (toNumber i * 0.02) (1700.0 + (toNumber i * 200.0))) (range 0 5))
                                  <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.0 + (nf * 0.45)) $ playerLights (show i) "Lights-b3-l" 1.0 (1000.0 + (nf * 200.0)) (0.85 - (abs (nf - 4.0) * 0.05))) (range 0 6))
                                  <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.05 + (nf * 0.5)) $ playerLights (show i) "Lights-g2-l" 1.02 (1400.0 + (nf * 200.0)) (0.65 - (abs (nf - 2.0) * 0.05))) (range 0 4))
                                  <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.15 + (nf * 0.6)) $ playerLights (show i) "Lights-e0-l" 1.0 (1500.0 + (nf * 200.0)) (0.45 - (abs (nf - 1.0) * 0.05))) (range 0 3))
                                  <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.2 + (nf * 0.9)) $ playerLights (show i) "Lights-c2-l" 1.0 (1700.0 + (nf * 200.0)) (0.65 - (nf * 0.05))) (range 0 1))
                                  <> [ atT 45.0 $ playerDrips "dripsF0" "Licks-onTheWingsOfEveryKiss2-l" 8.582857142857143 1600.0
                                    , atT 45.9 $ playerDrips "dripsF0" "Licks-onTheWingsOfEveryKiss3-l" 8.582857142857143 1800.0
                                    , atT 46.8 $ playerSAS "drips0" "Licks-andSweet0-l" 3.722176870748299 (-0.5) 1500.0 1.0
                                    , atT 46.8 $ playerDrips "dripsF1" "Licks-onTheWingsOfEveryKiss3-l" 8.582857142857143 1600.0
                                    , atT 47.0 $ playerSAS "drips0x" "Licks-andSweet2-l" 3.722176870748299 (0.5) 1500.0 1.0
                                    , atT 48.6 $ playerSAS "drips1" "Licks-andSweet1-l" 3.722176870748299 (0.5) 1500.0 1.0
                                    , atT 48.8 $ playerSAS "drips1x" "Licks-andSweet2-l" 3.722176870748299 (-0.5) 1500.0 1.0
                                    ]
                                  <> [ atT 62.5 $ playerRose ("rose0") "Bridge-rose3-l" 0.7 (2000.0) (0.7 * roseMult)
                                    , atT 62.8 $ playerRose ("rose0") "Bridge-rose2-l" 0.7 (2000.0) (0.7 * roseMult)
                                    , atT 63.5 $ playerRose ("rose2") "Bridge-rose3-l" (0.2) (1400.0) (0.7 * roseMult)
                                    , atT 64.2 $ playerRose ("rose3") "Bridge-rose2-l" (-0.2) (1700.0) (0.7 * roseMult)
                                    , atT 64.5 $ playerRose ("rose4") "Bridge-rose3-l" (0.7) (1200.0) (0.7 * roseMult)
                                    , atT 64.9 $ playerRose ("rose5") "Bridge-rose2-l" (-0.7) (1000.0) (0.7 * roseMult)
                                    , atT 65.5 $ playerRose ("rose6") "Bridge-rose3-l" (0.2) (1400.0) (0.7 * roseMult)
                                    , atT 65.9 $ playerRose ("rose7") "Bridge-rose2-l" (-0.2) (1700.0) (0.7 * roseMult)
                                    , atT 66.2 $ playerRose ("rose8") "Bridge-rose3-l" (0.7) (1200.0) (0.7 * roseMult)
                                    , atT 66.5 $ playerRose ("rose42") "Bridge-rose2-l" (-0.7) (1000.0) (0.7 * roseMult)
                                    , atT 66.9 $ playerRose ("rose531") "Bridge-rose4-l" (0.2) (900.0) (0.5 * roseMult)
                                    , atT 67.3 $ playerRose ("rose11") "Bridge-rose3-l" (0.7) (1200.0) (0.7 * roseMult)
                                    , atT 67.7 $ playerRose ("rose12") "Bridge-rose2-l" (-0.7) (1000.0) (0.7 * roseMult)
                                    , atT 67.9 $ playerRose ("rose13") "Bridge-rose4-l" (0.2) (900.0) (0.5 * roseMult)
                                    -------------------------
                                    -- , atT 68.95 $ playerRose ("seem1") "Bridge-rose2-l" 0.7 (1000.0) (0.6)
                                    , atT 70.8 $ playerRose ("rose0") "Bridge-rose2-l" 0.7 (2000.0) 0.7
                                    , atT 71.5 $ playerRose ("rose2") "Bridge-rose3-l" (0.2) (1400.0) 0.7
                                    , atT 72.2 $ playerRose ("rose3") "Bridge-rose2-l" (-0.2) (1700.0) 0.7
                                    , atT 72.5 $ playerRose ("rose4") "Bridge-rose3-l" (0.7) (1200.0) 0.4
                                    -----------------------------
                                    , atT 75.9 $ playerRose ("rose5") "Bridge-rose2-l" (-0.7) (1600.0) 0.6
                                    , atT 76.5 $ playerRose ("rose6") "Bridge-rose3-l" (0.2) (1400.0) 0.7
                                    , atT 76.9 $ playerRose ("rose7") "Bridge-rose2-l" (-0.2) (1700.0) 0.7
                                    , atT 77.2 $ playerRose ("rose8") "Bridge-rose3-l" (0.7) (1200.0) 0.65
                                    ]
                                  -------------- seem
                                  
                                  <> [ atT 69.5 $ playerRose ("seem1") "Bridge-seem1-l" 0.0 (1000.0) 0.5
                                    , atT 71.2 $ playerRose ("seem2") "Bridge-seem-l" (-0.7) (1700.0) (0.4)
                                    , atT 72.0 $ playerRose ("seem3") "Bridge-seem2-l" (0.8) (1500.0) 0.6
                                    , atT 73.5 $ playerRose ("seem4") "Bridge-seem4-l" (-0.8) (900.0) (0.7)
                                    , atT 74.1 $ playerRose ("seem5") "Bridge-seem1-l" 0.6 (1000.0) 0.6
                                    , atT 75.0 $ playerRose ("seem6") "Bridge-seem-l" (-0.4) (1500.0) (0.4)
                                    -------------- seemToFall
                                    -- , atT 75.0 $ playerRose ("seemToFall-AesGesF") "Bridge-seemToFall-AesGesF-l" 0.35 (2100.0) (0.4)
                                    , atT 75.7 $ playerRose ("seemToFall-DCBb") "Bridge-seemToFall-DCBb-l" (-0.15) (400.0) (0.3)
                                    , atT 76.2 $ playerRose ("seemToFall-AesGesF") "Bridge-seemToFall-AGFis-l" 0.5 (2300.0) (0.25)
                                    ----- entre deux
                                    , atT 76.1 $ playerRoseLong ("rosePetalsSeemToFall-GM") "rosePetalsSeemToFall-GM" (-0.6) (1000.0) (0.35)
                                    , atT 76.38 $ playerRoseLong ("rosePetalsSeemToFallItsAllICouldDreamToCallYouMine") "rosePetalsSeemToFallItsAllICouldDreamToCallYouMine" 0.5 (600.0) (0.4)
                                    , atT 77.5 $ playerRoseLong ("rosePetalsSeemToFall-Happy-Glitch") "rosePetalsSeemToFall-Happy-Glitch" (-1.0) (1400.0) (0.4)
                                    , atT 77.7 $ playerRoseLong ("rosePetalsSeemToFall-Happier-Glitch") "rosePetalsSeemToFall-Happier-Glitch" (1.0) (1700.0) (0.35)
                                    , atT 80.0 $ playerRoseLong ("rosePetalsSeemToFallItsAllICouldDreamToCallYouMine1") "rosePetalsSeemToFallItsAllICouldDreamToCallYouMine1" (-0.2) (600.0) (0.4)
                                    -- my heart
                                    , atT 82.81 $ playerRose ("rosenx") "Bridge-rose3-l" (-0.2) (1000.0) 0.55
                                    , atT 83.2 $ playerRose ("mhl-glitch-0") "Bridge2-mhl-glitch-0-l" 0.2 (1000.0) (0.4)
                                    , atT 87.2 $ playerRose ("mhl-glitch-2") "Bridge2-mhl-glitch-2-l" (-0.4) (1000.0) (0.4)
                                    , atT 83.9 $ playerRose ("rose0x") "Bridge-rose3-l" (0.7) (1200.0) 0.45
                                    , atT 84.1 $ playerRose ("rose1x") "Bridge-rose2-l" (-0.7) (1600.0) 0.55
                                    , atT 84.5 $ playerRose ("rose2x") "Bridge-rose3-l" (0.7) (1800.0) 0.65
                                    , atT 84.67 $ playerRose ("rose3x") "Bridge-rose3-l" (-0.7) (1400.0) 0.55
                                    , atT 84.67 $ playerRose ("rose3x") "Bridge-rose3-l" (-0.7) (1400.0) 0.55
                                    --- a light
                                    , atT 87.9 $ playerRose ("aLight") "Bridge2-aLight-l" (-0.4) (800.0) (0.45)
                                    , atT 88.4 $ playerRose ("aLight1") "Bridge2-aLight2-l" (0.4) (800.0) (0.55)
                                    , atT 88.9 $ playerRose ("aLight3") "Bridge2-aLight3-l" (0.4) (800.0) (0.45)
                                    , atT 89.4 $ playerRose ("mhl-glitch-4") "Bridge2-mhl-glitch-4-l" (-0.5) (1000.0) (0.6)
                                    , atT 89.8 $ playerRose ("mhl-glitch-5") "Bridge2-mhl-glitch-5-l" (0.5) (1000.0) (0.35)
                                    ---- di di di
                                    , atT 91.1 $ playerRose ("dididi") "Bridge2-dididi-l" (-0.7) (1000.0) (0.9)
                                    , atT 91.6 $ playerRose ("dididi1") "Bridge2-dididiA-l" (0.4) (500.0) (0.95)
                                    , atT 92.0 $ playerRose ("dididi3") "Bridge2-dididi-l" (-0.4) (700.0) (0.95)
                                    , atT 92.4 $ playerRose ("dididi4") "Bridge2-dididiA-l" (0.2) (1000.0) (0.95)
                                    , atT 92.7 $ playerRose ("dididi5") "Bridge2-dididi-l" (-0.7) (1000.0) (0.9)
                                    , atT 93.1 $ playerRose ("dididi6") "Bridge2-dididiA-l" (0.4) (500.0) (0.95)
                                    , atT 93.5 $ playerRose ("dididi7") "Bridge2-dididi-l" (-0.4) (700.0) (0.95)
                                    , atT 93.9 $ playerRose ("dididi8") "Bridge2-dididiA-l" (0.2) (1000.0) (0.95)
                                    ]
                                  -- <> [ atT 89.3 $ playerSAS "drips0" "Bridge2-myHeartsALighterThingSinceYouMadeThisNightAThingDivine-l" 11.0 (-0.0) 1500.0 1.0]
                                  
                                  -- guitar fill
                                  
                                  <> [ atT 85.406 $ playerGuitar2 ("guitarHack") ]
                                  <> [ atT 76.506 $ playerRodeFill ("rdfl") ]
                              )
                          )
                    )
                )
        )
