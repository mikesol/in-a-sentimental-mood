module Klank.IASM.Middle2 where

import Prelude
import Control.Promise (toAffE)
import Data.Array (filter, foldl, mapWithIndex)
import Data.Array (fold, head, last, range, span)
import Data.Int (toNumber)
import Data.Lazy (Lazy, defer, force)
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
  [ Tuple "guitarFill" 11.6
  ] ::
    Array (Tuple String Number)

fromSoundsLicks :: String -> Number
fromSoundsLicks i = fromMaybe 0.0 (M.lookup i soundsLicksMap)

soundsLicksMap :: M.Map String Number
soundsLicksMap = M.fromFoldable soundsLicks

soundsEnd =
  [ Tuple "endVoice2" 90.0
  , Tuple "endGuitar2" 90.0
  , Tuple "outroOrgan" 90.0
  , Tuple "lowC" 23.0
  , Tuple "inASentimentalMood" 10.0
  ] ::
    Array (Tuple String Number)

fromSoundsEnd :: String -> Number
fromSoundsEnd i = fromMaybe 0.0 (M.lookup i soundsEndMap)

soundsEndMap :: M.Map String Number
soundsEndMap = M.fromFoldable soundsEnd

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
  , Tuple "rosePetalsSeemToFallItsAllICouldDreamToCallYouMine" 10.745034013605443
  , Tuple "rosePetalsSeemToFallItsAllICouldDreamToCallYouMine1" 9.804625850340136
  ] ::
    Array (Tuple String Number)

fromSoundsBridge :: String -> Number
fromSoundsBridge i = fromMaybe 0.0 (M.lookup i soundsBridgeMap)

soundsBridgeMap :: M.Map String Number
soundsBridgeMap = M.fromFoldable soundsBridge

soundsBridge2 =
  [ Tuple "myHeartsALighterThingSinceYouMadeThisNightAThingDivine" 10.727619047619047
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

soundsRamp =
  [ (Tuple "A-A4-0" 2.511156462585034)
  , (Tuple "A-Fis4-0" 1.1590022675736962)
  , (Tuple "A-G4-0" 1.3642403628117914)
  , (Tuple "In-D4-0" 2.559433106575964)
  , (Tuple "In-E4-0" 2.61578231292517)
  , (Tuple "In-Fis4-0" 3.050408163265306)
  , (Tuple "In-G4-0" 0.9577777777777777)
  , (Tuple "Men-A4-0" 1.5292290249433107)
  , (Tuple "Men-B4-0" 1.5292290249433107)
  , (Tuple "Men-C5-0" 1.3280045351473924)
  , (Tuple "Men-D4-0" 1.464829931972789)
  , (Tuple "Men-D5-0" 1.1911791383219954)
  , (Tuple "Men-E4-0" 1.4769160997732427)
  , (Tuple "Men-G4-0" 1.2193650793650794)
  , (Tuple "Sen-A4-0" 1.7626303854875283)
  , (Tuple "Sen-B4-0" 1.782766439909297)
  , (Tuple "Sen-E4-0" 1.4044671201814058)
  , (Tuple "Sen-Fis4-0" 1.609705215419501)
  , (Tuple "Sen-G4-0" 1.4769160997732427)
  , (Tuple "Tal-A4-0" 1.0543764172335601)
  , (Tuple "Tal-B4-0" 1.1348526077097505)
  , (Tuple "Tal-C5-0" 1.255578231292517)
  , (Tuple "Tal-D4-0" 1.4085034013605442)
  , (Tuple "Tal-D5-0" 1.3561904761904762)
  , (Tuple "Tal-E4-0" 1.3400907029478457)
  , (Tuple "Tal-E5-0" 1.311904761904762)
  , (Tuple "Tal-Fis4-0" 1.1227891156462586)
  , (Tuple "Tal-Fis5-0" 1.2756916099773243)
  , (Tuple "Tal-G4-0" 1.2273922902494332)
  , (Tuple "Tal-G5-0" 1.2112925170068027)
  , (Tuple "Ti-A4-0" 1.4085034013605442)
  , (Tuple "Ti-B4-0" 1.2725396825396826)
  , (Tuple "Ti-C5-0" 1.1951927437641723)
  , (Tuple "Ti-Cis5-0" 1.203265306122449)
  , (Tuple "Ti-D5-0" 1.323968253968254)
  , (Tuple "Ti-Fis4-0" 1.3038548752834467)
  , (Tuple "Ti-G4-0" 1.2837414965986396)
  , Tuple "Men-E5-0" 1.8350566893424036
  ] ::
    Array (Tuple String Number)

fromSoundsRamp :: String -> Number
fromSoundsRamp i = fromMaybe 0.0 (M.lookup i soundsRampMap)

soundsRampMap :: M.Map String Number
soundsRampMap = M.fromFoldable soundsRamp

soundsMood =
  [ MoodIdx (Tuple "E3" 0) 1.8808163265306121
  , MoodIdx (Tuple "E3" 1) 1.689251700680272
  , MoodIdx (Tuple "E3" 2) 1.5615419501133787
  , MoodIdx (Tuple "E3" 3) 3.2565986394557824
  , MoodIdx (Tuple "B3" 0) 5.793378684807256
  , MoodIdx (Tuple "E4" 0) 7.7148299319727895
  , MoodIdx (Tuple "F#4" 0) 4.231836734693878
  , MoodIdx (Tuple "G4" 0) 4.818140589569161
  , MoodIdx (Tuple "A4" 0) 2.885079365079365
  , MoodIdx (Tuple "B4" 0) 1.486077097505669
  , MoodIdx (Tuple "C5" 0) 2.815419501133787
  , MoodIdx (Tuple "C#5" 0) 9.102222222222222
  , MoodIdx (Tuple "D5" 0) 3.1579138321995464
  , MoodIdx (Tuple "D#5" 0) 4.284081632653061
  , MoodIdx (Tuple "E5" 0) 8.59718820861678
  , MoodIdx (Tuple "F#5" 0) 9.189297052154195
  , MoodIdx (Tuple "G5" 0) 6.809251700680272
  , MoodIdx (Tuple "A5" 0) 4.14827664399093
  , MoodIdx (Tuple "B5" 0) 2.3858503401360545
  ] ::
    Array MoodIdx

fromSoundsMood :: String -> Int -> Number
fromSoundsMood s i = fromMaybe 0.0 (M.lookup (s <> "-" <> show i) soundsMoodMap)

soundsMoodMap :: M.Map String Number
soundsMoodMap = M.fromFoldable (map (\(MoodIdx (Tuple x y) b) -> Tuple (x <> "-" <> show y) b) soundsMood)

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
                      ( \(MoodIdx (Tuple pitch n) _) ->
                          let
                            s = show n
                          in
                            Tuple
                              ("Mood-" <> pitch <> "-" <> s <> "-l")
                              ("Mood/" <> (replace (Pattern "#") (Replacement "%23") pitch) <> "/" <> s <> ".l.ogg")
                      )
                      soundsMood
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
                      ( \i ->
                          let
                            s = fst i
                          in
                            Tuple
                              ("End-" <> s)
                              ("End/" <> s <> ".ogg")
                      )
                      soundsEnd
                  )
                <> ( map
                      ( \i ->
                          let
                            s = fst i
                          in
                            Tuple
                              ("Ramp-" <> s <> "-l")
                              ("Ramp/" <> s <> ".l.ogg")
                      )
                      soundsRamp
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
boundPlayer :: Number -> Number -> Lazy (List (AudioUnit D2)) -> List (AudioUnit D2)
boundPlayer len time a = if (time) + kr >= 0.0 && time < (len) then force a else Nil

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

------------------
endGainFunction :: Number -> Number -> AudioParameter Number
endGainFunction gl time = ((epwf [ Tuple 0.0 gl, Tuple 30.0 gl, Tuple 38.0 (gl / 2.0), Tuple 53.0 0.0 ]) time)

playerGuitar :: String -> String -> Number -> Number -> List (AudioUnit D2)
playerGuitar tag name tos time =
  let
    len = fromMaybe 0.0 (M.lookup name soundsFullMap)
  in
    boundPlayer (len + 1.0) time
      ( defer \_ ->
          pure
            $ panner_ (tag <> "_panGuitar") 0.0
                ( gainT_' (tag <> "_gainGuitar")
                    ((epwf [ Tuple 0.0 0.95, Tuple len 0.95 ]) time)
                    (playBufWithOffset_ (tag <> "_playerGuitar") ("Full-" <> name) 1.0 tos)
                )
      )

playerGuitarEnd :: Number -> List (AudioUnit D2)
playerGuitarEnd time =
  let
    len = fromMaybe 0.0 (M.lookup "endGuitar2" soundsEndMap)
  in
    boundPlayer (len + 1.0) time
      ( defer \_ ->
          pure
            $ panner_ ("guitarEnd" <> "_panGuitarEnd") 0.0
                ( gainT_' ("guitarEnd" <> "_panGuitarEnd")
                    (endGainFunction 0.94 time)
                    (playBufWithOffset_ ("guitarEnd" <> "_playerGuitar") ("End-endGuitar2") 1.0 0.0)
                )
      )

playerOrganOutro :: Number -> List (AudioUnit D2)
playerOrganOutro time =
  let
    len = fromMaybe 0.0 (M.lookup "outroOrgan" soundsEndMap)
  in
    boundPlayer (len + 1.0) time
      ( defer \_ ->
          pure
            $ panner_ ("outroOrgan" <> "-outroOrgan") 0.0
                ( gainT_' ("outroOrgan" <> "-outroOrgan")
                    (endGainFunction 0.94 time)
                    (playBufWithOffset_ ("outroOrgan" <> "_playerGuitar") ("End-outroOrgan") 1.0 0.0)
                )
      )

playerGuitar2 :: String -> Number -> List (AudioUnit D2)
playerGuitar2 tag time =
  boundPlayer (15.0 + 1.0) time
    ( defer \_ ->
        pure
          $ panner_ (tag <> "_panGuitar2") 0.0
              ( gainT_' (tag <> "_gainGuitar2")
                  ((epwf [ Tuple 0.0 0.4, Tuple 0.2 1.0, Tuple 15.0 1.0 ]) time)
                  (playBuf_ (tag <> "_playerGuitar2") ("Licks-guitarFill-l") 1.0)
              )
    )

playerRodeFill :: String -> Number -> List (AudioUnit D2)
playerRodeFill tag time =
  boundPlayer (14.0 + 1.0) time
    ( defer \_ ->
        pure
          $ panner_ (tag <> "_panRodeFill") 0.0
              ( gainT_' (tag <> "_gainRodeFill")
                  ((epwf [ Tuple 0.0 0.4, Tuple 0.2 1.0, Tuple 14.0 1.0 ]) time)
                  (playBuf_ (tag <> "_playerRodeFill") ("Rode-rodeFill") 1.0)
              )
    )

playerHarm :: String -> String -> Number -> Number -> Number -> List (AudioUnit D2)
playerHarm tag name hp g time =
  let
    len = fromMaybe 0.0 (M.lookup name soundsHarmMap)
  in
    boundPlayer len time
      ( defer \_ ->
          pure
            $ panner_ (tag <> "_panHarm") 0.0
                ( gainT_' (tag <> "_gainHarm")
                    ((epwf [ Tuple 0.0 g, Tuple 1.0 g, Tuple len 0.0 ]) time)
                    ( highpass_ (tag <> "_highpassHarm") hp 1.0
                        (playBufWithOffset_ (tag <> "_playerHarm") ("Harm-" <> name) 1.0 0.0)
                    )
                )
      )

vocalCompressor :: forall ch. Pos ch => String -> AudioUnit ch -> AudioUnit ch
vocalCompressor tag = dynamicsCompressor_ tag (-24.0) (30.0) (7.0) (0.003) (0.25)

mainHighpass :: forall ch. Pos ch => String -> AudioUnit ch -> AudioUnit ch
mainHighpass tag = highpass_ tag 150.0 1.0

playerVoice :: String -> String -> Number -> Number -> List (AudioUnit D2)
playerVoice tag name tos time =
  let
    len = fromMaybe 0.0 (M.lookup name soundsFullMap)
  in
    boundPlayer (len + 1.0) time
      ( defer \_ ->
          pure
            $ panner_ (tag <> "_panVoice") 0.0
                ( gainT_' (tag <> "_gainVoice")
                    ((epwf [ Tuple 0.0 0.94, Tuple len 0.94 ]) time)
                    ( vocalCompressor (tag <> "_compressorVoice")
                        ( mainHighpass (tag <> "_highpassVoice")
                            (playBufWithOffset_ (tag <> "_playerVoice") ("Full-" <> name) 1.0 tos)
                        )
                    )
                )
      )

playerLowCEnd :: Number -> List (AudioUnit D2)
playerLowCEnd time =
  let
    len = fromMaybe 0.0 (M.lookup "lowC" soundsEndMap)
  in
    boundPlayer (len + 1.0) time
      ( defer \_ ->
          pure
            $ panner_ ("lowC" <> "_panLowCEnd") 0.0
                ( gainT_' ("lowC" <> "_gainLowCEnd")
                    (endGainFunction 0.94 time)
                    ( vocalCompressor ("lowC" <> "_compressorLowCEnd")
                        ( mainHighpass ("lowC" <> "_highpassLowCEnd")
                            (playBufWithOffset_ ("lowC" <> "_playerLowCEnd") ("End-lowC") 1.0 0.0)
                        )
                    )
                )
      )

playerVoiceEnd :: Number -> List (AudioUnit D2)
playerVoiceEnd time =
  let
    len = fromMaybe 0.0 (M.lookup "endVoice2" soundsEndMap)
  in
    boundPlayer (len + 1.0) time
      ( defer \_ ->
          pure
            $ panner_ ("endVoice2" <> "_panVoiceEnd") 0.0
                ( gainT_' ("endVoice2" <> "_gainVoiceEnd")
                    (endGainFunction 0.94 time)
                    ( vocalCompressor ("endVoice2" <> "_compressorVoiceEnd")
                        ( mainHighpass ("endVoice2" <> "_highpassVoiceEnd")
                            (playBufWithOffset_ ("endVoice2" <> "_playerVoiceEnd") ("End-endVoice2") 1.0 0.0)
                        )
                    )
                )
      )

playerVoiceIASM :: Number -> List (AudioUnit D2)
playerVoiceIASM time =
  let
    len = fromMaybe 0.0 (M.lookup "endVoice2" soundsEndMap)
  in
    boundPlayer (len + 1.0) time
      ( defer \_ ->
          pure
            $ panner_ ("endVoice2" <> "_panVoiceEnd2") 0.0
                ( gainT_' ("endVoice2" <> "_gainVoiceEnd2")
                    ((epwf [ Tuple 0.0 0.94, Tuple len 0.94 ]) time)
                    ( vocalCompressor ("endVoice2" <> "_compressorVoiceEnd2")
                        ( mainHighpass ("endVoice2" <> "_highpassVoiceEnd2")
                            (playBufWithOffset_ ("endVoice2" <> "_playerVoiceEnd2") ("End-inASentimentalMood") 1.0 0.0)
                        )
                    )
                )
      )

playerLights :: String -> String -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerLights tag' name prate hpf vol time =
  let
    tag = tag' <> name
  in
    boundPlayer (4.0) time
      ( defer \_ ->
          pure
            $ panner_ (tag <> "_panLights") 0.0
                ( gainT_' (tag <> "_gainLights")
                    ((epwf [ Tuple 0.0 0.2, Tuple 0.11 vol, Tuple 1.0 vol, Tuple 3.0 0.0 ]) time)
                    ( vocalCompressor (tag <> "_compressorLights")
                        ( highpass_ (tag <> "_highpassLights") hpf 1.0
                            (playBufWithOffset_ (tag <> "_playerLights") (name) prate 0.0)
                        )
                    )
                )
      )

playerRose :: String -> String -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerRose tag' name pan hpf vol time =
  let
    tag = tag' <> name
  in
    boundPlayer (4.0) time
      ( defer \_ ->
          pure
            $ panner_ (tag <> "_panRose") pan
                ( gainT_' (tag <> "_gainRose")
                    ((epwf [ Tuple 0.0 0.2, Tuple 0.11 vol, Tuple 1.0 vol, Tuple 3.0 0.0 ]) time)
                    ( vocalCompressor (tag <> "_compressorRose")
                        ( highpass_ (tag <> "_highpassRose") hpf 1.0
                            (playBufWithOffset_ (tag <> "_playerRose") (name) 1.0 0.0)
                        )
                    )
                )
      )

playerRoseLong :: String -> String -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerRoseLong tag' name pan hpf vol time =
  let
    tag = tag' <> name
  in
    boundPlayer (len) time
      ( defer \_ ->
          pure
            $ panner_ (tag <> "_panRoseLong") pan
                ( gainT_' (tag <> "_gainRoseLong")
                    ((epwf [ Tuple 0.0 0.2, Tuple 0.11 vol, Tuple 4.7 vol, Tuple 7.8 0.0, Tuple len 0.0 ]) time)
                    ( vocalCompressor (tag <> "_compressorRoseLong")
                        ( highpass_ (tag <> "_highpassRoseLong") hpf 1.0
                            (playBufWithOffset_ (tag <> "_playerRoseLong") ("Bridge-" <> name <> "-l") 1.0 0.0)
                        )
                    )
                )
      )
  where
  len = (fromSoundsBridge name)

playerSAS :: String -> String -> Number -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerSAS tag' name len pan hpf vol time =
  let
    tag = tag' <> name
  in
    boundPlayer (len) time
      ( defer \_ ->
          pure
            $ panner_ (tag <> "_panSAS") pan
                ( gainT_' (tag <> "_gainSAS")
                    ((epwf [ Tuple 0.0 0.0, Tuple (0.12) vol, Tuple (len - 0.3) vol, Tuple len 0.0 ]) time)
                    ( vocalCompressor (tag <> "_compressorSAS")
                        ( highpass_ (tag <> "_highpassSAS") hpf 1.0
                            (playBufWithOffset_ (tag <> "_playerSAS") (name) 1.0 0.0)
                        )
                    )
                )
      )

playerDrips :: String -> String -> Number -> Number -> Number -> List (AudioUnit D2)
playerDrips tag' name len hpf time =
  let
    tag = tag' <> name
  in
    boundPlayer (len) time
      ( defer \_ ->
          pure
            $ pannerT_ (tag <> "_panDrips") ((epwf [ Tuple 0.0 0.2, Tuple 2.0 (-0.5), Tuple len 0.5 ]) time)
                ( gainT_' (tag <> "_gainDrips")
                    ((epwf [ Tuple 0.0 0.0, Tuple 1.0 0.55, Tuple 2.0 0.3, Tuple 3.0 0.6, Tuple 4.5 0.0 ]) time)
                    ( vocalCompressor (tag <> "_compressorDrips")
                        ( highpass_ (tag <> "_highpassDrips") hpf 1.0
                            (playBufWithOffset_ (tag <> "_playerDrips") (name) 1.0 0.0)
                        )
                    )
                )
      )

playerKiss :: String -> Number -> Number -> Number -> List (AudioUnit D2)
playerKiss tag gd hpf time =
  boundPlayer (5.0) time
    ( defer \_ ->
        pure
          $ panner_ (tag <> "_panKiss") 0.0
              ( gainT_' (tag <> "_gainKiss")
                  ((epwf [ Tuple 0.0 0.0, Tuple 0.15 0.0, Tuple 0.3 0.4, Tuple 0.5 0.2, Tuple 1.0 0.0 ]) time)
                  ( vocalCompressor (tag <> "_compressorKiss")
                      ( highpass_ (tag <> "_highpassKiss") hpf 1.0
                          (playBufWithOffset_ (tag <> "_playerKiss") ("Licks-onTheWingsOfEveryKiss6-l") 1.0 2.5)
                      )
                  )
              )
    )

playerIctus :: String -> String -> Number -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerIctus tag name len vos ros tos time =
  boundPlayer (len + 1.0) time
    ( defer \_ ->
        pure
          $ panner_ (tag <> "_panIctus") 0.0
              ( gainT_' (tag <> "_gainIctus")
                  ((epwf [ Tuple 0.0 0.0, Tuple 0.15 0.0, Tuple 1.0 vos, Tuple (len - 0.15) 0.0, Tuple len 0.0 ]) time)
                  ( highpassT_ (tag <> "_highpassIctus") ((epwf [ Tuple 0.0 200.0, Tuple len 200.0 ]) time)
                      ((epwf [ Tuple 0.0 1.0, Tuple len 1.0 ]) time)
                      (playBufWithOffset_ (tag <> "_playerIctus") (name) ros tos)
                  )
              )
    )

data InASentimentalMood
  = In
  | A
  | Sen
  | Ti
  | Men
  | Tal
  | Mood

data Pitch
  = C4
  | Cis4
  | D4
  | Dis4
  | E4
  | F4
  | Fis4
  | G4
  | Gis4
  | A4
  | B4
  | C5
  | Cis5
  | D5
  | Dis5
  | E5
  | F5
  | Fis5
  | G5
  | Gis5
  | A5
  | B5
  | C6
  | Cis6
  | D6

p2s_0 :: Pitch -> String
p2s_0 C4 = "C4"

p2s_0 Cis4 = "Cis4"

p2s_0 D4 = "D4"

p2s_0 Dis4 = "Dis4"

p2s_0 E4 = "E4"

p2s_0 F4 = "F4"

p2s_0 Fis4 = "Fis4"

p2s_0 G4 = "G4"

p2s_0 Gis4 = "Gis4"

p2s_0 A4 = "A4"

p2s_0 B4 = "B4"

p2s_0 C5 = "C5"

p2s_0 Cis5 = "Cis5"

p2s_0 D5 = "D5"

p2s_0 Dis5 = "Dis5"

p2s_0 E5 = "E5"

p2s_0 F5 = "F5"

p2s_0 Fis5 = "Fis5"

p2s_0 G5 = "G5"

p2s_0 Gis5 = "Gis5"

p2s_0 A5 = "A5"

p2s_0 B5 = "B5"

p2s_0 C6 = "C6"

p2s_0 Cis6 = "Cis6"

p2s_0 D6 = "D6"

p2s_1 :: Pitch -> String
p2s_1 C4 = "C4"

p2s_1 Cis4 = "C#4"

p2s_1 D4 = "D4"

p2s_1 Dis4 = "D#4"

p2s_1 E4 = "E4"

p2s_1 F4 = "F4"

p2s_1 Fis4 = "F#4"

p2s_1 G4 = "G4"

p2s_1 Gis4 = "G#4"

p2s_1 A4 = "A4"

p2s_1 B4 = "B4"

p2s_1 C5 = "C5"

p2s_1 Cis5 = "C#5"

p2s_1 D5 = "D5"

p2s_1 Dis5 = "D#5"

p2s_1 E5 = "E5"

p2s_1 F5 = "F5"

p2s_1 Fis5 = "F#5"

p2s_1 G5 = "G5"

p2s_1 Gis5 = "G#5"

p2s_1 A5 = "A5"

p2s_1 B5 = "B5"

p2s_1 C6 = "C6"

p2s_1 Cis6 = "C#6"

p2s_1 D6 = "D6"

data CascadeEvent
  = InEntry
  | AEntry
  | SenEntry
  | TiEntry
  | MenEntry
  | TalEntry
  | MoodEntry
  | NoEvent

data Cascade
  = Cascade InASentimentalMood Pitch Number CascadeEvent

data CSN
  = CSN Cascade String Number

cascades =
  [ Cascade In G4 0.1 NoEvent
  , Cascade In Fis4 0.1 NoEvent
  , Cascade In E4 0.1 NoEvent
  , Cascade In D4 0.1 NoEvent
  , Cascade A A4 0.08 NoEvent
  , Cascade A G4 0.08 NoEvent
  , Cascade A Fis4 0.08 NoEvent
  , Cascade A E4 0.08 NoEvent
  , Cascade Sen B4 0.1 NoEvent
  , Cascade Sen A4 0.1 NoEvent
  , Cascade Sen G4 0.1 NoEvent
  , Cascade Sen Fis4 0.1 NoEvent
  , Cascade Sen E4 0.1 NoEvent
  , Cascade Sen D4 0.1 NoEvent
  , Cascade Ti D5 0.08 NoEvent
  , Cascade Ti C5 0.08 NoEvent
  , Cascade Ti B4 0.08 NoEvent
  , Cascade Ti A4 0.08 NoEvent
  , Cascade Ti G4 0.08 NoEvent
  , Cascade Ti Fis4 0.08 NoEvent
  , Cascade Men E5 0.1 NoEvent
  , Cascade Men D5 0.1 NoEvent
  , Cascade Men C5 0.1 NoEvent
  , Cascade Men B4 0.1 NoEvent
  , Cascade Men A4 0.1 NoEvent
  , Cascade Men G4 0.1 NoEvent
  , Cascade Tal G5 0.08 NoEvent
  , Cascade Tal Fis5 0.08 NoEvent
  , Cascade Tal E5 0.08 NoEvent
  , Cascade Tal D5 0.08 NoEvent
  , Cascade Tal C5 0.08 NoEvent
  , Cascade Tal B4 0.08 NoEvent
  , Cascade Mood B5 0.1 NoEvent
  , Cascade Mood A5 0.1 NoEvent
  , Cascade Mood G5 0.1 NoEvent
  , Cascade Mood Fis5 0.1 NoEvent
  , Cascade Mood E5 0.1 NoEvent
  , Cascade Mood D5 0.1 NoEvent
  , Cascade Mood C5 0.1 NoEvent
  , Cascade Mood B4 0.1 NoEvent
  , Cascade Mood B5 0.1 NoEvent
  , Cascade Mood A5 0.1 NoEvent
  , Cascade Mood G5 0.1 NoEvent
  , Cascade Mood Fis5 0.1 NoEvent
  , Cascade Mood E5 0.1 NoEvent
  , Cascade Mood D5 0.1 NoEvent
  , Cascade Mood C5 0.1 NoEvent
  , Cascade Mood B4 0.1 NoEvent
  , Cascade Mood A4 0.1 NoEvent
  , Cascade Mood B5 0.1 NoEvent
  , Cascade Mood A5 0.1 NoEvent
  , Cascade Mood G5 0.1 NoEvent
  , Cascade Mood Fis5 0.1 NoEvent
  , Cascade Mood B5 0.1 NoEvent
  , Cascade Mood A5 0.1 NoEvent
  , Cascade Mood G5 0.1 NoEvent
  , Cascade Mood Fis5 0.1 NoEvent
  , Cascade Mood E5 0.1 NoEvent
  , Cascade Mood D5 0.1 NoEvent
  , Cascade Mood C5 0.1 NoEvent
  , Cascade Mood B4 0.1 NoEvent
  , Cascade Mood B5 0.1 NoEvent
  , Cascade Mood A5 0.1 NoEvent
  , Cascade Mood G5 0.1 NoEvent
  , Cascade Mood Fis5 0.1 NoEvent
  , Cascade Mood E5 0.1 NoEvent
  , Cascade Mood D5 0.1 NoEvent
  , Cascade Mood C5 0.1 NoEvent
  , Cascade Mood B4 0.1 NoEvent
  , Cascade Mood B5 0.35 NoEvent
  , Cascade Mood A5 0.38 NoEvent
  , Cascade Mood G5 0.41 NoEvent
  , Cascade Mood Fis5 0.44 NoEvent
  , Cascade Mood E5 0.47 NoEvent
  , Cascade Mood D5 0.50 NoEvent
  , Cascade Mood C5 0.53 NoEvent
  , Cascade Mood B4 0.56 NoEvent
  ] ::
    Array Cascade

c2s :: Cascade -> String
c2s c@(Cascade word _ _ _) =
  ( case word of
      Mood -> ""
      _ -> "Ramp-"
  )
    <> c2s_1 c
    <> "-"
    <> c2s_2 c
    <> "-0-l"

c2sShort :: Cascade -> String
c2sShort c@(Cascade word _ _ _) =
  c2s_1 c
    <> "-"
    <> c2s_2 c
    <> "-0"

c2s_1 :: Cascade -> String
c2s_1 (Cascade word _ _ _) = case word of
  In -> "In"
  A -> "A"
  Sen -> "Sen"
  Ti -> "Ti"
  Men -> "Men"
  Tal -> "Tal"
  Mood -> "Mood"

c2s_2 :: Cascade -> String
c2s_2 (Cascade word pitch _ _) = case word of
  Mood -> p2s_1 pitch
  _ -> p2s_0 pitch

cascadesWithInfo =
  map
    ( \c@(Cascade word pitch t _) ->
        let
          cname = c2s c

          nameShort = c2sShort c
        in
          CSN c (cname) case word of
            Mood -> (fromSoundsMood (c2s_2 c)) 0 -- hardcode zero as quick solution
            _ -> (fromSoundsRamp (nameShort))
    )
    cascades ::
    Array CSN

cascadesWithInfoInTime =
  ( foldl
      ( \{ acc, dur } (CSN (Cascade a b c marker) d e) ->
          { acc: acc <> [ (CSN (Cascade a b (c + dur) marker) d e) ], dur: dur + c }
      )
      { acc: [], dur: 0.0 }
      cascadesWithInfo
  ) ::
    { acc :: Array CSN, dur :: Number }

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
                              ( (map (\i -> atT (36.5 + (toNumber i * 0.6)) $ playerKiss (show i) (toNumber i * 0.02) (1700.0 + (toNumber i * 200.0))) (range 0 4))
                                  <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.0 + (nf * 0.45)) $ playerLights (show i) "Lights-b3-l" 1.0 (1000.0 + (nf * 200.0)) (0.85 - (abs (nf - 4.0) * 0.05))) (range 0 6))
                                  <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.05 + (nf * 0.5)) $ playerLights (show i) "Lights-g2-l" 1.02 (1400.0 + (nf * 200.0)) (0.65 - (abs (nf - 2.0) * 0.05))) (range 0 4))
                                  <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.15 + (nf * 0.6)) $ playerLights (show i) "Lights-e0-l" 1.0 (1500.0 + (nf * 200.0)) (0.45 - (abs (nf - 1.0) * 0.05))) (range 0 3))
                                  <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.2 + (nf * 0.9)) $ playerLights (show i) "Lights-c2-l" 1.0 (1700.0 + (nf * 200.0)) (0.65 - (nf * 0.05))) (range 0 1))
                                  <> [ atT 62.8 $ playerRose ("rose0") "Bridge-rose3-l" 0.7 (2000.0) (0.7 * roseMult)
                                    , atT 63.0 $ playerRose ("rose0") "Bridge-rose2-l" (-0.7) (2000.0) (0.7 * roseMult)
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
                                    ----- entre deux
                                    , atT 76.38 $ playerRoseLong ("rosePetalsSeemToFallItsAllICouldDreamToCallYouMine") "rosePetalsSeemToFallItsAllICouldDreamToCallYouMine" 0.5 (600.0) (0.4)
                                    , atT 76.5 $ playerRoseLong ("rosePetalsSeemToFallItsAllICouldDreamToCallYouMine2") "rosePetalsSeemToFallItsAllICouldDreamToCallYouMine" (-0.5) (800.0) (0.25)
                                    , atT 76.62 $ playerRoseLong ("rosePetalsSeemToFallItsAllICouldDreamToCallYouMine3") "rosePetalsSeemToFallItsAllICouldDreamToCallYouMine" 0.5 (600.0) (0.4)
                                    , atT 76.78 $ playerRoseLong ("rosePetalsSeemToFallItsAllICouldDreamToCallYouMine4") "rosePetalsSeemToFallItsAllICouldDreamToCallYouMine" (-0.5) (800.0) (0.25)
                                    ]
                                  -- <> [ atT 89.3 $ playerSAS "drips0" "Bridge2-myHeartsALighterThingSinceYouMadeThisNightAThingDivine-l" 11.0 (-0.0) 1500.0 1.0]
                                  
                                  -- guitar fill
                                  
                                  <> [ atT 85.406 $ playerGuitar2 ("guitarHack") ]
                                  <> [ atT 76.506 $ playerRodeFill ("rdfl") ]
                                  <> [ atT 98.2 $ playerVoiceIASM ]
                                  <> ( mapWithIndex (\i (CSN (Cascade a b c _) d e) -> atT (c + 98.0) $ playerIctus (d <> show i) d e 1.0 1.0 0.0) (cascadesWithInfoInTime.acc)
                                    )
                                  <> [ atT 102.0 $ playerLowCEnd ]
                                  <> [ atT 107.0 $ playerVoiceEnd ]
                                  <> [ atT 107.0 $ playerGuitarEnd ]
                                  <> [ atT 107.0 $ playerOrganOutro ]
                              )
                          )
                    )
                )
        )
