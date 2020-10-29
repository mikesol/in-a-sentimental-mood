-- In a Sentimental Mood (https://en.wikipedia.org/wiki/In_a_Sentimental_Mood)
-- by Duke Elington
-- lyrics by Manny Kurtz
-- duration :: 4m30s
-------------------------------
---------- README -------------
-- this piece is arranged on klank.dev
-- using purescript-audio-behaviors (https://github.com/mikesol/purescript-audio-behaviors)
-- to see how the arrangement is put together, start from the function `scene` on line 2354
-- the arrangement is a series of events. any event can be commented out to mute
-- and moved in time by changing the first argument to `atT`
-- the arrangement is also on github at https://github.com/mikesol/in-a-sentimental-mood
-- if you have any questions about how to change this arrangement or how to use parts of it
-- in your own work, please ask on https://discourse.klank.dev
-- thanks for listening!
module Klank.IASM.Piece where

import Prelude
import Control.Parallel (parallel, sequential)
import Control.Promise (toAffE)
import Data.Array (filter, foldl, mapWithIndex, fold, head, last, range, span)
import Data.Either (either)
import Data.Int (toNumber)
import Data.Lazy (Lazy, defer, force)
import Data.Lens (_1, _2, over, traversed)
import Data.List ((:), List(..))
import Data.Map as M
import Data.Maybe (fromMaybe, maybe)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (class Pos, D2)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, try)
import Effect.Exception (Error)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioContext, AudioParameter(..), AudioUnit, BrowserAudioBuffer, EngineInfo, bandpassT_, convolver_, decodeAudioDataFromUri, dynamicsCompressor_, gainT_', gain_, gain_', highpassT_, highpass_, pannerMonoT_, pannerMono_, pannerT_, panner_, playBuf, playBufT_, playBufWithOffset_, playBuf_, runInBrowser, sinOsc_, speaker, speaker')
import Foreign.Object as O
import Math (abs, cos, pi, pow, sin)
import Type.Klank.Dev (Klank, affable, klank)

iasmEngineInfo =
  { msBetweenSamples: 100
  , msBetweenPings: 95
  , fastforwardLowerBound: 0.025
  , rewindUpperBound: 2.0
  , initialOffset: 0.5
  , doWebAudio: true
  } ::
    EngineInfo

soundsIn =
  [ Tuple 35 3.224671201814059
  , Tuple 78 17.963537414965987
  , Tuple 0 3.310566893424036
  , Tuple 3 3.8385714285714285
  , Tuple 5 3.0733333333333333
  ] ::
    Array (Tuple Int Number)

fromSoundsIn :: Int -> Number
fromSoundsIn i = fromMaybe 0.0 (M.lookup i soundsInMap)

soundsInMap :: M.Map Int Number
soundsInMap = M.fromFoldable soundsIn

soundsA =
  [ Tuple 131 8.642743764172335
  , Tuple 130 0.5119501133786848
  , Tuple 129 0.5608843537414966
  , Tuple 128 0.5382766439909297
  , Tuple 127 0.5382766439909297
  , Tuple 126 0.5834467120181406
  , Tuple 108 0.7509750566893424
  , Tuple 97 3.0095238095238095
  , Tuple 78 1.6204988662131519
  , Tuple 45 0.6662585034013605
  , Tuple 44 0.7208616780045352
  , Tuple 43 0.6361678004535147
  , Tuple 42 3.581700680272109
  , Tuple 35 2.008231292517007
  , Tuple 29 1.2421995464852609
  , Tuple 106 12.736371882086168
  ] ::
    Array (Tuple Int Number)

fromSoundsA :: Int -> Number
fromSoundsA i = fromMaybe 0.0 (M.lookup i soundsAMap)

soundsAMap :: M.Map Int Number
soundsAMap = M.fromFoldable soundsA

soundsSen =
  [ Tuple 105 1.3107256235827665
  , Tuple 104 1.1829251700680272
  , Tuple 103 1.0809523809523809
  , Tuple 102 1.2127891156462585
  , Tuple 114 1.1819727891156462
  , Tuple 108 1.2045804988662132
  , Tuple 107 1.227142857142857
  , Tuple 101 1.400294784580499
  , Tuple 61 11.070680272108843
  ] ::
    Array (Tuple Int Number)

fromSoundsSen :: Int -> Number
fromSoundsSen i = fromMaybe 0.0 (M.lookup i soundsSenMap)

soundsSenMap :: M.Map Int Number
soundsSenMap = M.fromFoldable soundsSen

soundsTi =
  [ Tuple 84 8.447006802721088
  , Tuple 83 12.203718820861678
  , Tuple 82 11.849886621315193
  , Tuple 19 17.6543537414966
  ] ::
    Array (Tuple Int Number)

fromSoundsTi :: Int -> Number
fromSoundsTi i = fromMaybe 0.0 (M.lookup i soundsTiMap)

soundsTiMap :: M.Map Int Number
soundsTiMap = M.fromFoldable soundsTi

soundsMen =
  [ Tuple 95 2.836031746031746
  , Tuple 92 1.4133786848072563
  , Tuple 85 0.8898412698412699
  , Tuple 94 8.6378231292517
  , Tuple 93 6.060408163265306
  , Tuple 82 2.3940816326530614
  , Tuple 1 5.638866213151927
  , Tuple 0 5.563560090702948
  , Tuple 2 8.334058956916099
  , Tuple 3 11.277709750566894
  , Tuple 4 11.59390022675737
  ] ::
    Array (Tuple Int Number)

fromSoundsMen :: Int -> Number
fromSoundsMen i = fromMaybe 0.0 (M.lookup i soundsMenMap)

soundsMenMap :: M.Map Int Number
soundsMenMap = M.fromFoldable soundsMen

soundsTal =
  [ Tuple 43 6.315827664399093
  , Tuple 42 6.362267573696145
  , Tuple 32 2.2058956916099772
  , Tuple 31 2.9024943310657596
  , Tuple 24 7.604535147392291
  ] ::
    Array (Tuple Int Number)

fromSoundsTal :: Int -> Number
fromSoundsTal i = fromMaybe 0.0 (M.lookup i soundsTalMap)

soundsTalMap :: M.Map Int Number
soundsTalMap = M.fromFoldable soundsTal

data MoodIdx
  = MoodIdx (Tuple String Int) Number

soundsMood =
  [ MoodIdx (Tuple "E3" 0) 1.8808163265306121
  , MoodIdx (Tuple "E3" 1) 1.689251700680272
  , MoodIdx (Tuple "E3" 2) 1.5615419501133787
  , MoodIdx (Tuple "E3" 3) 3.2565986394557824
  , MoodIdx (Tuple "B3" 0) 5.793378684807256
  , MoodIdx (Tuple "B3" 1) 5.31156462585034
  , MoodIdx (Tuple "B3" 2) 4.481451247165533
  , MoodIdx (Tuple "B3" 3) 1.7240816326530612
  , MoodIdx (Tuple "E4" 0) 7.7148299319727895
  , MoodIdx (Tuple "E4" 1) 6.472562358276644
  , MoodIdx (Tuple "E4" 2) 7.302675736961452
  , MoodIdx (Tuple "E4" 3) 6.635102040816326
  , MoodIdx (Tuple "F#4" 0) 4.231836734693878
  , MoodIdx (Tuple "F#4" 1) 4.4117913832199545
  , MoodIdx (Tuple "F#4" 2) 4.6381859410430835
  , MoodIdx (Tuple "F#4" 3) 4.649795918367347
  , MoodIdx (Tuple "G4" 0) 4.818140589569161
  , MoodIdx (Tuple "G4" 1) 3.779047619047619
  , MoodIdx (Tuple "G4" 2) 2.7921995464852607
  , MoodIdx (Tuple "G4" 3) 3.5874829931972787
  , MoodIdx (Tuple "G4" 4) 5.491519274376417
  , MoodIdx (Tuple "A4" 0) 2.885079365079365
  , MoodIdx (Tuple "A4" 1) 3.221768707482993
  , MoodIdx (Tuple "A4" 2) 3.0203628117913834
  , MoodIdx (Tuple "A4" 3) 3.111473922902494
  , MoodIdx (Tuple "A4" 5) 8.765532879818593
  , MoodIdx (Tuple "B4" 0) 1.486077097505669
  , MoodIdx (Tuple "B4" 1) 2.948934240362812
  , MoodIdx (Tuple "B4" 2) 7.796099773242631
  , MoodIdx (Tuple "B4" 3) 5.741496598639456
  , MoodIdx (Tuple "C5" 0) 2.815419501133787
  , MoodIdx (Tuple "C5" 1) 0.6675736961451247
  , MoodIdx (Tuple "C5" 2) 5.955918367346939
  , MoodIdx (Tuple "C5" 3) 5.381224489795918
  , MoodIdx (Tuple "C#5" 0) 9.102222222222222
  , MoodIdx (Tuple "C#5" 1) 3.465578231292517
  , MoodIdx (Tuple "C#5" 2) 2.6238548752834467
  , MoodIdx (Tuple "C#5" 3) 4.284081632653061
  , MoodIdx (Tuple "D5" 0) 3.1579138321995464
  , MoodIdx (Tuple "D5" 1) 1.787936507936508
  , MoodIdx (Tuple "D5" 2) 8.695873015873016
  , MoodIdx (Tuple "D5" 3) 1.5615419501133787
  , MoodIdx (Tuple "D#5" 0) 4.284081632653061
  , MoodIdx (Tuple "D#5" 1) 6.780226757369615
  , MoodIdx (Tuple "D#5" 2) 6.757006802721088
  , MoodIdx (Tuple "D#5" 3) 1.8169614512471655
  , MoodIdx (Tuple "E5" 0) 8.59718820861678
  , MoodIdx (Tuple "E5" 1) 7.024036281179138
  , MoodIdx (Tuple "E5" 2) 7.7496598639455785
  , MoodIdx (Tuple "E5" 3) 5.932698412698413
  , MoodIdx (Tuple "F#5" 0) 9.189297052154195
  , MoodIdx (Tuple "F#5" 1) 9.549206349206349
  , MoodIdx (Tuple "F#5" 2) 5.143219954648526
  , MoodIdx (Tuple "F#5" 3) 8.858412698412698
  , MoodIdx (Tuple "G5" 0) 6.809251700680272
  , MoodIdx (Tuple "G5" 1) 4.8703854875283445
  , MoodIdx (Tuple "G5" 2) 5.282539682539682
  , MoodIdx (Tuple "G5" 3) 7.999274376417233
  , MoodIdx (Tuple "A5" 0) 4.14827664399093
  , MoodIdx (Tuple "A5" 1) 6.826666666666667
  , MoodIdx (Tuple "A5" 2) 17.925804988662133
  , MoodIdx (Tuple "A5" 3) 15.696689342403628
  , MoodIdx (Tuple "B5" 0) 2.3858503401360545
  , MoodIdx (Tuple "B5" 1) 3.465578231292517
  , MoodIdx (Tuple "B5" 2) 3.4771882086167802
  , MoodIdx (Tuple "B5" 3) 3.6048979591836736
  ] ::
    Array MoodIdx

fromSoundsMood :: String -> Int -> Number
fromSoundsMood s i = fromMaybe 0.0 (M.lookup (s <> "-" <> show i) soundsMoodMap)

soundsMoodMap :: M.Map String Number
soundsMoodMap = M.fromFoldable (map (\(MoodIdx (Tuple x y) b) -> Tuple (x <> "-" <> show y) b) soundsMood)

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
  , Tuple "onTheWingsOfEveryKiss6" 6.182312925170068
  ] ::
    Array (Tuple String Number)

fromSoundsLicks :: String -> Number
fromSoundsLicks i = fromMaybe 0.0 (M.lookup i soundsLicksMap)

soundsLicksMap :: M.Map String Number
soundsLicksMap = M.fromFoldable soundsLicks

soundsEnd =
  [ Tuple "endVoice2" 90.0
  , Tuple "endGuitar2" 90.0
  , Tuple "outroOrgan2" 90.0
  , Tuple "lowC" 23.0
  , Tuple "lowA" 23.0
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
  , Tuple "aLight" 1.4164172335600906
  , Tuple "mhl-glitch-0" 5.787573696145125
  , Tuple "mhl-glitch-1" 5.456689342403628
  , Tuple "mhl-glitch-2" 6.30421768707483
  , Tuple "mhl-glitch-3" 7.22140589569161
  , Tuple "mhl-glitch-4" 2.211700680272109
  , Tuple "mhl-glitch-5" 7.836734693877551
  ] ::
    Array (Tuple String Number)

fromSoundsBridge2 :: String -> Number
fromSoundsBridge2 i = fromMaybe 0.0 (M.lookup i soundsBridge2Map)

soundsBridge2Map :: M.Map String Number
soundsBridge2Map = M.fromFoldable soundsBridge2

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
                        s = show $ fst i
                      in
                        Tuple
                          ("In-G4-" <> s <> "-l")
                          ("In/G4/" <> s <> ".l.ogg")
                  )
                  soundsIn
              )
                <> ( map
                      ( \i ->
                          let
                            s = show $ fst i
                          in
                            Tuple
                              ("A-A4-" <> s <> "-l")
                              ("A/A4/" <> s <> ".l.ogg")
                      )
                      soundsA
                  )
                <> ( map
                      ( \i ->
                          let
                            s = show $ fst i
                          in
                            Tuple
                              ("Sen-B4-" <> s <> "-l")
                              ("Sen/B4/" <> s <> ".l.ogg")
                      )
                      soundsSen
                  )
                <> ( map
                      ( \i ->
                          let
                            s = show $ fst i
                          in
                            Tuple
                              ("Ti-D5-" <> s <> "-l")
                              ("Ti/D5/" <> s <> ".l.ogg")
                      )
                      soundsTi
                  )
                <> ( map
                      ( \i ->
                          let
                            s = show $ fst i
                          in
                            Tuple
                              ("Men-E5-" <> s <> "-l")
                              ("Men/E5/" <> s <> ".l.ogg")
                      )
                      soundsMen
                  )
                <> ( map
                      ( \i ->
                          let
                            s = show $ fst i
                          in
                            Tuple
                              ("Tal-G5-" <> s <> "-l")
                              ("Tal/G5/" <> s <> ".l.ogg")
                      )
                      soundsTal
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
                <> ( [ Tuple "Random-glass-bell-rubbing1" "Random/glass-bell-rubbing1.wav.mp3"
                    , Tuple "Random-crotales" "Random/crotales.mp3"
                    , Tuple "Random-one-round-bellhit" "Random/one-round-bellhit.mp3"
                    , Tuple "Random-wind_chimes" "Random/wind_chimes.aif.mp3"
                    , Tuple "Random-reverse-bell-crash" "Random/reverse-bell-crash.wav.mp3"
                    , Tuple "Random-tinkle-bright-bell" "Random/tinkle-bright-bell.wav.mp3"
                    , Tuple "tiny-chimes-1" "Windchime/Smol-Metal---Jangle-6.ogg"
                    , Tuple "metal-chimes-1" "Windchime/Medium-Metal---Jangle-6.ogg"
                    , Tuple "mel-1" "TongueDrum/Melodic-1.ogg"
                    , Tuple "mel-2" "TongueDrum/Melodic-2.ogg"
                    , Tuple "mel-3" "TongueDrum/Melodic-3.ogg"
                    , Tuple "large-strike" "SingingBowls/Large---Strike-1.ogg"
                    , Tuple "Impulses-matrix-reverb3" "Impulses/matrix-reverb3.wav"
                    ]
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
loopDownload :: AudioContext -> String -> Aff BrowserAudioBuffer
loopDownload ctx str =
  res
    >>= either
        ( \e -> do
            -- liftEffect $ log (show e)
            -- a bit buggy, but it gets the job done...
            delay (Milliseconds 20.0)
            loopDownload ctx str
        )
        pure
  where
  res = try $ toAffE (decodeAudioDataFromUri ctx str)

makeBuffersUsingCache :: (O.Object BrowserAudioBuffer -> Tuple (Array (Tuple String String)) (O.Object BrowserAudioBuffer)) -> AudioContext -> O.Object BrowserAudioBuffer -> (O.Object BrowserAudioBuffer -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
makeBuffersUsingCache bf ctx prev' =
  affable do
    sequential
      ( O.union <$> (pure prev)
          <*> ( sequence
                $ O.fromFoldable
                    ( map
                        ( over _2
                            (parallel <<< loopDownload ctx)
                        )
                        (filter (not <<< flip O.member prev <<< fst) newB)
                    )
            )
      )
  where
  (Tuple newB prev) = bf prev'

makeBuffersKeepingCache :: Array (Tuple String String) -> AudioContext -> O.Object BrowserAudioBuffer -> (O.Object BrowserAudioBuffer -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
makeBuffersKeepingCache = makeBuffersUsingCache <<< Tuple

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

conv440 :: Int -> Number
conv440 i = 440.0 * (2.0 `pow` ((toNumber $ 0 + i) / 12.0))

boundPlayer :: Number -> Number -> Lazy (List (AudioUnit D2)) -> List (AudioUnit D2)
boundPlayer len time a = if (time) + kr >= 0.0 && time < (len) then force a else Nil

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

playerDrone :: String -> String -> Number -> Number -> List (AudioUnit D2)
playerDrone tag name prate time =
  if time + kr >= 0.0 && time < 18.0 then
    pure
      $ pannerMono_ (tag <> "_panDrone") 0.0
          ( gainT_' (tag <> "_gainDrone")
              ((epwf [ Tuple 0.0 0.0, Tuple 3.0 (0.4), Tuple 20.0 0.4 ]) time)
              (playBuf_ (tag <> "_playerDrone") name prate)
          )
  else
    Nil

oscSimpl :: String -> Number -> Number -> Number -> List (AudioUnit D2)
oscSimpl tag end freq time =
  if time + kr >= 0.0 && time < end then
    let
      (AudioParameter { param, timeOffset }) =
        ( epwf
            [ Tuple 0.0 0.0, Tuple 3.0 (0.01), Tuple end 0.0 ]
        )
          time
    in
      pure
        $ pannerMono_ (tag <> "_panOscSimpl") 0.0
            ( gainT_' (tag <> "_gainOscSimpl")
                (AudioParameter { param: param + (0.005 * sin (0.1 * pi * time)), timeOffset })
                (sinOsc_ (tag <> "_sinOscSimpl") freq)
            )
  else
    Nil

playerIn :: Int -> (Number -> PlayerInOpts) -> Number -> List (AudioUnit D2)
playerIn name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMonoT_ (opts.tag <> "_panIn") (opts.pan time)
          ( gainT_' (opts.tag <> "_gainIn")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpfIn")
                  (opts.hpff time)
                  (opts.hpfq time)
                  (playBufT_ (opts.tag <> "_playerIn") name (epwf [ Tuple 0.0 1.0, Tuple len (1.0) ] time))
              )
          )
  else
    Nil
  where
  len = (fromSoundsIn name')

  opts = opts' len

  name = "In-G4-" <> show name' <> "-l"

simplIn :: Number -> Int -> String -> String -> Number -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
simplIn os snd tg t2 ps pe fs fe =
  ( atT (0.0 + os)
      $ playerIn snd
          ( \l ->
              { tag: tg <> t2
              , pan: epwf [ Tuple 0.0 ps, Tuple l pe ]
              , gain: epwf [ Tuple 0.0 0.0, Tuple (l - 1.0) 1.0, Tuple (l - 0.6) 0.0, Tuple l 0.0 ]
              , hpff: epwf [ Tuple 0.0 fs, Tuple l fe ]
              , hpfq: epwf [ Tuple 0.0 10.0, Tuple l 1.0 ]
              }
          )
  )

fadeIn :: Number -> String -> Array (Number -> List (AudioUnit D2))
fadeIn os tg =
  [ simplIn (0.0 + os) 0 tg "i0" 0.6 0.0 4000.0 1700.0
  , simplIn (0.5 + os) 3 tg "i1" 0.6 (-0.1) 3500.0 1600.0
  , simplIn (1.0 + os) 5 tg "i2" 0.6 (-0.2) 3000.0 1400.0
  , simplIn (1.5 + os) 0 tg "i3" 0.5 (-0.3) 2500.0 1300.0
  , simplIn (2.0 + os) 3 tg "i4" 0.5 (-0.4) 2000.0 1200.0
  , simplIn (2.5 + os) 5 tg "i6" 0.45 (-0.5) 1700.0 1000.0
  , simplIn (3.0 + os) 0 tg "i7" 0.4 (-0.6) 1500.0 800.0
  , simplIn (3.2 + os) 35 tg "i8" 0.3 (0.5) 1800.0 1600.0
  , simplIn (3.5 + os) 3 tg "i9" 0.3 (-0.7) 1400.0 900.0
  , simplIn (4.0 + os) 5 tg "i10" 0.35 (-0.6) 1500.0 900.0
  , simplIn (4.5 + os) 0 tg "i11" 0.4 (-0.55) 1600.0 1000.0
  , simplIn (5.0 + os) 3 tg "i12" 0.5 (-0.4) 1700.0 1200.0
  , simplIn (5.5 + os) 5 tg "i13" 0.6 (-0.3) 1900.0 1400.0
  , simplIn (6.0 + os) 0 tg "i14" 0.7 (-0.2) 2100.0 1600.0
  , simplIn (6.5 + os) 3 tg "i15" 0.8 (-0.1) 2400.0 1700.0
  , simplIn (7.0 + os) 5 tg "i16" 0.9 (0.0) 2500.0 1900.0
  , simplIn (7.5 + os) 0 tg "i17" 0.9 (0.1) 2600.0 2000.0
  , simplIn (8.0 + os) 3 tg "i18" 0.9 (0.2) 2700.0 2100.0
  , simplIn (8.5 + os) 5 tg "i19" 0.9 (0.3) 2800.0 2200.0
  , simplIn (9.0 + os) 0 tg "i20" 0.9 (0.2) 3000.0 2900.0
  , simplIn (9.5 + os) 3 tg "i21" 0.9 (0.0) 3200.0 3000.0
  , simplIn (10.0 + os) 5 tg "i22" 0.9 (0.0) 3500.0 3400.0
  , simplIn (10.5 + os) 0 tg "i23" 0.9 (-0.1) 3800.0 3500.0
  , simplIn (11.0 + os) 3 tg "i24" 0.9 (-0.2) 4000.0 3700.0
  , simplIn (11.5 + os) 5 tg "i25" 0.9 (-0.3) 4000.0 3700.0
  , simplIn (12.0 + os) 0 tg "i26" 0.9 (-0.4) 4000.0 3700.0
  , simplIn (12.5 + os) 3 tg "i27" 0.9 (-0.5) 4100.0 3700.0
  , simplIn (13.0 + os) 5 tg "i28" 0.9 (-0.6) 4200.0 3700.0
  , simplIn (13.5 + os) 0 tg "i29" 0.9 (-0.7) 4300.0 3700.0
  , simplIn (14.0 + os) 3 tg "i30" 0.9 (-0.8) 4400.0 3800.0
  , simplIn (14.5 + os) 5 tg "i31" 0.9 (-0.9) 4400.0 3900.0
  , simplIn (15.0 + os) 0 tg "i32" 0.9 (-0.9) 4400.0 4000.0
  , simplIn (15.5 + os) 3 tg "i33" 0.9 (-0.9) 4400.0 4000.0
  ]

--------------------
-------------------
--------------
---- A
type PlayerAOpts
  = { tag :: String
    , pan :: Number -> AudioParameter Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

playerA :: Int -> (Number -> PlayerAOpts) -> Number -> List (AudioUnit D2)
playerA name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMonoT_ (opts.tag <> "_panA") (opts.pan time)
          ( gainT_' (opts.tag <> "_gainA")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpfA")
                  (opts.hpff time)
                  (opts.hpfq time)
                  (playBufWithOffset_ (opts.tag <> "_playerA") name 1.0 0.2)
              )
          )
  else
    Nil
  where
  len = (fromSoundsA name')

  opts = opts' len

  name = "A-A4-" <> show name' <> "-l"

data DotInfo
  = DotInfo Int Number Number Number A_Articulation

nDotInfo :: Int -> Number -> Number -> Number -> DotInfo
nDotInfo a b c d = DotInfo a b c d A_Normal

sDotInfo :: Int -> Number -> Number -> Number -> DotInfo
sDotInfo a b c d = DotInfo a b c d A_Stacc

fast = 0.35 :: Number

data A_Articulation
  = A_Normal
  | A_Stacc

aCF = 1000.0 :: Number

aMF = 1500.0 :: Number

aGn = 2.6 :: Number

aDots :: Number -> String -> Array (Number -> List (AudioUnit D2))
aDots os tg =
  map
    ( \(DotInfo x f y z art) ->
        ( atT (y + os)
            $ playerA x
                ( \l ->
                    { tag: tg <> "r" <> (show x) <> (show y)
                    , pan: epwf [ Tuple 0.0 z, Tuple l z ]
                    , gain:
                        epwf
                          [ Tuple 0.0 0.0
                          , Tuple 0.12 aGn
                          , case art of
                              A_Normal -> Tuple l 0.0
                              A_Stacc -> Tuple 0.6 0.0
                          ]
                    , hpff: epwf [ Tuple 0.0 (aCF + (f * aMF)), Tuple l (aCF + (f * aMF)) ]
                    , hpfq: epwf [ Tuple 0.0 1.0, Tuple l 1.0 ]
                    }
                )
        )
    )
    ( foldl (\{ acc, t } e@(DotInfo x f y z a) -> { acc: [ DotInfo x f t z a ] <> acc, t: t + y }) { acc: [], t: 0.0 }
          [ nDotInfo 130 1.0 0.65 0.0
          , nDotInfo 129 0.8 0.6 (0.1)
          , nDotInfo 127 1.0 0.55 (-0.0)
          , nDotInfo 130 1.0 0.5 0.2
          , nDotInfo 129 0.9 0.4 (-0.3)
          , sDotInfo 130 1.0 fast (-0.6)
          , sDotInfo 128 0.9 fast (0.3)
          , sDotInfo 127 0.8 fast (0.1)
          , sDotInfo 129 0.65 fast (0.6)
          , sDotInfo 130 (0.7) fast (-0.3)
          , sDotInfo 129 (0.65) fast (0.5)
          , sDotInfo 128 (0.7) fast (-0.6)
          , sDotInfo 127 (0.65) fast (0.8)
          , sDotInfo 129 (0.7) fast (0.0)
          , sDotInfo 130 (0.65) fast (-0.8)
          , sDotInfo 128 (0.7) fast (0.4)
          , sDotInfo 127 0.65 fast (-0.5)
          , nDotInfo 128 0.7 0.45 (0.2)
          , nDotInfo 127 0.8 0.50 (0.3)
          , nDotInfo 129 0.9 0.55 (0.2)
          , nDotInfo 130 1.0 0.6 (0.1)
          , nDotInfo 129 0.9 0.9 (0.0)
          , nDotInfo 127 0.9 1.2 (-0.1)
          , nDotInfo 128 0.9 1.5 (-0.2)
          , nDotInfo 130 0.9 1.8 (-0.3)
          , nDotInfo 127 0.9 2.0 (-0.4)
          , nDotInfo 127 0.9 2.5 (-0.5)
          , nDotInfo 129 0.8 3.6 (0.1)
          , nDotInfo 127 1.0 5.55 (-0.0)
          , nDotInfo 130 1.0 1.5 0.2
          , nDotInfo 129 0.9 1.0 (-0.3)
          , nDotInfo 130 1.0 0.9 (-0.6)
          , nDotInfo 128 0.9 1.3 (0.3)
          , nDotInfo 127 0.8 1.7 (0.1)
          , nDotInfo 129 0.65 2.3 (0.6)
          ]
      )
      .acc

-----------------------------------------------
-----------------
------- Sen
type PlayerSenOpts
  = { tag :: String
    , offset :: Number
    , pan :: Number -> AudioParameter Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

playerSen :: Int -> (Number -> PlayerSenOpts) -> Number -> List (AudioUnit D2)
playerSen name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMonoT_ (opts.tag <> "_panSen") (opts.pan time)
          ( gainT_' (opts.tag <> "_gainSen")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpfSen")
                  (opts.hpff time)
                  (opts.hpfq time)
                  (playBufWithOffset_ (opts.tag <> "_playerSen") name 1.0 opts.offset)
              )
          )
  else
    Nil
  where
  len = (fromSoundsSen name')

  opts = opts' len

  name = "Sen-B4-" <> show name' <> "-l"

data SenInfo
  = SenInfo Int Number Number SenDir Number Number

data SenEchoInfo
  = SenEchoInfo Int Number Number SenDir

data SenDir
  = SenLeft
  | SenRight

fSI :: (Number -> Number) -> Array SenInfo -> Array SenInfo
fSI f = map (\(SenInfo a b c d e q) -> SenInfo a (f b) c d e q)

quietSen :: Array SenInfo -> Array SenInfo
quietSen = map (\(SenInfo a b c d e f) -> SenInfo a b c d 0.3 0.3)

fSEI :: (Number -> Number) -> Array SenEchoInfo -> Array SenEchoInfo
fSEI f = map (\(SenEchoInfo a b c d) -> SenEchoInfo a (f b) c d)

senInfo :: Array SenInfo
senInfo =
  [ SenInfo 105 0.0 0.0 SenLeft 0.1 0.1
  , SenInfo 104 0.0 0.0 SenRight 0.1 0.1
  , SenInfo 103 0.6 0.0 SenLeft 0.2 0.2
  , SenInfo 102 0.6 0.0 SenRight 0.2 0.2
  , SenInfo 114 1.2 0.0 SenLeft 0.3 0.3
  , SenInfo 108 1.2 0.0 SenRight 0.3 0.3
  , SenInfo 103 1.8 0.0 SenLeft 0.4 0.4
  , SenInfo 102 1.8 0.0 SenRight 0.4 0.4
  , SenInfo 105 2.4 0.0 SenLeft 0.5 0.5
  , SenInfo 104 2.4 0.0 SenRight 0.5 0.5
  , SenInfo 103 3.0 0.0 SenLeft 0.6 0.6
  , SenInfo 102 3.0 0.0 SenRight 0.6 0.6
  , SenInfo 114 3.6 0.0 SenLeft 0.7 0.7
  , SenInfo 108 3.6 0.0 SenRight 0.7 0.7
  , SenInfo 103 4.2 0.0 SenLeft 0.8 0.8
  , SenInfo 102 4.2 0.0 SenRight 0.8 0.8
  ]

senSpread :: Number -> String -> Array SenInfo -> Array (Number -> List (AudioUnit D2))
senSpread os tg si =
  map
    ( \(SenInfo x y o sd gs ge) ->
        ( atT (y + os)
            $ playerSen x
                ( \l ->
                    { tag: tg <> "r" <> (show x) <> (show y)
                    , pan:
                        epwf
                          [ Tuple 0.0 0.0
                          , Tuple l case sd of
                              SenLeft -> (-0.9)
                              SenRight -> 0.9
                          ]
                    , offset: o
                    , gain: epwf [ Tuple 0.0 gs, Tuple l ge ]
                    , hpff: epwf [ Tuple 0.0 (1000.0 + (600.0 * (1.0 - gs))), Tuple l (300.0 + (600.0 * (1.0 - gs))) ]
                    , hpfq: epwf [ Tuple 0.0 1.0, Tuple l 1.0 ]
                    }
                )
        )
    )
    si

senEchoInfo :: Array SenEchoInfo
senEchoInfo =
  [ SenEchoInfo 105 0.3 0.3 SenLeft
  , SenEchoInfo 102 0.9 0.3 SenRight
  , SenEchoInfo 108 1.5 0.3 SenRight
  , SenEchoInfo 103 2.1 0.3 SenLeft
  , SenEchoInfo 105 2.7 0.3 SenLeft
  , SenEchoInfo 104 2.7 0.3 SenRight
  , SenEchoInfo 102 3.3 0.3 SenRight
  , SenEchoInfo 108 3.9 0.3 SenRight
  , SenEchoInfo 103 4.5 0.3 SenLeft
  ]

senEcho :: Number -> String -> Array SenEchoInfo -> Array (Number -> List (AudioUnit D2))
senEcho os tg sei =
  map
    ( \(SenEchoInfo x y o sd) ->
        let
          pz = case sd of
            SenLeft -> (-1.0)
            SenRight -> 1.0
        in
          ( atT (y + os)
              $ playerSen x
                  ( \l ->
                      { tag: tg <> "r" <> (show x) <> (show y)
                      , pan:
                          epwf
                            [ Tuple 0.0 pz
                            , Tuple l (-1.0 * pz)
                            ]
                      , offset: o
                      , gain: epwf [ Tuple 0.0 0.0, Tuple 0.2 0.9, Tuple l 0.2 ]
                      , hpff: epwf [ Tuple 0.0 3000.0, Tuple l 500.0 ]
                      , hpfq: epwf [ Tuple 0.0 1.0, Tuple l 1.0 ]
                      }
                  )
          )
    )
    sei

senArr :: Number -> Array (Number -> List (AudioUnit D2))
senArr os =
  ( (senSpread os "SenA" senInfo)
      <> (senEcho os "SenB" senEchoInfo)
      <> (senSpread os "SenC" $ fSI (\i -> 7.5 - i * 0.4 / 0.6) senInfo)
      <> (senSpread os "SenG" $ fSI (\i -> 8.3 + i * 0.6) (quietSen senInfo))
      <> (senSpread os "SenI" $ fSI (\i -> 11.4 + i * 0.6) (quietSen senInfo))
      <> (senSpread os "SenJ" $ fSI (\i -> 15.0 + i * 0.8) (quietSen senInfo))
      <> (senSpread os "SenK" $ fSI (\i -> 19.0 + i * 1.0) (quietSen senInfo))
  )

-------------------------------
----------------------
---------- Ti
----
type PlayerTiOpts
  = { tag :: String
    , pan :: Number -> Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

playerTi :: Int -> (Number -> PlayerTiOpts) -> Number -> List (AudioUnit D2)
playerTi name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMono_ (opts.tag <> "_panTi") (opts.pan time)
          ( gainT_' (opts.tag <> "_gainTi")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpfTi")
                  (opts.hpff time)
                  (opts.hpfq time)
                  (playBufWithOffset_ (opts.tag <> "_playerTi") name 1.0 0.0)
              )
          )
  else
    Nil
  where
  len = (fromSoundsTi name')

  opts = opts' len

  name = "Ti-D5-" <> show name' <> "-l"

data TiInfo
  = TiInfo Int (Number -> Number)

tiDots :: Number -> String -> Array (Number -> List (AudioUnit D2))
tiDots os tg =
  map
    ( \(TiInfo x pf) ->
        ( atT os
            $ playerTi x
                ( \l ->
                    { tag: tg <> "ti" <> (show x)
                    , pan: pf
                    , gain:
                        epwf
                          [ Tuple 0.0 0.0
                          , Tuple 1.0 1.0
                          , Tuple l 0.0
                          ]
                    , hpff: epwf [ Tuple 0.0 1800.0, Tuple l 400.0 ]
                    , hpfq: epwf [ Tuple 0.0 1.0, Tuple l 1.0 ]
                    }
                )
        )
    )
    [ TiInfo 82 (\t -> sin (t * pi))
    , TiInfo 83 (\t -> cos (t * pi))
    , TiInfo 84 (\t -> -1.0 * cos (t * pi))
    ]

----------------------------
-------------------
----- Men
type PlayerMenOpts
  = { tag :: String
    , offset :: Number
    , pan :: Number -> AudioParameter Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

playerMen :: Int -> (Number -> PlayerMenOpts) -> Number -> List (AudioUnit D2)
playerMen name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMonoT_ (opts.tag <> "_panMen") (opts.pan time)
          ( gainT_' (opts.tag <> "_gainMen")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpfMen")
                  (opts.hpff time)
                  (opts.hpfq time)
                  (playBufWithOffset_ (opts.tag <> "_playerMen") name 1.01 opts.offset)
              )
          )
  else
    Nil
  where
  len = fromSoundsMen name'

  opts = opts' len

  name = "Men-E5-" <> show name' <> "-l"

data MenInfo
  = MenInfo Int Number Number MenDir

data MenDir
  = MenLeft
  | MenRight

menPlayer1 :: Number -> String -> Array (Number -> List (AudioUnit D2))
menPlayer1 os tg =
  map
    ( \(MenInfo x y o sd) ->
        ( atT (y + os)
            $ playerMen x
                ( \l ->
                    let
                      pn = case sd of
                        MenLeft -> (-0.9)
                        MenRight -> 0.9
                    in
                      { tag: tg <> "mp2" <> (show x) <> (show y)
                      , pan:
                          epwf
                            [ Tuple 0.0 pn
                            , Tuple l pn
                            ]
                      , offset: o
                      , gain: epwf [ Tuple 0.0 0.0, Tuple l 0.3 ]
                      , hpff: epwf [ Tuple 0.0 3000.0, Tuple l 3000.0 ]
                      , hpfq: epwf [ Tuple 0.0 30.0, Tuple l 30.0 ]
                      }
                )
        )
    )
    [ MenInfo 2 0.0 0.0 MenLeft
    , MenInfo 3 0.0 0.0 MenRight
    ]

menPlayer2 :: Number -> String -> Array (Number -> List (AudioUnit D2))
menPlayer2 os tg =
  map
    ( \(MenInfo x y o sd) ->
        ( atT (y + os)
            $ playerMen x
                ( \l' ->
                    let
                      l = min l' 2.0
                    in
                      { tag: tg <> "mp1" <> (show x) <> (show y)
                      , pan:
                          epwf
                            [ Tuple 0.0 case sd of
                                MenLeft -> (-0.9)
                                MenRight -> 0.9
                            , Tuple l case sd of
                                MenLeft -> 0.9
                                MenRight -> (-0.9)
                            ]
                      , offset: o
                      , gain: epwf [ Tuple 0.0 0.1, Tuple 0.4 0.5, Tuple l 0.0 ]
                      , hpff: epwf [ Tuple 0.0 300.0, Tuple l 2000.0 ]
                      , hpfq: epwf [ Tuple 0.0 1.0, Tuple l 1.0 ]
                      }
                )
        )
    )
    [ MenInfo 95 0.0 0.0 MenLeft
    , MenInfo 82 1.0 0.0 MenRight
    , MenInfo 95 2.0 0.0 MenLeft
    , MenInfo 94 3.0 0.0 MenRight
    , MenInfo 95 4.0 0.0 MenLeft
    , MenInfo 82 5.0 0.0 MenRight
    , MenInfo 94 6.0 0.0 MenLeft
    , MenInfo 94 7.0 0.0 MenRight
    , MenInfo 95 8.0 0.0 MenLeft
    , MenInfo 82 9.0 0.0 MenRight
    , MenInfo 95 10.0 0.0 MenLeft
    , MenInfo 82 11.0 0.0 MenRight
    ]

---------------------------------------
-------------------------
----- Tal
type PlayerTalOpts
  = { tag :: String
    , offset :: Number
    , pan :: Number -> Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

playerTal :: Int -> (Number -> PlayerTalOpts) -> Number -> List (AudioUnit D2)
playerTal name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMono_ (opts.tag <> "_panTal") (opts.pan time)
          ( gainT_' (opts.tag <> "_gainTal")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpfTal")
                  (opts.hpff time)
                  (opts.hpfq time)
                  (playBufWithOffset_ (opts.tag <> "_playerTal") name 1.0 opts.offset)
              )
          )
  else
    Nil
  where
  len = fromSoundsTal name'

  opts = opts' len

  name = "Tal-G5-" <> show name' <> "-l"

data TalInfo
  = TalInfo Int Number Number (Number -> AudioParameter Number)

playerTal_ :: Int -> (Number -> PlayerTalOpts) -> Number -> Behavior (AudioUnit D2)
playerTal_ name opts time = pure $ speaker (zero :| playerTal name opts time)

peak :: Number -> Array (Tuple Number Number)
peak n = [ Tuple n 0.2, Tuple (n + 0.12) 1.0, Tuple (n + 0.23) 1.0, Tuple (n + 0.34) 0.2 ]

talPlayer2 :: Number -> String -> Array (Number -> List (AudioUnit D2))
talPlayer2 os tg =
  map
    ( \(TalInfo x y o gf) ->
        ( atT (y + os)
            $ playerTal x
                ( \l' ->
                    let
                      l = min l' 2.0
                    in
                      { tag: tg <> "mp1" <> (show x) <> (show y)
                      , pan: \t -> 0.5 * sin (0.3 * pi * t)
                      , offset: o
                      , gain: gf
                      , hpff: epwf [ Tuple 0.0 10.0, Tuple l 10.0 ]
                      , hpfq: epwf [ Tuple 0.0 1.0, Tuple l 1.0 ]
                      }
                )
        )
    )
    [ TalInfo 43 0.0 0.0 (epwf (join $ map peak [ 0.0, 0.2, 0.45, 0.7, 1.0, 1.4, 1.9 ]))
    , TalInfo 42 2.5 0.0 (epwf (join $ map (peak <<< (0.2 * _) <<< toNumber) (range 0 24)))
    ]

----------------------------------------
----------------------------------
------------------------
------------ Mood
type PlayerMoodOpts
  = { tag :: String
    , offset :: Number
    , pan :: Number -> Number
    , filt :: FiltSig
    , gain :: Number -> AudioParameter Number
    , bpff :: Number -> AudioParameter Number
    , bpfq :: Number -> AudioParameter Number
    }

playerMood :: String -> Int -> (Number -> PlayerMoodOpts) -> Number -> List (AudioUnit D2)
playerMood pitch name' opts' time =
  if time + kr >= 0.0 && time < (len + 3.0) then
    pure
      $ pannerMono_ (opts.tag <> "_panMood") (opts.pan time)
          ( gainT_' (opts.tag <> "_gainMood")
              (opts.gain time)
              ( opts.filt (opts.tag <> "_bpfMood")
                  (opts.bpff time)
                  (opts.bpfq time)
                  (playBufWithOffset_ (opts.tag <> "_playerMood") name 1.0 opts.offset)
              )
          )
  else
    Nil
  where
  len = fromSoundsMood pitch name'

  opts = opts' len

  name = "Mood-" <> pitch <> "-" <> show name' <> "-l"

data MoodInfo
  = MoodInfo String String Int Number Number FiltSig Number (Number -> Number -> AudioParameter Number) MoodPan

data MoodPan
  = MoodPan Number Number Number

type FiltSig
  = forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch

bypassFilt :: FiltSig
bypassFilt _ _ _ i = i

lowOs = 1.3 :: Number

moodPlayer2 :: Number -> String -> Array (Number -> List (AudioUnit D2))
moodPlayer2 os tg =
  map
    ( \(MoodInfo pitch tgn x y f filt q gf (MoodPan _a _b _c)) ->
        ( atT (y + os)
            $ playerMood pitch x
                ( \l ->
                    { tag: tgn <> tg <> "mp1" <> pitch <> (show x) <> tgn <> tg
                    , pan: (\t -> _a * sin (_b * (t + _c) * pi))
                    , filt
                    , offset: 0.0
                    , gain: gf l
                    , bpff: epwf [ Tuple 0.0 f, Tuple 0.0 f ]
                    , bpfq: epwf [ Tuple 0.0 q, Tuple l q ]
                    }
                )
        )
    )
    ( ( map
          ( \i ->
              let
                x = (toNumber i * 0.1)
              in
                MoodInfo "E3" (show i) 0 (lowOs + (1.65 * (toNumber i)))
                  100.0 -- (conv440 (-29))
                  bandpassT_
                  4.0
                  -- shifted back to make more sense
                  (\l -> epwf [ Tuple 0.0 0.0, Tuple 0.4 (0.9 - x), Tuple (l - 0.3) (0.9 - x), Tuple (l - 0.15) 0.0 ])
                  (MoodPan 0.0 0.2 0.2)
          )
          (range 0 5)
      )
        <> [ MoodInfo "E4" "-" 0 0.8
              300.0 -- (conv440 (-17))
              bypassFilt
              4.0
              (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
              (MoodPan 0.3 0.2 0.2)
          , MoodInfo "F#4" "-" 0 1.45
              400.0 -- (conv440 (-15))
              bypassFilt
              4.0
              (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
              (MoodPan 0.4 0.1 0.35)
          , MoodInfo "G4" "-" 4 1.15
              450.0 --(conv440 (22))
              bypassFilt
              4.0
              (\l -> epwf [ Tuple 0.0 0.4, Tuple (l - 1.0) 0.4, Tuple l 0.0 ])
              (MoodPan 0.1 0.5 (-0.7))
          , MoodInfo "A4" "-" 5 1.3
              500.0 --(conv440 (-12))
              bypassFilt
              4.0
              (\l -> epwf [ Tuple 0.0 0.4, Tuple (l - 1.0) 0.4, Tuple l 0.0 ])
              (MoodPan 0.05 1.0 0.05)
          , MoodInfo "B4" "-" 2 1.0
              550.0 --(conv440 (14))
              bypassFilt
              4.0
              (\l -> epwf [ Tuple 0.0 0.4, Tuple (l - 1.0) 0.4, Tuple l 0.0 ])
              (MoodPan 0.15 0.4 0.0)
          , MoodInfo "C5" "-" 2 1.6
              600.0 --(conv440 (3))
              bypassFilt
              4.0
              (\l -> epwf [ Tuple 0.0 0.4, Tuple (l - 1.0) 0.4, Tuple l 0.0 ])
              (MoodPan 0.6 0.6 (-0.8))
          , MoodInfo "D5" "-" 2 0.4
              650.0 -- (conv440 (-7))
              bypassFilt
              4.0
              (\l -> epwf [ Tuple 0.0 0.4, Tuple (l - 1.0) 0.4, Tuple l 0.0 ])
              (MoodPan 0.5 0.75 0.8)
          , MoodInfo "E5" "-" 0 0.3
              700.0 --(conv440 (7))
              bypassFilt
              3.0
              (\l -> epwf [ Tuple 0.0 0.2, Tuple (l - 1.0) 0.2, Tuple l 0.0 ])
              (MoodPan 0.9 0.15 (-0.2))
          , MoodInfo "F#5" "-" 0 0.3
              800.0 --(conv440 (9))
              bypassFilt
              2.0
              (\l -> epwf [ Tuple 0.0 0.2, Tuple (l - 1.0) 0.2, Tuple l 0.0 ])
              (MoodPan 0.3 0.5 0.9)
          , MoodInfo "G5" "-" 3 0.5
              900.0 --(conv440 (10))
              bypassFilt
              2.0
              (\l -> epwf [ Tuple 0.0 0.3, Tuple l 0.3 ])
              (MoodPan 0.4 0.1 (-0.2))
          , MoodInfo "A5" "-" 2 0.0
              1000.0 --(conv440 0)
              bypassFilt
              0.01
              (\l -> epwf [ Tuple 0.0 1.0, Tuple 7.0 1.0, Tuple l 0.0 ])
              (MoodPan 0.3 0.1 0.5)
          ]
    )

reverbVoice :: forall ch. Pos ch => String -> AudioUnit ch -> AudioUnit ch
reverbVoice tag au = convolver_ tag "Impulses-matrix-reverb3" au

---------------------------
----- Middle
endGainFunction :: Number -> Number -> AudioParameter Number
endGainFunction gl time = ((epwf [ Tuple 0.0 gl, Tuple 30.0 gl, Tuple 38.0 (gl / 2.0), Tuple 53.0 0.0 ]) time)

playerWindChimes :: Number -> List (AudioUnit D2)
playerWindChimes time =
  boundPlayer (40.0) time
    ( defer \_ ->
        pure
          $ panner_ ("Random-wind_chimespn") 0.0
              ( gainT_' ("Random-wind_chimesgn")
                  ( ( epwf
                        [ Tuple 0.0 0.0
                        , Tuple 10.0 0.02
                        , Tuple 20.0 0.005
                        , Tuple 40.0 0.0
                        ]
                    )
                      time
                  )
                  (playBufWithOffset_ ("Random-wind_chimespl") ("Random-wind_chimes") (1.0 + 0.1 + (sin $ 0.1 * pi * time)) 0.0)
              )
    )

playerMel1 :: String -> (Number -> Number) -> Number -> Number -> List (AudioUnit D2)
playerMel1 tag ratef vol time =
  boundPlayer (18.0) time
    ( defer \_ ->
        pure
          $ panner_ ("mel-1-pan" <> tag) (0.5 * sin (0.0))
              ( gainT_' ("mel-1-gn" <> tag)
                  ( ( epwf
                        [ Tuple 0.0 vol
                        , Tuple 17.0 vol
                        , Tuple 18.0 0.0
                        ]
                    )
                      time
                  )
                  ( playBuf_ ("mel-1-impl-0" <> tag) ("mel-1") (ratef time)
                  )
              )
    )

playerCro :: String -> (Number -> Number) -> Number -> Number -> List (AudioUnit D2)
playerCro tag ratef vol time =
  boundPlayer (18.0) time
    ( defer \_ ->
        pure
          $ panner_ ("crotales-1-pan" <> tag) (0.5 * sin (0.0))
              ( gainT_' ("crotales-1-gn" <> tag)
                  ( ( epwf
                        [ Tuple 0.0 0.0
                        , Tuple 3.0 vol
                        , Tuple 6.5 0.0
                        ]
                    )
                      time
                  )
                  ( playBuf_ ("crotales-1-impl-0" <> tag) ("Random-crotales") (ratef time)
                  )
              )
    )

playerMel2 :: String -> (Number -> Number) -> Number -> Number -> List (AudioUnit D2)
playerMel2 tag ratef vol time =
  boundPlayer (18.0) time
    ( defer \_ ->
        pure
          $ panner_ ("mel-2-pan" <> tag) (0.5 * sin (0.0))
              ( gainT_' ("mel-2-gn" <> tag)
                  ( ( epwf
                        [ Tuple 0.0 vol
                        , Tuple 17.0 vol
                        , Tuple 18.0 0.0
                        ]
                    )
                      time
                  )
                  ( playBuf_ ("mel-2-impl-0" <> tag) ("mel-2") (ratef time)
                  )
              )
    )

playerMetalC1 :: String -> (Number -> Number) -> Number -> Number -> List (AudioUnit D2)
playerMetalC1 tag ratef vol time =
  boundPlayer (20.0) time
    ( defer \_ ->
        pure
          $ panner_ ("pnr0" <> tag) 0.0
              ( gainT_' ("mchgn0" <> tag)
                  ( ( epwf
                        [ Tuple 0.0 vol
                        , Tuple 10.0 vol
                        , Tuple 20.0 0.0
                        ]
                    )
                      time
                  )
                  ( playBuf_ ("mchim-0" <> tag) ("metal-chimes-1") (ratef time)
                  )
              )
    )

playerSmol :: String -> (Number -> Number) -> Number -> Number -> List (AudioUnit D2)
playerSmol tag ratef vol time =
  boundPlayer (20.0) time
    ( defer \_ ->
        pure
          $ panner_ ("xpnr0" <> tag) 0.0
              ( gainT_' ("xmchgn0" <> tag)
                  ( ( epwf
                        [ Tuple 0.0 vol
                        , Tuple 10.0 vol
                        , Tuple 20.0 0.0
                        ]
                    )
                      time
                  )
                  ( playBuf_ ("xmchim-0" <> tag) ("tiny-chimes-1") (ratef time)
                  )
              )
    )

playerBowl :: String -> (Number -> Number) -> Number -> Number -> List (AudioUnit D2)
playerBowl tag ratef vol time =
  boundPlayer (20.0) time
    ( defer \_ ->
        pure
          $ panner_ ("npnr0" <> tag) 0.0
              ( gainT_' ("nmchgn0" <> tag)
                  ( ( epwf
                        [ Tuple 0.0 vol
                        , Tuple 10.0 vol
                        , Tuple 20.0 0.0
                        ]
                    )
                      time
                  )
                  ( playBuf_ ("nmchim-0" <> tag) ("large-strike") (ratef time)
                  )
              )
    )

playerWindChimes2 :: Number -> List (AudioUnit D2)
playerWindChimes2 time =
  boundPlayer (10.0) time
    ( defer \_ ->
        pure
          $ panner_ ("Random-wind_chimespnx") 0.0
              ( gainT_' ("Random-wind_chimesgnx")
                  ( ( epwf
                        [ Tuple 0.0 0.03
                        , Tuple 8.0 0.03
                        , Tuple 10.0 0.0
                        ]
                    )
                      time
                  )
                  ( playBufT_ ("Random-wind_chimesplx") ("Random-wind_chimes")
                      ( ( epwf
                            [ Tuple 0.0 1.8
                            , Tuple 8.0 3.0
                            , Tuple 10.0 3.0
                            ]
                        )
                          time
                      )
                  )
              )
    )

playerGlassRub :: Number -> List (AudioUnit D2)
playerGlassRub time =
  boundPlayer (19.8) time
    ( defer \_ ->
        pure
          $ panner_ ("glassRubPanner") 0.0
              ( gainT_' ("glassRubGain")
                  ( ( epwf
                        [ Tuple 0.0 0.0
                        , Tuple 10.0 0.05
                        , Tuple 11.0 0.005
                        , Tuple 12.0 0.07
                        , Tuple 13.0 0.01
                        , Tuple 16.0 0.15
                        , Tuple 19.8 0.0
                        ]
                    )
                      time
                  )
                  (playBufWithOffset_ ("glassRubPlay") ("Random-glass-bell-rubbing1") 1.0 0.0)
              )
    )

playerBellHit :: Number -> List (AudioUnit D2)
playerBellHit time =
  boundPlayer (5.0) time
    ( defer \_ ->
        pure
          $ panner_ ("bhglassRubPanner") 0.0
              ( gainT_' ("bhglassRubGain")
                  ( ( epwf
                        [ Tuple 0.0 0.05
                        , Tuple 5.0 0.00
                        ]
                    )
                      time
                  )
                  (playBufWithOffset_ ("bhglassRubPlay") ("Random-one-round-bellhit") 1.0 0.0)
              )
    )

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
                    ((epwf [ Tuple 0.0 guitarGain, Tuple len guitarGain ]) time)
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
                    (endGainFunction guitarGain time)
                    (playBufWithOffset_ ("guitarEnd" <> "_playerGuitar") ("End-endGuitar2") 1.0 0.0)
                )
      )

playerOrganOutro :: Number -> List (AudioUnit D2)
playerOrganOutro time =
  let
    len = fromMaybe 0.0 (M.lookup "outroOrgan2" soundsEndMap)
  in
    boundPlayer (len + 1.0) time
      ( defer \_ ->
          pure
            $ panner_ ("outroOrgan2" <> "-outroOrgan2") 0.0
                ( gainT_' ("outroOrgan2" <> "-outroOrgan2")
                    (endGainFunction 0.94 time)
                    (playBufWithOffset_ ("outroOrgan2" <> "_playerGuitar") ("End-outroOrgan2") 1.0 0.0)
                )
      )

playerGuitar2 :: String -> Number -> List (AudioUnit D2)
playerGuitar2 tag time =
  boundPlayer (15.0 + 1.0) time
    ( defer \_ ->
        pure
          $ panner_ (tag <> "_panGuitar2") 0.0
              ( gainT_' (tag <> "_gainGuitar2")
                  ((epwf [ Tuple 0.0 0.4, Tuple 0.2 guitarGain, Tuple 15.0 guitarGain ]) time)
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

vocalCompressorA :: forall ch. Pos ch => String -> AudioUnit ch -> AudioUnit ch
vocalCompressorA tag = dynamicsCompressor_ tag (-24.0) (30.0) (7.0) (0.003) (0.25)

vocalCompressor :: forall ch. Pos ch => String -> AudioUnit ch -> AudioUnit ch
vocalCompressor _ = identity

mainHighpass :: forall ch. Pos ch => String -> AudioUnit ch -> AudioUnit ch
mainHighpass tag = highpass_ tag 150.0 1.0

vocalGain = 0.75 :: Number

guitarGain = 0.7 :: Number

reverbBlend = 0.32 :: Number

dryBlend = 0.68 :: Number

playerVoice :: String -> String -> Number -> Number -> List (AudioUnit D2)
playerVoice tag' name tos time =
  boundPlayer (len + 1.0) time
    ( defer \_ ->
        pure
          $ panner_ (tag' <> "_panVoice") 0.0
              ( ( gain_' ("verb'd_voice") reverbBlend
                    ( reverbVoice (tag' <> "rvbVoice")
                        (vx (tag' <> "withReverb"))
                    )
                )
                  + (gain_' ("verb'd_voice") dryBlend (vx (tag' <> "withoutReverb")))
              )
    )
  where
  len = fromMaybe 0.0 (M.lookup name soundsFullMap)

  vx tag =
    ( gainT_' (tag <> "_gainVoice")
        ( ( epwf
              [ Tuple 0.0 vocalGain
              , Tuple 66.5 vocalGain
              , Tuple 67.2 (vocalGain - 0.25)
              , Tuple 75.0 vocalGain
              , Tuple len vocalGain
              ]
          )
            time
        )
        ( vocalCompressor (tag <> "_compressorVoice")
            ( mainHighpass (tag <> "_highpassVoice")
                (playBufWithOffset_ (tag <> "_playerVoice") ("Full-" <> name) 1.0 tos)
            )
        )
    )

playerLowEnd :: String -> String -> Number -> Number -> List (AudioUnit D2)
playerLowEnd tag lc gn time =
  let
    len = fromMaybe 0.0 (M.lookup lc soundsEndMap)
  in
    boundPlayer (len + 1.0) time
      ( defer \_ ->
          pure
            $ panner_ (lc <> tag <> "_panLowCEnd") 0.0
                ( gainT_' (lc <> tag <> "_gainLowCEnd")
                    (endGainFunction gn time)
                    (playBufWithOffset_ (lc <> tag <> "_playerLowCEnd") ("End-" <> lc) 1.0 0.0)
                )
      )

playerVoiceEnd :: Number -> List (AudioUnit D2)
playerVoiceEnd time =
  boundPlayer (len + 1.0) time
    ( defer \_ ->
        pure
          $ panner_ ("endVoice2" <> "_panVoiceEndd") 0.0
              ( ( ( gain_' ("verb'd_voiced") reverbBlend
                      ( reverbVoice ("rvbVoiced")
                          (vx ("withReverb"))
                      )
                  )
                    + (gain_' ("verb'd_voice") dryBlend (vx ("withoutReverb")))
                )
              )
    )
  where
  len = fromMaybe 0.0 (M.lookup "endVoice2" soundsEndMap)

  vx tag =
    ( gainT_' ("endVoice2" <> tag <> "_gainVoiceEnd")
        (endGainFunction vocalGain time)
        ( vocalCompressor ("endVoice2" <> tag <> "_compressorVoiceEnd")
            ( mainHighpass ("endVoice2" <> tag <> "_highpassVoiceEnd")
                (playBufWithOffset_ ("endVoice2" <> tag <> "_playerVoiceEnd") ("End-endVoice2") 1.0 0.0)
            )
        )
    )

playerVoiceIASM :: String -> Number -> Number -> Number -> List (AudioUnit D2)
playerVoiceIASM tag' hpf vol time =
  boundPlayer (len + 1.0) time
    ( defer \_ ->
        pure
          $ panner_ ("endVoice2" <> tag' <> "_panVoiceEnd2") 0.0
              ( ( ( ( gain_' (tag' <> "verb'd_voiced") reverbBlend
                        ( reverbVoice (tag' <> "rvbVoiced")
                            (vx (tag' <> "withReverb"))
                        )
                    )
                      + (gain_' (tag' <> "verb'd_voice") dryBlend (vx (tag' <> "withoutReverb")))
                  )
                )
              )
    )
  where
  len = fromMaybe 0.0 (M.lookup "endVoice2" soundsEndMap)

  vx tag =
    ( gainT_' ("endVoice2" <> tag <> "_gainVoiceEnd2")
        ((epwf [ Tuple 0.0 vol, Tuple len vol ]) time)
        ( vocalCompressor ("endVoice2" <> tag <> "_compressorVoiceEnd2")
            ( highpass_ ("endVoice2" <> tag <> "_highpassVoiceEnd2") hpf 1.0
                (playBufWithOffset_ ("endVoice2" <> tag <> "_playerVoiceEnd2") ("End-inASentimentalMood") 1.0 0.0)
            )
        )
    )

playerVoiceIASMGlitch :: String -> Number -> Number -> Number -> List (AudioUnit D2)
playerVoiceIASMGlitch tag hpf vol time =
  let
    len = fromMaybe 0.0 (M.lookup "inASentimentalMood" soundsEndMap)
  in
    boundPlayer (len + 1.0) time
      ( defer \_ ->
          pure
            $ panner_ ("endVoice2" <> tag <> "_panVoiceEnd2") 0.0
                ( gainT_' ("endVoice2" <> tag <> "_gainVoiceEnd2")
                    ((epwf [ Tuple 0.0 vol, Tuple len vol ]) time)
                    ( vocalCompressor ("endVoice2" <> tag <> "_compressorVoiceEnd2")
                        ( highpass_ ("endVoice2" <> tag <> "_highpassVoiceEnd2") hpf 1.0
                            (playBufWithOffset_ ("endVoice2" <> tag <> "_playerVoiceEnd2") ("End-inASentimentalMood") 1.0 0.0)
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

playerMHL :: (String -> Number) -> String -> String -> String -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerMHL dct pfx tag' name pan hpf vol time =
  let
    tag = tag' <> name
  in
    boundPlayer (len) time
      ( defer \_ ->
          pure
            $ panner_ (tag <> "_panRoseLong") pan
                ( gainT_' (tag <> "_gainRoseLong")
                    ((epwf [ Tuple 0.0 0.2, Tuple 0.11 vol, Tuple 5.5 vol, Tuple 8.2 0.0, Tuple len 0.0 ]) time)
                    ( vocalCompressor (tag <> "_compressorRoseLong")
                        ( highpass_ (tag <> "_highpassRoseLong") hpf 1.0
                            (playBufWithOffset_ (tag <> "_playerRoseLong") (pfx <> name <> "-l") 1.0 0.0)
                        )
                    )
                )
      )
  where
  len = (dct name)

playerRoseLong :: String -> String -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerRoseLong = playerMHL fromSoundsBridge "Bridge-"

playerALight :: String -> String -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerALight = playerMHL fromSoundsBridge2 "Bridge2-"

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

playerIctus :: String -> String -> Number -> Number -> (Number -> Number) -> Number -> Number -> List (AudioUnit D2)
playerIctus tag name len vos rosf tos time =
  boundPlayer (len + 2.0) time
    ( defer \_ ->
        pure
          $ panner_ (tag <> "_panIctus") 0.0
              ( gainT_' (tag <> "_gainIctus")
                  ((epwf [ Tuple 0.0 0.0, Tuple (len / 2.0) vos, Tuple len 0.0 ]) time)
                  ( highpassT_ (tag <> "_highpassIctus") ((epwf [ Tuple 0.0 200.0, Tuple len 3500.0 ]) time)
                      ((epwf [ Tuple 0.0 1.0, Tuple len 1.0 ]) time)
                      (playBufWithOffset_ (tag <> "_playerIctus") (name) (rosf time) tos)
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
  [ Cascade Mood E5 0.2 NoEvent
  , Cascade Mood D5 0.2 NoEvent
  , Cascade Mood C5 0.2 NoEvent
  , Cascade Mood B4 0.2 NoEvent
  , Cascade Mood B5 0.2 NoEvent
  , Cascade Mood A5 0.2 NoEvent
  , Cascade Mood G5 0.2 NoEvent
  , Cascade Mood Fis5 0.2 NoEvent
  , Cascade Mood E5 0.2 NoEvent
  , Cascade Mood D5 0.2 NoEvent
  , Cascade Mood C5 0.2 NoEvent
  , Cascade Mood B4 0.2 NoEvent
  , Cascade Mood A4 0.2 NoEvent
  , Cascade Mood B5 0.2 NoEvent
  , Cascade Mood A5 0.2 NoEvent
  , Cascade Mood G5 0.2 NoEvent
  , Cascade Mood Fis5 0.2 NoEvent
  , Cascade Mood B5 0.2 NoEvent
  , Cascade Mood A5 0.2 NoEvent
  , Cascade Mood G5 0.2 NoEvent
  , Cascade Mood Fis5 0.2 NoEvent
  , Cascade Mood E5 0.2 NoEvent
  , Cascade Mood D5 0.2 NoEvent
  , Cascade Mood C5 0.2 NoEvent
  , Cascade Mood B4 0.2 NoEvent
  , Cascade Mood B5 0.2 NoEvent
  , Cascade Mood A5 0.2 NoEvent
  , Cascade Mood G5 0.2 NoEvent
  , Cascade Mood Fis5 0.2 NoEvent
  , Cascade Mood B5 0.2 NoEvent
  , Cascade Mood A5 0.2 NoEvent
  , Cascade Mood G5 0.2 NoEvent
  , Cascade Mood Fis5 0.2 NoEvent
  , Cascade Mood E5 0.2 NoEvent
  , Cascade Mood D5 0.2 NoEvent
  , Cascade Mood C5 0.2 NoEvent
  , Cascade Mood B4 0.2 NoEvent
  , Cascade Mood B5 0.2 NoEvent
  , Cascade Mood A5 0.2 NoEvent
  , Cascade Mood G5 0.2 NoEvent
  , Cascade Mood Fis5 0.2 NoEvent
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

startAt = 24.0 :: Number

eos = 2.5 :: Number

lightsStart = 27.5 :: Number

roseMult = 1.2 :: Number

sceneA :: Number -> Behavior (AudioUnit D2)
sceneA time = pure $ speaker' (playBuf ("Windchime-" <> "Glass---Jangle-1" <> "-l") 1.0)

-- for Arline Solomon
scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( ( gain_ "globalGain" 2.0
              ( zero
                  :| fold
                      ( map ((#) time)
                          ( [ atT 3.0 $ playerDrone "Indr" "In-G4-78-l" 1.0
                            , atT 7.0 $ oscSimpl "InOsc" 20.0 (conv440 (-14))
                            , atT 3.25 $ playerDrone "Adr" "A-A4-106-l" 1.0
                            , atT 9.0 $ oscSimpl "AOsc" 18.0 (conv440 (-12))
                            , atT 10.0 $ playerDrone "Sendr" "Sen-B4-61-l" 1.0
                            , atT 8.5 $ playerGlassRub
                            , atT 0.5 $ playerWindChimes
                            , atT 15.1 $ playerWindChimes2
                            , atT 24.3 $ oscSimpl "FoundationOsc" 5.0 (conv440 (-17))
                            , atT 11.0 $ oscSimpl "SenOsc" 15.0 (conv440 (-10))
                            , atT 13.0 $ playerDrone "Tidr" "Ti-D5-19-l" 1.0
                            , atT 15.5 $ oscSimpl "TiOsc" 10.0 (conv440 (-7))
                            , atT 16.0 $ playerDrone "Mendr" "Men-E5-3-l" 1.0
                            , atT 18.5 $ oscSimpl "MenOsc" 7.0 (conv440 (-5))
                            , atT 19.0 $ playerDrone "Taldr" "Tal-G5-24-l" 1.0
                            , atT 19.15 $ playerDrone "Indr1" "In-G4-78-l" 1.0
                            , atT 16.0 $ playerDrone "Adr1" "A-A4-106-l" 1.0
                            , atT 20.0 $ playerDrone "Sendr1" "Sen-B4-61-l" 1.0
                            ]
                              <> (fadeIn 0.0 "In")
                              <> (aDots 4.0 "A")
                              <> (senArr 7.0)
                              <> (tiDots 11.9 "Ti1")
                              <> (tiDots 20.0 "Ti2")
                              <> (menPlayer1 16.0 "Men1")
                              <> (menPlayer2 16.0 "Men2")
                              <> (talPlayer2 20.0 "Tal2")
                              <> (moodPlayer2 24.5 "Mood2")
                          )
                      )
              )
          )
            :| ( gain_ "globalGain" 1.0
                  ( zero
                      :| fold
                          ( map ((#) time)
                              ( ( [ atT startAt $ playerVoice "Voice" "voice" 0.0 ]
                                    <> [ atT startAt $ playerGuitar "Guitar" "guitar" 0.0 ]
                                    <> ( map (atT (startAt))
                                          ( (map (\i -> atT (36.5 + (toNumber i * 0.6)) $ playerKiss (show i) (toNumber i * 0.02) (1700.0 + (toNumber i * 200.0))) (range 0 6))
                                              <> [ atT 36.6 $ playerMel1 "a" (const 1.0) 0.1 ]
                                              <> [ atT 43.0 $ playerMel1 "b" (\t -> 2.0 + (0.1 * sin (0.1 * t * pi))) 0.08 ]
                                              <> [ atT 45.6 $ playerMel1 "c" (\t -> 4.0 + (0.1 * sin (0.1 * t * pi))) 0.02 ]
                                              <> [ atT 50.0 $ playerMel2 "d" (const 1.0) 0.1 ]
                                              <> [ atT 50.0 $ playerMel2 "e" (\t -> 4.0 + (0.1 * sin (0.1 * t * pi))) 0.1 ]
                                              <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.0 + (nf * 0.45)) $ playerLights (show i) "Lights-b3-l" 1.0 (1000.0 + (nf * 200.0)) (0.85 - (abs (nf - 5.0) * 0.05))) (range 0 7))
                                              <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.05 + (nf * 0.5)) $ playerLights (show i) "Lights-g2-l" 1.02 (1400.0 + (nf * 200.0)) (0.65 - (abs (nf - 3.0) * 0.05))) (range 0 4))
                                              <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.15 + (nf * 0.6)) $ playerLights (show i) "Lights-e0-l" 1.0 (1500.0 + (nf * 200.0)) (0.45 - (abs (nf - 1.0) * 0.05))) (range 0 3))
                                              <> (map (\i -> let nf = toNumber i in atT (lightsStart + 0.2 + (nf * 0.9)) $ playerLights (show i) "Lights-c2-l" 1.0 (1700.0 + (nf * 200.0)) (0.65 - (nf * 0.05))) (range 0 1))
                                              <> [ atT 33.1 $ playerHarm "c-sharp0" "c-sharp" 1200.0 0.35
                                                , atT 36.0 $ playerHarm "f-sharp0" "f-sharp" 1500.0 0.22
                                                ]
                                              <> [ atT 62.8 $ playerRose ("rose0") "Bridge-rose3-l" 0.7 (2000.0) (1.0 * roseMult)
                                                , atT 63.0 $ playerRose ("rose0") "Bridge-rose2-l" (-0.7) (2000.0) (0.9 * roseMult)
                                                , atT 63.5 $ playerRose ("rose2") "Bridge-rose3-l" (0.2) (1400.0) (0.8 * roseMult)
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
                                              <> ( map (atT 26.0)
                                                    [ atT 62.8 $ playerRose ("xrose0") "Bridge-rose3-l" 0.7 (2000.0) (1.0 * roseMult)
                                                    , atT 63.0 $ playerRose ("xrose0") "Bridge-rose2-l" (-0.7) (2000.0) (0.9 * roseMult)
                                                    , atT 63.5 $ playerRose ("xrose2") "Bridge-rose3-l" (0.2) (1400.0) (0.8 * roseMult)
                                                    , atT 64.2 $ playerRose ("xrose3") "Bridge-rose2-l" (-0.2) (1700.0) (0.7 * roseMult)
                                                    , atT 64.5 $ playerRose ("xrose4") "Bridge-rose3-l" (0.7) (1200.0) (0.7 * roseMult)
                                                    , atT 64.9 $ playerRose ("xrose5") "Bridge-rose2-l" (-0.7) (1000.0) (0.7 * roseMult)
                                                    , atT 65.5 $ playerRose ("xrose6") "Bridge-rose3-l" (0.2) (1400.0) (0.7 * roseMult)
                                                    , atT 65.9 $ playerRose ("xrose7") "Bridge-rose2-l" (-0.2) (1700.0) (0.7 * roseMult)
                                                    , atT 66.2 $ playerRose ("xrose8") "Bridge-rose3-l" (0.7) (1200.0) (0.7 * roseMult)
                                                    , atT 66.5 $ playerRose ("xrose42") "Bridge-rose2-l" (-0.7) (1000.0) (0.7 * roseMult)
                                                    , atT 66.9 $ playerRose ("xrose531") "Bridge-rose4-l" (0.2) (900.0) (0.5 * roseMult)
                                                    , atT 67.3 $ playerRose ("xrose11") "Bridge-rose3-l" (0.7) (1200.0) (0.7 * roseMult)
                                                    , atT 67.7 $ playerRose ("rose12") "Bridge-rose2-l" (-0.7) (1000.0) (0.7 * roseMult)
                                                    , atT 67.9 $ playerRose ("xrose13") "Bridge-rose4-l" (0.2) (900.0) (0.5 * roseMult)
                                                    , atT 68.1 $ playerRose ("zxrose42") "Bridge-rose2-l" (-0.7) (1000.0) (0.5 * roseMult)
                                                    , atT 68.2 $ playerRose ("zxrose531") "Bridge-rose4-l" (0.2) (900.0) (0.5 * roseMult)
                                                    , atT 68.4 $ playerRose ("zxrose11") "Bridge-rose3-l" (0.7) (1200.0) (0.4 * roseMult)
                                                    , atT 68.6 $ playerRose ("zrose12") "Bridge-rose2-l" (-0.7) (1000.0) (0.3 * roseMult)
                                                    , atT 68.9 $ playerRose ("zxrose13") "Bridge-rose4-l" (0.2) (900.0) (0.25 * roseMult)
                                                    , atT 69.1 $ playerRose ("zxrose42") "Bridge-rose2-l" (-0.7) (1000.0) (0.25 * roseMult)
                                                    , atT 69.17 $ playerRose ("zxrose531") "Bridge-rose4-l" (0.2) (900.0) (0.5 * roseMult)
                                                    , atT 69.23 $ playerRose ("zxrose11") "Bridge-rose3-l" (0.7) (1200.0) (0.2 * roseMult)
                                                    , atT 70.33 $ playerRose ("zrose12") "Bridge-rose2-l" (-0.7) (1000.0) (0.2 * roseMult)
                                                    , atT 70.42 $ playerRose ("zxrose13") "Bridge-rose4-l" (0.2) (900.0) (0.15 * roseMult)
                                                    -----------------------------
                                                    ]
                                                )
                                              -- <> [ atT 89.3 $ playerSAS "drips0" "Bridge2-myHeartsALighterThingSinceYouMadeThisNightAThingDivine-l" 11.0 (-0.0) 1500.0 1.0]
                                              
                                              -- guitar fill
                                              
                                              <> [ atT 84.5 $ playerMetalC1 "mcla" (\t -> 1.0 + (0.1 * sin (0.1 * t * pi))) 0.08 ]
                                              <> [ atT 84.0 $ playerBowl "up" (\t -> (0.8 + (-0.01 * (sin $ pi * t)))) 0.2
                                                , atT 84.0 $ playerBowl "down" (\t -> (0.8 + (-0.01 * (cos $ pi * t)))) 0.12
                                                ]
                                              <> [ atT 85.5 $ playerALight ("aLight0") "aLight" (-0.5) (1500.0) (0.6)
                                                , atT 85.9 $ playerALight ("aLight1") "aLight" (0.5) (2000.0) (0.55)
                                                , atT 86.0 $ playerALight ("mhl1") "mhl-glitch-0" (-0.5) (1500.0) (0.4)
                                                , atT 86.3 $ playerALight ("mhl2") "mhl-glitch-0" (0.5) (2000.0) (0.3)
                                                , atT 87.5 $ playerALight ("aLight0") "aLight" (-0.5) (1500.0) (0.6)
                                                , atT 87.9 $ playerALight ("aLight1") "aLight" (0.5) (2000.0) (0.55)
                                                ]
                                              <> [ atT 85.406 $ playerGuitar2 ("guitarHack") ]
                                              <> [ atT 76.506 $ playerRodeFill ("rdfl") ]
                                              <> [ atT 97.6 $ oscSimpl "dominant" 5.7 (conv440 (-19))
                                                ]
                                              <> [ atT 98.2 $ playerVoiceIASM "-a" 150.0 vocalGain
                                                , atT 102.3 $ playerVoiceIASM "-b" 1400.0 0.5
                                                , atT 102.5 $ playerVoiceIASM "-b" 1400.0 0.4
                                                , atT 102.7 $ playerVoiceIASM "-c" 1700.0 0.3
                                                , atT 102.9 $ playerVoiceIASM "-d" 2000.0 0.2
                                                , atT 101.4 $ playerCro "ctz" (const 1.0) 0.25
                                                ]
                                              <> [ atT 98.4 $ playerVoiceIASMGlitch "ga" 1000.0 0.4, atT 98.6 $ playerVoiceIASMGlitch "ga" 1500.0 0.3, atT 98.8 $ playerVoiceIASMGlitch "ga" 2000.0 0.2 ]
                                              <> [ atT 98.2 $ playerDrone "xIndr" "In-G4-78-l" 1.0
                                                , atT 98.5 $ playerDrone "xAdr" "A-A4-106-l" 1.0
                                                , atT 98.7 $ playerDrone "xSendr" "Sen-B4-61-l" 1.0
                                                , atT 99.0 $ playerDrone "xTidr" "Ti-D5-19-l" 1.0
                                                , atT 99.3 $ playerDrone "xMendr" "Men-E5-3-l" 1.0
                                                , atT 99.6 $ playerDrone "xTaldr" "Tal-G5-24-l" 1.0
                                                ]
                                              <> ( mapWithIndex
                                                    (\i (CSN (Cascade a b c _) d e) -> atT (c + 100.0) $ playerIctus (d <> show i) d e (min 0.9 (0.1 + 0.1 * (toNumber i))) (\_ -> 1.0) 0.0)
                                                    (cascadesWithInfoInTime.acc)
                                                )
                                              <> [ atT (98.6 + eos) $ playerLowEnd "a" "lowC" 0.4
                                                , atT (100.7 + eos) $ playerLowEnd "b" "lowC" 0.4
                                                , atT 99.0 $ oscSimpl "cosc" 10.0 (conv440 (-33))
                                                ]
                                              <> [ atT (107.0 + eos) $ playerVoiceEnd ]
                                              <> [ atT (107.0 + eos) $ playerGuitarEnd ]
                                              <> [ atT (107.0 + eos) $ playerOrganOutro ]
                                              <> [ atT (121.6 + eos) $ playerSmol "tinychimesa" (\t -> 1.0 + ((-0.05 * t))) 0.08
                                                , atT (121.9 + eos) $ playerSmol "tinychimesb" (\t -> 1.8 + ((-0.1 * t))) 0.03
                                                , atT (130.5 + eos) $ playerSmol "tinychimesc" (\t -> 1.0 + ((-0.05 * t))) 0.06
                                                , atT (130.8 + eos) $ playerSmol "tinychimesd" (\t -> 1.8 + ((-0.1 * t))) 0.02
                                                ]
                                          )
                                      )
                                )
                              )
                          )
                  )
              )
            : Nil
        )
