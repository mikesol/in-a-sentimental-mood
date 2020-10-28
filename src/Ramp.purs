module Klank.IASM.Ramp where

import Prelude
import Control.Promise (toAffE)
import Data.Array (filter, foldl, mapWithIndex, take)
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

soundsRamp =
  [ (Tuple "A-A4-0" 2.511156462585034)
  , (Tuple "A-A4-1" 2.6399092970521543)
  , (Tuple "A-A4-2" 2.317981859410431)
  , (Tuple "A-E4-0" 1.4487301587301586)
  , (Tuple "A-E4-1" 1.3521541950113378)
  , (Tuple "A-E4-2" 1.2112925170068027)
  , (Tuple "A-Fis4-0" 1.1590022675736962)
  , (Tuple "A-Fis4-1" 1.16702947845805)
  , (Tuple "A-Fis4-2" 1.2475283446712018)
  , (Tuple "A-G4-0" 1.3642403628117914)
  , (Tuple "A-G4-1" 1.0302267573696144)
  , (Tuple "In-D4-0" 2.559433106575964)
  , (Tuple "In-E4-0" 2.61578231292517)
  , (Tuple "In-E4-1" 2.6238321995464853)
  , (Tuple "In-E4-2" 2.7244444444444444)
  , (Tuple "In-Fis-3" 2.764671201814059)
  , (Tuple "In-Fis4-0" 3.050408163265306)
  , (Tuple "In-Fis4-1" 2.2777324263038548)
  , (Tuple "In-Fis4-2" 2.7606575963718822)
  , (Tuple "In-G4-0" 0.9577777777777777)
  , (Tuple "In-G4-1" 0.8531519274376417)
  , (Tuple "Men-A4-0" 1.5292290249433107)
  , (Tuple "Men-A4-1" 1.6741043083900227)
  , (Tuple "Men-A4-2" 1.5654421768707483)
  , (Tuple "Men-B4-0" 1.5292290249433107)
  , (Tuple "Men-C5-0" 1.3280045351473924)
  , (Tuple "Men-C5-1" 1.3239909297052155)
  , (Tuple "Men-C5-2" 1.3038775510204081)
  , (Tuple "Men-C5-3" 1.271655328798186)
  , (Tuple "Men-C5-4" 1.2555555555555555)
  , (Tuple "Men-D4-0" 1.464829931972789)
  , (Tuple "Men-D4-1" 1.6499546485260772)
  , (Tuple "Men-D5-0" 1.1911791383219954)
  , (Tuple "Men-D5-1" 1.2805668934240362)
  , (Tuple "Men-D5-2" 1.3924036281179137)
  , (Tuple "Men-D5-3" 1.3159410430839003)
  , (Tuple "Men-D5-4" 1.412517006802721)
  , (Tuple "Men-E4-0" 1.4769160997732427)
  , (Tuple "Men-Fis4-0" 1.3320408163265307)
  , (Tuple "Men-Fis4-1" 1.468843537414966)
  , (Tuple "Men-Fis4-2" 1.36421768707483)
  , (Tuple "Men-G4-0" 1.2193650793650794)
  , (Tuple "Men-G4-1" 1.3199773242630386)
  , (Tuple "Sen-A4-0" 1.7626303854875283)
  , (Tuple "Sen-A4-1" 1.7384807256235828)
  , (Tuple "Sen-A4-2" 1.6901814058956917)
  , (Tuple "Sen-A4-3" 1.5131292517006802)
  , (Tuple "Sen-B4-0" 1.782766439909297)
  , (Tuple "Sen-B4-1" 1.6499546485260772)
  , (Tuple "Sen-B4-2" 2.2898185941043083)
  , (Tuple "Sen-E4-0" 1.4044671201814058)
  , (Tuple "Sen-E4-1" 1.5694784580498866)
  , (Tuple "Sen-E4-2" 1.6741043083900227)
  , (Tuple "Sen-Fis4-0" 1.609705215419501)
  , (Tuple "Sen-Fis4-1" 1.412517006802721)
  , (Tuple "Sen-G4-0" 1.4769160997732427)
  , (Tuple "Sen-G4-1" 1.360204081632653)
  , (Tuple "Sen-G4-2" 1.505079365079365)
  , (Tuple "Sen-G4-3" 1.5251927437641724)
  , (Tuple "Tal-A4-0" 1.0543764172335601)
  , (Tuple "Tal-A4-1" 1.1469160997732426)
  , (Tuple "Tal-B4-0" 1.1348526077097505)
  , (Tuple "Tal-B4-1" 1.255578231292517)
  , (Tuple "Tal-B4-2" 1.2273922902494332)
  , (Tuple "Tal-C5-0" 1.255578231292517)
  , (Tuple "Tal-C5-1" 1.3843537414965987)
  , (Tuple "Tal-C5-2" 1.215328798185941)
  , (Tuple "Tal-D4-0" 1.4085034013605442)
  , (Tuple "Tal-D4-1" 1.271655328798186)
  , (Tuple "Tal-D5-0" 1.3561904761904762)
  , (Tuple "Tal-D5-1" 0.9859410430839002)
  , (Tuple "Tal-D5-2" 0.9859410430839002)
  , (Tuple "Tal-D5-3" 1.1791156462585033)
  , (Tuple "Tal-E4-0" 1.3400907029478457)
  , (Tuple "Tal-E5-0" 1.311904761904762)
  , (Tuple "Tal-E5-1" 1.16702947845805)
  , (Tuple "Tal-E5-2" 1.1227891156462586)
  , (Tuple "Tal-E5-3" 1.295827664399093)
  , (Tuple "Tal-E5-4" 1.2515646258503401)
  , (Tuple "Tal-E5-5" 1.2877551020408162)
  , (Tuple "Tal-E5-6" 1.2354421768707482)
  , (Tuple "Tal-Fis4-0" 1.1227891156462586)
  , (Tuple "Tal-Fis5-0" 1.2756916099773243)
  , (Tuple "Tal-Fis5-1" 1.3239909297052155)
  , (Tuple "Tal-Fis5-2" 0.965827664399093)
  , (Tuple "Tal-Fis5-3" 1.0463038548752834)
  , (Tuple "Tal-G4-0" 1.2273922902494332)
  , (Tuple "Tal-G4-1" 1.1831292517006802)
  , (Tuple "Tal-G5-0" 1.2112925170068027)
  , (Tuple "Tal-G5-1" 1.2434920634920634)
  , (Tuple "Tal-G5-2" 1.2917913832199546)
  , (Tuple "Tal-G5-3" 1.3722902494331066)
  , (Tuple "Ti-A4-0" 1.4085034013605442)
  , (Tuple "Ti-A4-1" 1.3682539682539683)
  , (Tuple "Ti-A4-2" 1.1469160997732426)
  , (Tuple "Ti-A4-3" 1.1590022675736962)
  , (Tuple "Ti-A4-4" 1.1670521541950114)
  , (Tuple "Ti-B4-0" 1.2725396825396826)
  , (Tuple "Ti-B4-1" 1.3400907029478457)
  , (Tuple "Ti-B4-2" 1.3521541950113378)
  , (Tuple "Ti-B4-3" 1.3119274376417234)
  , (Tuple "Ti-B4-4" 1.2877777777777777)
  , (Tuple "Ti-B4-5" 1.1590022675736962)
  , (Tuple "Ti-C5-0" 1.1951927437641723)
  , (Tuple "Ti-C5-1" 1.4165532879818594)
  , (Tuple "Ti-C5-2" 1.4406802721088436)
  , (Tuple "Ti-C5-3" 1.2636281179138322)
  , (Tuple "Ti-Cis5-0" 1.203265306122449)
  , (Tuple "Ti-Cis5-1" 1.2917913832199546)
  , (Tuple "Ti-Cis5-2" 1.36421768707483)
  , (Tuple "Ti-Cis5-3" 1.8471428571428572)
  , (Tuple "Ti-Cis5-4" 1.2877551020408162)
  , (Tuple "Ti-D5-0" 1.323968253968254)
  , (Tuple "Ti-D5-1" 1.4326530612244899)
  , (Tuple "Ti-D5-2" 1.215328798185941)
  , (Tuple "Ti-Fis4-0" 1.3038548752834467)
  , (Tuple "Ti-Fis4-1" 1.0342403628117913)
  , (Tuple "Ti-G4-0" 1.2837414965986396)
  , (Tuple "Ti-G4-1" 1.376281179138322)
  , (Tuple "Ti-G4-2" 1.3280045351473924)
  , (Tuple "Ti-G4-3" 1.2354648526077097)
  , Tuple "Men-E5-0" 1.8350566893424036
  , Tuple "Men-E5-1" 1.255578231292517
  , Tuple "Men-E5-2" 1.835079365079365
  ] ::
    Array (Tuple String Number)

fromSoundsRamp :: String -> Number
fromSoundsRamp i = fromMaybe 0.0 (M.lookup i soundsRampMap)

soundsRampMap :: M.Map String Number
soundsRampMap = M.fromFoldable soundsRamp

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
                      soundsMood
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

playerIctus :: String -> String -> Number -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerIctus tag name len vos ros tos time =
  if (time) + kr >= 0.0 && time < (len + 1.0) then
    pure
      $ panner_ (tag <> "_panIctus") 0.0
          ( gainT_' (tag <> "_gainIctus")
              ((epwf [ Tuple 0.0 0.0, Tuple 0.15 0.0, Tuple 1.0 vos, Tuple (len - 0.15) 0.0, Tuple len 0.0 ]) time)
              ( highpassT_ (tag <> "_highpassIctus") ((epwf [ Tuple 0.0 200.0, Tuple len 200.0 ]) time)
                  ((epwf [ Tuple 0.0 1.0, Tuple len 1.0 ]) time)
                  (playBufWithOffset_ (tag <> "_playerIctus") (name) ros tos)
              )
          )
  else
    Nil

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

data Cascade
  = Cascade InASentimentalMood Pitch Number

data CSN
  = CSN Cascade String Number

cascades =
  [ Cascade In G4 0.2
  , Cascade In Fis4 0.2
  , Cascade In E4 0.2
  , Cascade In D4 0.2
  , Cascade In G4 0.2
  , Cascade In Fis4 0.2
  , Cascade In E4 0.2
  , Cascade In D4 0.2
  , Cascade A A4 0.2
  , Cascade A G4 0.22
  , Cascade A Fis4 0.24
  , Cascade A E4 0.25
  , Cascade A A4 0.25
  , Cascade A G4 0.25
  , Cascade A Fis4 0.25
  , Cascade A E4 0.25
  , Cascade Sen B4 0.25
  , Cascade Sen A4 0.25
  , Cascade Sen G4 0.25
  , Cascade Sen Fis4 0.25
  , Cascade Sen E4 0.25
  , Cascade Sen B4 0.25
  , Cascade Sen A4 0.25
  , Cascade Sen G4 0.25
  , Cascade Sen Fis4 0.25
  , Cascade Sen E4 0.25
  , Cascade Ti D5 0.25
  , Cascade Ti C5 0.25
  , Cascade Ti B4 0.25
  , Cascade Ti A4 0.25
  , Cascade Ti G4 0.25
  , Cascade Ti Fis4 0.25
  , Cascade Ti D5 0.25
  , Cascade Ti C5 0.25
  , Cascade Ti B4 0.25
  , Cascade Ti A4 0.25
  , Cascade Ti G4 0.25
  , Cascade Ti Fis4 0.25
  , Cascade Men E5 0.25
  , Cascade Men D5 0.25
  , Cascade Men C5 0.25
  , Cascade Men B4 0.25
  , Cascade Men A4 0.25
  , Cascade Men G4 0.25
  , Cascade Men E5 0.25
  , Cascade Men D5 0.25
  , Cascade Men C5 0.25
  , Cascade Men B4 0.25
  , Cascade Men A4 0.25
  , Cascade Men G4 0.25
  , Cascade Tal G5 0.25
  , Cascade Tal Fis5 0.25
  , Cascade Tal E5 0.25
  , Cascade Tal D5 0.25
  , Cascade Tal C5 0.25
  , Cascade Tal B4 0.25
  , Cascade Tal G5 0.25
  , Cascade Tal Fis5 0.25
  , Cascade Tal E5 0.25
  , Cascade Tal D5 0.25
  , Cascade Tal G5 0.25
  , Cascade Tal Fis5 0.25
  , Cascade Tal E5 0.25
  , Cascade Tal D5 0.25
  , Cascade Tal C5 0.25
  , Cascade Tal B4 0.25
  , Cascade Tal G5 0.29
  , Cascade Tal Fis5 0.31
  , Cascade Tal E5 0.33
  , Cascade Tal D5 0.35
  , Cascade Mood B5 0.37
  , Cascade Mood A5 0.35
  , Cascade Mood G5 0.32
  , Cascade Mood Fis5 0.30
  , Cascade Mood E5 0.28
  , Cascade Mood D5 0.25
  , Cascade Mood C5 0.25
  , Cascade Mood B4 0.25
  , Cascade Mood B5 0.25
  , Cascade Mood A5 0.25
  , Cascade Mood G5 0.25
  , Cascade Mood Fis5 0.25
  , Cascade Mood E5 0.25
  , Cascade Mood D5 0.25
  , Cascade Mood C5 0.25
  , Cascade Mood B4 0.25
  , Cascade Mood A4 0.25
  , Cascade Mood B5 0.25
  , Cascade Mood A5 0.25
  , Cascade Mood G5 0.25
  , Cascade Mood Fis5 0.25
  , Cascade Mood B5 0.25
  , Cascade Mood A5 0.25
  , Cascade Mood G5 0.25
  , Cascade Mood Fis5 0.25
  , Cascade Mood E5 0.25
  , Cascade Mood D5 0.25
  , Cascade Mood C5 0.25
  , Cascade Mood B4 0.25
  , Cascade Mood B5 0.25
  , Cascade Mood A5 0.25
  , Cascade Mood G5 0.25
  , Cascade Mood Fis5 0.25
  , Cascade Mood E5 0.25
  , Cascade Mood D5 0.25
  , Cascade Mood C5 0.25
  , Cascade Mood B4 0.25
  , Cascade Mood B5 0.35
  , Cascade Mood A5 0.38
  , Cascade Mood G5 0.41
  , Cascade Mood Fis5 0.44
  , Cascade Mood E5 0.47
  , Cascade Mood D5 0.50
  , Cascade Mood C5 0.53
  , Cascade Mood B4 0.56
  ] ::
    Array Cascade

c2s :: Cascade -> String
c2s c@(Cascade word _ _) =
  ( case word of
      Mood -> ""
      _ -> "Ramp-"
  )
    <> c2s_1 c
    <> "-"
    <> c2s_2 c
    <> "-0-l"

c2sShort :: Cascade -> String
c2sShort c@(Cascade word _ _) =
  c2s_1 c
    <> "-"
    <> c2s_2 c
    <> "-0"

c2s_1 :: Cascade -> String
c2s_1 (Cascade word _ _) = case word of
  In -> "In"
  A -> "A"
  Sen -> "Sen"
  Ti -> "Ti"
  Men -> "Men"
  Tal -> "Tal"
  Mood -> "Mood"

c2s_2 :: Cascade -> String
c2s_2 (Cascade word pitch _) = case word of
  Mood -> p2s_1 pitch
  _ -> p2s_0 pitch

cascadesWithInfo =
  map
    ( \c@(Cascade word pitch t) ->
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
      ( \{ acc, dur } (CSN (Cascade a b c) d e) ->
          { acc: acc <> [ (CSN (Cascade a b $ c + dur) d e) ], dur: dur + c }
      )
      { acc: [], dur: 0.0 }
      cascadesWithInfo
  ) ::
    { acc :: Array CSN, dur :: Number }

conv440 :: Int -> Number
conv440 i = 440.0 * (2.0 `pow` ((toNumber $ 0 + i) / 12.0))

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( zero
            :| fold
                ( map ((#) time)
                    ( ( mapWithIndex (\i (CSN (Cascade a b c) d e) -> atT (c + 1.0) $ playerIctus (d <> show i) d e 1.0 1.0 0.0) (cascadesWithInfoInTime.acc)
                      )
                        <> [ atT (10.0) (\t -> if t > 10.0 then pure (gain_' "gso" 0.03 (pannerMono_ "pso" 0.0 (sinOsc_ "sso" (conv440 (-21))))) else Nil) ]
                        <> [ atT (12.0) (\t -> if t > 10.0 then pure (gain_' "gso1" 0.03 (pannerMono_ "pso1" 0.0 (sinOsc_ "sso1" (conv440 (-24))))) else Nil) ]
                    )
                )
        )
