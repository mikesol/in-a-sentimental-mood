module Klank.IASM.Ramp where

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

soundsRamp =
  [] ::
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
                          ("Ramp-" <> s)
                          ("Ramp/" <> s <> ".ogg")
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

playerIctus :: String -> String -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerIctus tag name vos ros tos time =
  let
    len = fromMaybe 0.0 (M.lookup name soundsRampMap)
  in
    if time + kr >= 0.0 && time < (len + 1.0) then
      pure
        $ panner_ (tag <> "_panIctus") 0.0
            ( gainT_' (tag <> "_gainIctus")
                ((epwf [ Tuple 0.0 vos, Tuple (len - 0.3) vos, Tuple len 0.3 ]) time)
                ( highpass_ "_highpassIctus" 150.0 1.0
                    (playBufWithOffset_ (tag <> "_playerIctus") ("Full-" <> name) ros tos)
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
  | Bes4
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
  | Bes5
  | B5
  | C6
  | Cis6
  | D6

data Cascade
  = Cascade InASentimentalMood Pitch Number

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
  , Cascade Sen B4 0.25
  , Cascade Sen A4 0.25
  , Cascade Sen G4 0.25
  , Cascade Sen Fis4 0.25
  , Cascade Ti D5 0.25
  , Cascade Ti C5 0.25
  , Cascade Ti B4 0.25
  , Cascade Ti A4 0.25
  , Cascade Ti D5 0.25
  , Cascade Ti C5 0.25
  , Cascade Ti B4 0.25
  , Cascade Ti A4 0.25
  , Cascade Men E4 0.25
  , Cascade Men D5 0.25
  , Cascade Men C5 0.25
  , Cascade Men B4 0.25
  , Cascade Men E5 0.25
  , Cascade Men D5 0.25
  , Cascade Men C5 0.25
  , Cascade Men B4 0.25
  , Cascade Tal G5 0.25
  , Cascade Tal Fis5 0.25
  , Cascade Tal E5 0.25
  , Cascade Tal D5 0.25
  , Cascade Tal C5 0.25
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
  ]

--cascade :: Array (Number -> List (AudioUnit D2))
--cascade = map ?hole cascades
scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( zero
            :| fold
                ( map ((#) time)
                    ( [ atT 0.0 $ playerIctus "Voice" "voice" 0.9 1.0 0.0 ]
                    )
                )
        )
