module Klank.IASM.Mood where

import Prelude
import Control.Promise (toAffE)
import Data.Array (filter)
import Data.Array (fold, foldl, head, last, range, span)
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
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, allpassT_, bandpass, bandpassT, bandpassT_, decodeAudioDataFromUri, gain', gainT', gainT_', gain_', highpass, highpassT_, pannerMono, pannerMonoT_, pannerMono_, playBuf, playBufT_, playBufWithOffset_, playBuf_, runInBrowser, sinOsc, speaker, speaker')
import Foreign.Object as O
import Math (pi, pow, sin)
import Type.Klank.Dev (Buffers, Klank, affable, defaultEngineInfo, klank, makeBuffersKeepingCache)

data MoodIdx
  = MoodIdx (Tuple String Int) Number

sounds =
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
  ] ::
    Array MoodIdx

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

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
            ( map
                ( \(MoodIdx (Tuple pitch n) _) ->
                    let
                      s = show n
                    in
                      Tuple
                        ("Mood-" <> pitch <> "-" <> s <> "-l")
                        ("Mood/" <> (replace (Pattern "#") (Replacement "%23") pitch) <> "/" <> s <> ".l.ogg")
                )
                sounds
            )
        )
    , run = runInBrowser scene
    }

fromSounds :: String -> Int -> Number
fromSounds s i = fromMaybe 0.0 (M.lookup (s <> "-" <> show i) soundsMap)

soundsMap :: M.Map String Number
soundsMap = M.fromFoldable (map (\(MoodIdx (Tuple x y) b) -> Tuple (x <> "-" <> show y) b) sounds)

type PlayerMoodOpts
  = { tag :: String
    , offset :: Number
    , pan :: Number -> Number
    , filt :: FiltSig
    , gain :: Number -> AudioParameter Number
    , bpff :: Number -> AudioParameter Number
    , bpfq :: Number -> AudioParameter Number
    }

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

playerMood :: String -> Int -> (Number -> PlayerMoodOpts) -> Number -> List (AudioUnit D2)
playerMood pitch name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMono_ (opts.tag <> "_pan") (opts.pan time)
          ( gainT_' (opts.tag <> "_gain")
              (opts.gain time)
              ( opts.filt (opts.tag <> "_bpf")
                  (opts.bpff time)
                  (opts.bpfq time)
                  (playBufWithOffset_ (opts.tag <> "_playerMood") name 1.0 opts.offset)
              )
          )
  else
    Nil
  where
  len = fromSounds pitch name'

  opts = opts' len

  name = "Mood-" <> pitch <> "-" <> show name' <> "-l"

data MoodInfo
  = MoodInfo String Int Number Number FiltSig Number (Number -> Number -> AudioParameter Number) MoodPan

data MoodPan
  = MoodPan Number Number Number

type FiltSig
  = forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch

moodPlayer2 :: Number -> String -> Array (Number -> List (AudioUnit D2))
moodPlayer2 os tg =
  map
    ( \(MoodInfo pitch x y f filt q gf (MoodPan _a _b _c)) ->
        ( atT (y + os)
            $ playerMood pitch x
                ( \l ->
                    { tag: tg <> "mp1" <> pitch <> (show x) <> (show y)
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
    [ MoodInfo "E3" 0 1.1
        100.0 -- (conv440 (-29))
        bandpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.0 0.2 0.2)
    , MoodInfo "E3" 1 1.7
        100.0 -- (conv440 (-29))
        bandpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.0 0.2 0.2)
    , MoodInfo "E3" 0 2.3
        100.0 -- (conv440 (-29))
        bandpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.0 0.2 0.2)
    , MoodInfo "E3" 2 2.9
        100.0 -- (conv440 (-29))
        bandpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.0 0.2 0.2)
    , MoodInfo "E3" 1 3.5 100.0 -- (conv440 (-29))
        bandpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.0 0.2 0.2)
    , MoodInfo "E3" 0 4.1
        100.0 -- (conv440 (-29))
        bandpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.0 0.2 0.2)
    , MoodInfo "E3" 1 4.7
        100.0 -- (conv440 (-29))
        bandpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.0 0.2 0.2)
    , MoodInfo "E3" 0 5.3
        100.0 -- (conv440 (-29))
        bandpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.0 0.2 0.2)
    , MoodInfo "E3" 0 5.9
        100.0 -- (conv440 (-29))
        bandpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.0 0.2 0.2)
    , MoodInfo "E3" 0 6.5
        100.0 -- (conv440 (-29))
        bandpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.0 0.2 0.2)
    , MoodInfo "E3" 0 7.1
        100.0 -- (conv440 (-29))
        bandpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.0 0.2 0.2)
    , MoodInfo "E3" 0 7.7
        100.0 -- (conv440 (-29))
        bandpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.0 0.2 0.2)
    , MoodInfo "E4" 0 1.0
        300.0 -- (conv440 (-17))
        allpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.3 0.2 0.2)
    , MoodInfo "F#4" 0 0.9
        400.0 -- (conv440 (-15))
        allpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.4 0.1 0.35)
    , MoodInfo "G4" 4 0.8
        450.0 --(conv440 (22))
        allpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.1 0.5 (-0.7))
    , MoodInfo "A4" 5 0.7
        500.0 --(conv440 (-12))
        allpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.05 1.0 0.05)
    , MoodInfo "B4" 2 0.6
        550.0 --(conv440 (14))
        allpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.15 0.4 0.0)
    , MoodInfo "C5" 2 0.5
        600.0 --(conv440 (3))
        allpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.6 0.6 (-0.8))
    , MoodInfo "D5" 2 0.4
        650.0 -- (conv440 (-7))
        allpassT_
        4.0
        (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
        (MoodPan 0.5 0.75 0.8)
    , MoodInfo "E5" 0 0.3
        700.0 --(conv440 (7))
        allpassT_
        3.0
        (\l -> epwf [ Tuple 0.0 0.2, Tuple l 0.2 ])
        (MoodPan 0.9 0.15 (-0.2))
    , MoodInfo "F#5" 0 0.2
        800.0 --(conv440 (9))
        allpassT_
        2.0
        (\l -> epwf [ Tuple 0.0 0.2, Tuple l 0.2 ])
        (MoodPan 0.3 0.5 0.9)
    , MoodInfo "G5" 3 0.1
        900.0 --(conv440 (10))
        allpassT_
        2.0
        (\l -> epwf [ Tuple 0.0 0.3, Tuple l 0.3 ])
        (MoodPan 0.4 0.1 (-0.2))
    , MoodInfo "A5" 2 0.0
        1000.0 --(conv440 0)
        allpassT_
        0.01
        (\l -> epwf [ Tuple 0.0 1.0, Tuple l 1.0 ])
        (MoodPan 0.3 0.1 0.5)
    ]

conv440 :: Int -> Number
conv440 i = 440.0 * (2.0 `pow` ((toNumber $ 0 + i) / 12.0))

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( zero --(pannerMono 0.0 ((gain' 0.03 $ sinOsc 440.0) + (gain' 0.015 $ sinOsc 880.0)))
            :| fold
                ( map ((#) time)
                    ( (moodPlayer2 1.0 "Mood2")
                    )
                )
        )
