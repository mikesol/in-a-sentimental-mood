module Klank.IASM.In where

import Prelude
import Control.Promise (toAffE)
import Data.Array (filter)
import Data.Array (fold, head, last, range, span)
import Data.Int (toNumber)
import Data.Lens (_1, _2, over, traversed)
import Data.List ((:), List(..))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1, D2)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, decodeAudioDataFromUri, gain', gainT', gainT_', gain_', highpassT_, pannerMonoT_, playBuf, playBufT_, playBuf_, runInBrowser, sinOsc, speaker, speaker')
import Foreign.Object as O
import Math (pi, sin)
import Type.Klank.Dev (Buffers, Klank, affable, defaultEngineInfo, klank, makeBuffersKeepingCache)

sounds =
  [ Tuple 24 0.9984580498866213
  , Tuple 22 0.8048299319727891
  , Tuple 23 1.04281179138322
  , Tuple 30 1.3982086167800454
  , Tuple 32 0.6238095238095238
  , Tuple 33 0.7687981859410431
  , Tuple 80 1.1522902494331067
  , Tuple 91 0.7662585034013606
  , Tuple 98 0.7720634920634921
  , Tuple 6 1.2198866213151927
  , Tuple 19 0.816281179138322
  , Tuple 27 5.98204081632653
  , Tuple 28 4.5859410430839
  , Tuple 35 3.224671201814059
  , Tuple 36 3.453968253968254
  , Tuple 49 2.6238548752834467
  , Tuple 68 7.3723356009070296
  , Tuple 69 6.916643990929705
  , Tuple 72 2.7980045351473923
  , Tuple 75 5.822403628117914
  , Tuple 76 3.9299773242630387
  , Tuple 77 2.9663492063492063
  , Tuple 78 17.963537414965987
  , Tuple 0 3.310566893424036
  , Tuple 3 3.8385714285714285
  , Tuple 5 3.0733333333333333
  , Tuple 7 2.7114965986394557
  , Tuple 11 1.2943310657596372
  , Tuple 14 3.669750566893424
  , Tuple 15 3.4256009070294784
  , Tuple 42 2.028843537414966
  , Tuple 37 2.683718820861678
  , Tuple 30 1.3982086167800454
  , Tuple 39 2.780294784580499
  , Tuple 62 1.8234920634920635
  , Tuple 79 0.6630612244897959
  , Tuple 83 0.7871201814058957
  , Tuple 85 0.7746938775510204
  , Tuple 87 0.7589342403628118
  , Tuple 88 0.7049659863945579
  , Tuple 93 0.7396371882086168
  , Tuple 94 0.732766439909297
  , Tuple 102 0.6807256235827664
  , Tuple 103 0.9392743764172335
  , Tuple 104 0.9188208616780046
  ] ::
    Array (Tuple Int Number)

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
                ( \i ->
                    let
                      s = show $ fst i
                    in
                      Tuple
                        ("In-G4-" <> s <> "-l")
                        ("In/G4/" <> s <> ".l.ogg")
                )
                sounds
            )
        )
    , run = runInBrowser scene
    }

fromSounds :: Int -> Number
fromSounds i = fromMaybe 0.0 (M.lookup i soundsMap)

soundsMap :: M.Map Int Number
soundsMap = M.fromFoldable sounds

type PlayerInOpts
  = { tag :: String
    , pan :: Number -> AudioParameter Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

playerIn :: Int -> (Number -> PlayerInOpts) -> Number -> List (AudioUnit D2)
playerIn name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMonoT_ (opts.tag <> "_pan") (opts.pan time)
          ( gainT_' (opts.tag <> "_gain")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpf")
                  (opts.hpff time)
                  (opts.hpfq time)
                  -- pitch shift adds interesting feel
                  -- experiment with keeping or not
                  (playBufT_ (opts.tag <> "_playerIn") name (epwf [ Tuple 0.0 0.98, Tuple len (1.0) ] time))
              )
          )
  else
    Nil
  where
  len = (fromSounds name')

  opts = opts' len

  name = "In-G4-" <> show name' <> "-l"

playerIn_ :: Int -> (Number -> PlayerInOpts) -> Number -> Behavior (AudioUnit D2)
playerIn_ name opts time = pure $ speaker (zero :| playerIn name opts time)

fadeIn :: Number -> String -> Array (Number → List (AudioUnit D2))
fadeIn os tg =
  [ ( atT (0.0 + os)
        $ playerIn 0
            ( \l ->
                { tag: tg <> "i0"
                , pan: epwf [ Tuple 0.0 0.6, Tuple l (0.0) ]
                , gain: epwf [ Tuple 0.0 0.0, Tuple (l - 1.0) 1.0, Tuple (l - 0.6) 0.0, Tuple l 0.0 ]
                , hpff: epwf [ Tuple 0.0 4000.0, Tuple l 1700.0 ]
                , hpfq: epwf [ Tuple 0.0 10.0, Tuple l 1.0 ]
                }
            )
    )
  , ( atT (0.5 + os)
        $ playerIn 3
            ( \l ->
                { tag: tg <> "i1"
                , pan: epwf [ Tuple 0.0 0.6, Tuple l (-0.1) ]
                , gain: epwf [ Tuple 0.0 0.0, Tuple (l - 1.0) 1.0, Tuple (l - 0.6) 0.0, Tuple l 0.0 ]
                , hpff: epwf [ Tuple 0.0 3500.0, Tuple l 1600.0 ]
                , hpfq: epwf [ Tuple 0.0 10.0, Tuple l 1.0 ]
                }
            )
    )
  , ( atT (1.0 + os)
        $ playerIn 3
            ( \l ->
                { tag: tg <> "i2"
                , pan: epwf [ Tuple 0.0 0.6, Tuple l (-0.2) ]
                , gain: epwf [ Tuple 0.0 0.0, Tuple (l - 1.0) 1.0, Tuple (l - 0.6) 0.0, Tuple l 0.0 ]
                , hpff: epwf [ Tuple 0.0 3000.0, Tuple l 1400.0 ]
                , hpfq: epwf [ Tuple 0.0 10.0, Tuple l 1.0 ]
                }
            )
    )
  , ( atT (1.5 + os)
        $ playerIn 0
            ( \l ->
                { tag: tg <> "i3"
                , pan: epwf [ Tuple 0.0 (0.5), Tuple l (-0.3) ]
                , gain: epwf [ Tuple 0.0 0.0, Tuple (l - 1.0) 1.0, Tuple (l - 0.6) 0.0, Tuple l 0.0 ]
                , hpff: epwf [ Tuple 0.0 2500.0, Tuple l 1300.0 ]
                , hpfq: epwf [ Tuple 0.0 10.0, Tuple l 1.0 ]
                }
            )
    )
  ---------------------------------------
  , ( atT (2.0 + os)
        $ playerIn 0
            ( \l ->
                { tag: tg <> "i4"
                , pan: epwf [ Tuple 0.0 0.5, Tuple l (-0.4) ]
                , gain: epwf [ Tuple 0.0 0.0, Tuple (l - 1.0) 1.0, Tuple (l - 0.6) 0.0, Tuple l 0.0 ]
                , hpff: epwf [ Tuple 0.0 2000.0, Tuple l 1400.0 ]
                , hpfq: epwf [ Tuple 0.0 10.0, Tuple l 1.0 ]
                }
            )
    )
  , ( atT (2.5 + os)
        $ playerIn 3
            ( \l ->
                { tag: tg <> "i5"
                , pan: epwf [ Tuple 0.0 0.45, Tuple l (-0.4) ]
                , gain: epwf [ Tuple 0.0 0.0, Tuple (l - 1.0) 1.0, Tuple (l - 0.6) 0.0, Tuple l 0.0 ]
                , hpff: epwf [ Tuple 0.0 2000.0, Tuple l 1300.0 ]
                , hpfq: epwf [ Tuple 0.0 10.0, Tuple l 1.0 ]
                }
            )
    )
  , ( atT (2.8 + os)
        $ playerIn 5
            ( \l ->
                { tag: tg <> "i6"
                , pan: epwf [ Tuple 0.0 0.4, Tuple l (-0.4) ]
                , gain: epwf [ Tuple 0.0 0.0, Tuple (l - 1.0) 1.0, Tuple (l - 0.6) 0.0, Tuple l 0.0 ]
                , hpff: epwf [ Tuple 0.0 2000.0, Tuple l 1200.0 ]
                , hpfq: epwf [ Tuple 0.0 10.0, Tuple l 1.0 ]
                }
            )
    )
  , ( atT (3.0 + os)
        $ playerIn 0
            ( \l ->
                { tag: tg <> "i7"
                , pan: epwf [ Tuple 0.0 (0.35), Tuple l (-0.4) ]
                , gain: epwf [ Tuple 0.0 0.0, Tuple (l - 1.0) 1.0, Tuple (l - 0.6) 0.0, Tuple l 0.0 ]
                , hpff: epwf [ Tuple 0.0 2000.0, Tuple l 1000.0 ]
                , hpfq: epwf [ Tuple 0.0 10.0, Tuple l 1.0 ]
                }
            )
    )
  , ( atT (3.5 + os)
        $ playerIn 35
            ( \l ->
                { tag: tg <> "i9"
                , pan: epwf [ Tuple 0.0 (0.3), Tuple l (0.5) ]
                , gain: epwf [ Tuple 0.0 0.0, Tuple (l - 1.0) 1.0, Tuple (l - 0.6) 0.0, Tuple l 0.0 ]
                , hpff: epwf [ Tuple 0.0 1800.0, Tuple l 1600.0 ]
                , hpfq: epwf [ Tuple 0.0 10.0, Tuple l 1.0 ]
                }
            )
    )
  ]

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( zero
            :| fold
                ( map ((#) time)
                    ( [ ( atT 0.0
                            $ playerIn 78
                                ( \l ->
                                    { tag: "drone"
                                    , pan: epwf [ Tuple 0.0 (-0.8), Tuple l (-0.4) ]
                                    , gain: epwf [ Tuple 0.0 0.0, Tuple 2.0 0.3, Tuple (l - 1.0) 1.0, Tuple (l - 0.6) 0.0, Tuple l 0.0 ]
                                    , hpff: epwf [ Tuple 0.0 800.0, Tuple l 400.0 ]
                                    , hpfq: epwf [ Tuple 0.0 10.0, Tuple l 1.0 ]
                                    }
                                )
                        )
                      ]
                        <> (fadeIn 0.0 "In")
                    )
                )
        )
