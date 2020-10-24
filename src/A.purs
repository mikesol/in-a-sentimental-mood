module Klank.IASM.A where

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
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1, D2)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, decodeAudioDataFromUri, gain', gainT', gainT_', gain_', highpassT_, pannerMonoT_, playBuf, playBufT_, playBufWithOffset_, playBuf_, runInBrowser, sinOsc, speaker, speaker')
import Foreign.Object as O
import Math (pi, sin)
import Type.Klank.Dev (Buffers, Klank, affable, defaultEngineInfo, klank, makeBuffersKeepingCache)

sounds =
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
                        ("A-A4-" <> s <> "-l")
                        ("A/A4/" <> s <> ".l.ogg")
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

type PlayerAOpts
  = { tag :: String
    , pan :: Number -> AudioParameter Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

playerA :: Int -> (Number -> PlayerAOpts) -> Number -> List (AudioUnit D2)
playerA name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMonoT_ (opts.tag <> "_pan") (opts.pan time)
          ( gainT_' (opts.tag <> "_gain")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpf")
                  (opts.hpff time)
                  (opts.hpfq time)
                  (playBufWithOffset_ (opts.tag <> "_playerA") name 1.0 0.2)
              )
          )
  else
    Nil
  where
  len = (fromSounds name')

  opts = opts' len

  name = "A-A4-" <> show name' <> "-l"

playerA_ :: Int -> (Number -> PlayerAOpts) -> Number -> Behavior (AudioUnit D2)
playerA_ name opts time = pure $ speaker (zero :| playerA name opts time)

data DotInfo
  = DotInfo Int Number Number A_Articulation

nDotInfo :: Int -> Number -> Number -> DotInfo
nDotInfo a b c = DotInfo a b c A_Normal

sDotInfo :: Int -> Number -> Number -> DotInfo
sDotInfo a b c = DotInfo a b c A_Stacc

fast = 0.17 :: Number

data A_Articulation
  = A_Normal
  | A_Stacc

aDots :: Number -> String -> Array (Number → List (AudioUnit D2))
aDots os tg =
  map
    ( \(DotInfo x y z art) ->
        ( atT (y + os)
            $ playerA x
                ( \l ->
                    { tag: tg <> "r" <> (show x) <> (show y)
                    , pan: epwf [ Tuple 0.0 z, Tuple l z ]
                    , gain:
                        epwf
                          [ Tuple 0.0 0.0
                          , Tuple 0.1 1.0
                          , case art of
                              A_Normal -> Tuple l 1.0
                              A_Stacc -> Tuple 0.2 0.0
                          ]
                    , hpff: epwf [ Tuple 0.0 600.0, Tuple l 600.0 ]
                    , hpfq: epwf [ Tuple 0.0 1.0, Tuple l 1.0 ]
                    }
                )
        )
    )
    ( foldl (\{ acc, t } e@(DotInfo x y z a) -> { acc: [ DotInfo x t z a ] <> acc, t: t + y }) { acc: [], t: 0.0 }
          [ nDotInfo 130 0.5 0.2
          , nDotInfo 129 0.4 (-0.3)
          , nDotInfo 127 0.35 (0.5)
          , nDotInfo 128 0.3 (-0.7)
          , nDotInfo 130 0.25 (0.2)
          , sDotInfo 129 0.2 (-0.3)
          , sDotInfo 130 fast (-0.6)
          , sDotInfo 128 fast (0.3)
          , sDotInfo 127 fast (0.1)
          , sDotInfo 129 fast (0.6)
          , sDotInfo 130 fast (0.3)
          , sDotInfo 129 fast (0.0)
          , sDotInfo 128 fast (0.1)
          , sDotInfo 127 fast (-0.2)
          , sDotInfo 129 fast (0.0)
          , sDotInfo 130 fast (-0.3)
          , sDotInfo 128 fast (-0.4)
          , sDotInfo 127 fast (-0.5)
          , sDotInfo 128 0.2 (-0.4)
          , sDotInfo 127 0.25 (-0.3)
          , sDotInfo 129 0.3 (-0.15)
          , nDotInfo 130 0.35 (0.0)
          , nDotInfo 129 0.4 (0.1)
          , nDotInfo 128 0.45 (0.2)
          , nDotInfo 127 0.50 (0.3)
          , nDotInfo 129 0.55 (0.2)
          , nDotInfo 130 0.6 (0.1)
          ]
      )
      .acc

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( zero
            :| fold
                ( map ((#) time)
                    ( (aDots 1.0 "A")
                    )
                )
        )
