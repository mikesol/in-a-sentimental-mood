module Klank.IASM.Ti where

import Prelude
import Control.Promise (toAffE)
import Data.Array (filter)
import Data.Array (fold, foldl, head, last, range, span)
import Data.Int (toNumber)
import Data.Lens (_1, _2, over, traversed)
import Data.List ((:), List(..))
import Data.Map as M
import Data.Maybe (fromMaybe, maybe)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1, D2)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter, AudioUnit, defaultParam, gainT_', highpassT_, pannerMono_, playBufWithOffset_, runInBrowser, speaker)
import Foreign.Object as O
import Math (cos, pi, sin)
import Type.Klank.Dev (Buffers, Klank, affable, defaultEngineInfo, klank, makeBuffersKeepingCache)

soundsTi =
  [ Tuple 84 8.447006802721088
  , Tuple 83 12.203718820861678
  , Tuple 82 11.849886621315193
  ] ::
    Array (Tuple Int Number)

fromSoundsTi :: Int -> Number
fromSoundsTi i = fromMaybe 0.0 (M.lookup i soundsTiMap)

soundsTiMap :: M.Map Int Number
soundsTiMap = M.fromFoldable soundsTi

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
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
      defaultParam
        { param = (snd right)
        , timeOffset = (fst right - s)
        }
    else
      let
        m = (snd right - snd left) / (fst right - fst left)

        b = (snd right - (m * fst right))
      in
        defaultParam { param = (m * s + b), timeOffset = 0.0 }

fromCloud :: String -> String
fromCloud s = "https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/" <> s

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
                        ("Ti-D5-" <> s <> "-l")
                        ("Ti/D5/" <> s <> ".l.ogg")
                )
                soundsTi
            )
        )
    , run = runInBrowser scene
    }

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

type PlayerTiOpts
  = { tag :: String
    , pan :: Number -> Number
    , gain :: Number -> AudioParameter
    , hpff :: Number -> AudioParameter
    , hpfq :: Number -> AudioParameter
    }

playerTi :: Int -> (Number -> PlayerTiOpts) -> Number -> List (AudioUnit D2)
playerTi name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMono_ (opts.tag <> "_pan") (opts.pan time)
          ( gainT_' (opts.tag <> "_gain")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpf")
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

fast = 0.17 :: Number

tiDots :: Number -> String -> Array (Number → List (AudioUnit D2))
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
                          [ Tuple 0.0 1.0
                          , Tuple l 1.0
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

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( zero
            :| fold
                ( map ((#) time)
                    ( (tiDots 1.0 "Ti1") <> (tiDots 10.0 "Ti2")
                    )
                )
        )
