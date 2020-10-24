module Klank.IASM.Ti where

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
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, decodeAudioDataFromUri, gain', gainT', gainT_', gain_', highpassT_, pannerMonoT_, pannerMono_, playBuf, playBufT_, playBufWithOffset_, playBuf_, runInBrowser, sinOsc, speaker, speaker')
import Foreign.Object as O
import Math (cos, pi, sin)
import Type.Klank.Dev (Buffers, Klank, affable, defaultEngineInfo, klank, makeBuffersKeepingCache)
import Web.HTML.HTMLMediaElement.CanPlayType (CanPlayType(..))

sounds =
  [ Tuple 84 8.447006802721088
  , Tuple 83 12.203718820861678
  , Tuple 82 11.849886621315193
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
                sounds
            )
        )
    , run = runInBrowser scene
    }

fromSounds :: Int -> Number
fromSounds i = fromMaybe 0.0 (M.lookup i soundsMap)

soundsMap :: M.Map Int Number
soundsMap = M.fromFoldable sounds

type PlayerTiOpts
  = { tag :: String
    , pan :: Number -> Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

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
  len = (fromSounds name')

  opts = opts' len

  name = "Ti-D5-" <> show name' <> "-l"

playerTi_ :: Int -> (Number -> PlayerTiOpts) -> Number -> Behavior (AudioUnit D2)
playerTi_ name opts time = pure $ speaker (zero :| playerTi name opts time)

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
                    , hpfq: epwf [ Tuple 0.0 10.0, Tuple l 1.0 ]
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
                    ( (tiDots 1.0 "Ti") <> (tiDots 10.0 "Ti2")
                    )
                )
        )
