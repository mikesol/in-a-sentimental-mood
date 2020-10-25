module Klank.IASM.Tal where

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
import Math (pi, sin)
import Type.Klank.Dev (Buffers, Klank, affable, defaultEngineInfo, klank, makeBuffersKeepingCache)

soundsTal =
  [ Tuple 43 6.315827664399093
  , Tuple 42 6.362267573696145
  , Tuple 32 2.2058956916099772
  , Tuple 31 2.9024943310657596
  , Tuple 24 7.604535147392291
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

-- https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/Tal/G5/43.l.ogg
-- https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/Tal/G5/43.l.ogg
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
                        ("Tal-G5-" <> s <> "-l")
                        ("Tal/G5/" <> s <> ".l.ogg")
                )
                soundsTal
            )
        )
    , run = runInBrowser scene
    }

fromSoundsTalTal :: Int -> Number
fromSoundsTalTal i = fromMaybe 0.0 (M.lookup i soundsTalMap)

soundsTalMap :: M.Map Int Number
soundsTalMap = M.fromFoldable soundsTal

type PlayerTalOpts
  = { tag :: String
    , offset :: Number
    , pan :: Number -> Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

playerTal :: Int -> (Number -> PlayerTalOpts) -> Number -> List (AudioUnit D2)
playerTal name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMono_ (opts.tag <> "_pan") (opts.pan time)
          ( gainT_' (opts.tag <> "_gain")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpf")
                  (opts.hpff time)
                  (opts.hpfq time)
                  (playBufWithOffset_ (opts.tag <> "_playerTal") name 1.0 opts.offset)
              )
          )
  else
    Nil
  where
  len = fromSoundsTalTal name'

  opts = opts' len

  name = "Tal-G5-" <> show name' <> "-l"

data TalInfo
  = TalInfo Int Number Number (Number -> AudioParameter Number)

playerTal_ :: Int -> (Number -> PlayerTalOpts) -> Number -> Behavior (AudioUnit D2)
playerTal_ name opts time = pure $ speaker (zero :| playerTal name opts time)

peak :: Number → Array (Tuple Number Number)
peak n = [ Tuple n 0.2, Tuple (n + 0.05) 1.0, Tuple (n + 0.1) 1.0, Tuple (n + 0.15) 0.2 ]

talPlayer2 :: Number -> String -> Array (Number → List (AudioUnit D2))
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
    [ TalInfo 43 0.0 0.0 (epwf (join $ map peak [ 0.0, 0.2, 0.45, 0.7, 1.0, 1.4, 1.9, 2.7, 3.7, 5.0 ]))
    , TalInfo 42 2.5 0.0 (epwf (join $ map (peak <<< (0.2 * _) <<< toNumber) (range 0 32)))
    , TalInfo 43 7.2 0.0 (epwf (join $ map peak [ 0.0, 0.2, 0.45, 0.7, 1.0, 1.4, 1.9, 2.7, 3.7, 5.0 ]))
    ]

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( zero
            :| fold
                ( map ((#) time)
                    ( (talPlayer2 1.0 "Tal2")
                    )
                )
        )
