module Klank.IASM.Men where

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
                        ("Men-E5-" <> s <> "-l")
                        ("Men/E5/" <> s <> ".l.ogg")
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

type PlayerMenOpts
  = { tag :: String
    , offset :: Number
    , pan :: Number -> AudioParameter Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

playerMen :: Int -> (Number -> PlayerMenOpts) -> Number -> List (AudioUnit D2)
playerMen name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMonoT_ (opts.tag <> "_pan") (opts.pan time)
          ( gainT_' (opts.tag <> "_gain")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpf")
                  (opts.hpff time)
                  (opts.hpfq time)
                  (playBufWithOffset_ (opts.tag <> "_playerMen") name 1.0 opts.offset)
              )
          )
  else
    Nil
  where
  len = fromSounds name'

  opts = opts' len

  name = "Men-E5-" <> show name' <> "-l"

data MenInfo
  = MenInfo Int Number Number MenDir

playerMen_ :: Int -> (Number -> PlayerMenOpts) -> Number -> Behavior (AudioUnit D2)
playerMen_ name opts time = pure $ speaker (zero :| playerMen name opts time)

data MenDir
  = MenLeft
  | MenRight

menPlayer1 :: Number -> String -> Array (Number → List (AudioUnit D2))
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

menPlayer2 :: Number -> String -> Array (Number → List (AudioUnit D2))
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
                      , gain: epwf [ Tuple 0.0 1.0, Tuple 0.4 1.0, Tuple l 0.0 ]
                      , hpff: epwf [ Tuple 0.0 300.0, Tuple l 2000.0 ]
                      , hpfq: epwf [ Tuple 0.0 1.0, Tuple l 1.0 ]
                      }
                )
        )
    )
    [ MenInfo 95 0.0 0.0 MenLeft
    , MenInfo 82 1.0 0.0 MenRight
    , MenInfo 93 2.0 0.0 MenLeft
    , MenInfo 94 3.0 0.0 MenRight
    , MenInfo 95 4.0 0.0 MenLeft
    , MenInfo 82 5.0 0.0 MenRight
    , MenInfo 93 6.0 0.0 MenLeft
    , MenInfo 94 7.0 0.0 MenRight
    ]

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( zero
            :| fold
                ( map ((#) time)
                    ( (menPlayer1 1.0 "Men1") <> (menPlayer2 1.0 "Men2")
                    )
                )
        )
