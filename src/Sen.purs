module Klank.IASM.Sen where

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
import Web.HTML.HTMLMediaElement.CanPlayType (CanPlayType(..))

sounds =
  [ Tuple 105 1.3107256235827665
  , Tuple 104 1.1829251700680272
  , Tuple 103 1.0809523809523809
  , Tuple 102 1.2127891156462585
  , Tuple 114 1.1819727891156462
  , Tuple 108 1.2045804988662132
  , Tuple 107 1.227142857142857
  , Tuple 101 1.400294784580499
  , Tuple 98 1.539591836734694
  , Tuple 93 1.434172335600907
  , Tuple 10 1.2178684807256235
  , Tuple 7 2.362607709750567
  , Tuple 4 4.443492063492063
  , Tuple 3 2.1881632653061223
  , Tuple 82 7.25734693877551
  , Tuple 92 9.685442176870747
  , Tuple 90 8.093151927437642
  , Tuple 75 5.5974376417233564
  , Tuple 79 15.730839002267574
  , Tuple 45 5.438820861678004
  , Tuple 116 4.863401360544218
  , Tuple 117 2.148639455782313
  , Tuple 43 2.5426757369614514
  , Tuple 40 2.8945351473922902
  , Tuple 38 2.287482993197279
  , Tuple 30 2.1062585034013606
  , Tuple 28 2.7581179138321996
  , Tuple 12 3.4119047619047618
  , Tuple 9 3.025124716553288
  , Tuple 8 3.214671201814059
  , Tuple 36 3.3765532879818596
  , Tuple 31 2.7516780045351474
  , Tuple 27 5.64655328798186
  , Tuple 26 5.971859410430839
  , Tuple 25 5.974829931972789
  , Tuple 24 5.95562358276644
  , Tuple 22 5.243310657596372
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
                        ("Sen-B4-" <> s <> "-l")
                        ("Sen/B4/" <> s <> ".l.ogg")
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

type PlayerSenOpts
  = { tag :: String
    , offset :: Number
    , pan :: Number -> AudioParameter Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

playerSen :: Int -> (Number -> PlayerSenOpts) -> Number -> List (AudioUnit D2)
playerSen name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMonoT_ (opts.tag <> "_pan") (opts.pan time)
          ( gainT_' (opts.tag <> "_gain")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpf")
                  (opts.hpff time)
                  (opts.hpfq time)
                  (playBufWithOffset_ (opts.tag <> "_playerSen") name 1.0 opts.offset)
              )
          )
  else
    Nil
  where
  len = (fromSounds name')

  opts = opts' len

  name = "Sen-B4-" <> show name' <> "-l"

data SenInfo
  = SenInfo Int Number Number SenDir

playerSen_ :: Int -> (Number -> PlayerSenOpts) -> Number -> Behavior (AudioUnit D2)
playerSen_ name opts time = pure $ speaker (zero :| playerSen name opts time)

data SenDir
  = SenLeft
  | SenRight

senSpread :: Number -> String -> Array (Number → List (AudioUnit D2))
senSpread os tg =
  map
    ( \(SenInfo x y o sd) ->
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
                    , gain: epwf [ Tuple 0.0 1.0, Tuple l 1.0 ]
                    , hpff: epwf [ Tuple 0.0 1000.0, Tuple l 300.0 ]
                    , hpfq: epwf [ Tuple 0.0 10.0, Tuple l 1.0 ]
                    }
                )
        )
    )
    [ SenInfo 105 0.0 0.0 SenLeft
    , SenInfo 104 0.0 0.0 SenRight
    , SenInfo 103 0.6 0.0 SenLeft
    , SenInfo 102 0.6 0.0 SenRight
    , SenInfo 114 1.2 0.0 SenLeft
    , SenInfo 108 1.2 0.0 SenRight
    , SenInfo 103 1.8 0.0 SenLeft
    , SenInfo 102 1.8 0.0 SenRight
    , SenInfo 105 2.4 0.0 SenLeft
    , SenInfo 104 2.4 0.0 SenRight
    , SenInfo 103 3.0 0.0 SenLeft
    , SenInfo 102 3.0 0.0 SenRight
    , SenInfo 114 3.6 0.0 SenLeft
    , SenInfo 108 3.6 0.0 SenRight
    , SenInfo 103 4.2 0.0 SenLeft
    , SenInfo 102 4.2 0.0 SenRight
    ]

senEcho :: Number -> String -> Array (Number → List (AudioUnit D2))
senEcho os tg =
  map
    ( \(SenInfo x y o sd) ->
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
                            , Tuple l 0.0
                            ]
                      , offset: o
                      , gain: epwf [ Tuple 0.0 0.0, Tuple 0.2 0.9, Tuple l 0.2 ]
                      , hpff: epwf [ Tuple 0.0 3000.0, Tuple l 1000.0 ]
                      , hpfq: epwf [ Tuple 0.0 1.0, Tuple l 1.0 ]
                      }
                  )
          )
    )
    [ SenInfo 105 0.3 0.3 SenLeft
    , SenInfo 104 0.3 0.3 SenRight
    , SenInfo 103 0.9 0.3 SenLeft
    , SenInfo 102 0.9 0.3 SenRight
    , SenInfo 114 1.5 0.3 SenLeft
    , SenInfo 108 1.5 0.3 SenRight
    , SenInfo 103 2.1 0.3 SenLeft
    , SenInfo 102 2.1 0.3 SenRight
    , SenInfo 105 2.7 0.3 SenLeft
    , SenInfo 104 2.7 0.3 SenRight
    , SenInfo 103 3.3 0.3 SenLeft
    , SenInfo 102 3.3 0.3 SenRight
    , SenInfo 114 3.9 0.3 SenLeft
    , SenInfo 108 3.9 0.3 SenRight
    , SenInfo 103 4.5 0.3 SenLeft
    , SenInfo 102 4.5 0.3 SenRight
    ]

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( zero
            :| fold
                ( map ((#) time)
                    ( (senSpread 1.0 "A") <> (senEcho 1.0 "B")
                    )
                )
        )
