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

soundsSen =
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

fromSoundsSen :: Int -> Number
fromSoundsSen i = fromMaybe 0.0 (M.lookup i soundsSenMap)

soundsSenMap :: M.Map Int Number
soundsSenMap = M.fromFoldable soundsSen

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
                soundsSen
            )
        )
    , run = runInBrowser scene
    }

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

type PlayerSenOpts
  = { tag :: String
    , offset :: Number
    , pan :: Number -> AudioParameter Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

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
  len = (fromSoundsSen name')

  opts = opts' len

  name = "Sen-B4-" <> show name' <> "-l"

data SenInfo
  = SenInfo Int Number Number SenDir Number Number

data SenEchoInfo
  = SenEchoInfo Int Number Number SenDir

data SenDir
  = SenLeft
  | SenRight

fSI :: (Number -> Number) -> Array SenInfo -> Array SenInfo
fSI f = map (\(SenInfo a b c d e q) -> SenInfo a (f b) c d e q)

fSEI :: (Number -> Number) -> Array SenEchoInfo -> Array SenEchoInfo
fSEI f = map (\(SenEchoInfo a b c d) -> SenEchoInfo a (f b) c d)

senInfo :: Array SenInfo
senInfo =
  [ SenInfo 105 0.0 0.0 SenLeft 0.1 0.1
  , SenInfo 104 0.0 0.0 SenRight 0.1 0.1
  , SenInfo 103 0.6 0.0 SenLeft 0.2 0.2
  , SenInfo 102 0.6 0.0 SenRight 0.2 0.2
  , SenInfo 114 1.2 0.0 SenLeft 0.3 0.3
  , SenInfo 108 1.2 0.0 SenRight 0.3 0.3
  , SenInfo 103 1.8 0.0 SenLeft 0.4 0.4
  , SenInfo 102 1.8 0.0 SenRight 0.4 0.4
  , SenInfo 105 2.4 0.0 SenLeft 0.5 0.5
  , SenInfo 104 2.4 0.0 SenRight 0.5 0.5
  , SenInfo 103 3.0 0.0 SenLeft 0.6 0.6
  , SenInfo 102 3.0 0.0 SenRight 0.6 0.6
  , SenInfo 114 3.6 0.0 SenLeft 0.7 0.7
  , SenInfo 108 3.6 0.0 SenRight 0.7 0.7
  , SenInfo 103 4.2 0.0 SenLeft 0.8 0.8
  , SenInfo 102 4.2 0.0 SenRight 0.8 0.8
  ]

senSpread :: Number -> String -> Array SenInfo -> Array (Number → List (AudioUnit D2))
senSpread os tg si =
  map
    ( \(SenInfo x y o sd gs ge) ->
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
                    , gain: epwf [ Tuple 0.0 gs, Tuple l ge ]
                    , hpff: epwf [ Tuple 0.0 (1000.0 + (600.0 * (1.0 - gs))), Tuple l (300.0 + (600.0 * (1.0 - gs))) ]
                    , hpfq: epwf [ Tuple 0.0 1.0, Tuple l 1.0 ]
                    }
                )
        )
    )
    si

senEchoInfo :: Array SenEchoInfo
senEchoInfo =
  [ SenEchoInfo 105 0.3 0.3 SenLeft
  , SenEchoInfo 102 0.9 0.3 SenRight
  , SenEchoInfo 108 1.5 0.3 SenRight
  , SenEchoInfo 103 2.1 0.3 SenLeft
  , SenEchoInfo 105 2.7 0.3 SenLeft
  , SenEchoInfo 104 2.7 0.3 SenRight
  , SenEchoInfo 102 3.3 0.3 SenRight
  , SenEchoInfo 108 3.9 0.3 SenRight
  , SenEchoInfo 103 4.5 0.3 SenLeft
  ]

senEcho :: Number -> String -> Array SenEchoInfo -> Array (Number → List (AudioUnit D2))
senEcho os tg sei =
  map
    ( \(SenEchoInfo x y o sd) ->
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
                            , Tuple l (-1.0 * pz)
                            ]
                      , offset: o
                      , gain: epwf [ Tuple 0.0 0.0, Tuple 0.2 0.9, Tuple l 0.2 ]
                      , hpff: epwf [ Tuple 0.0 3000.0, Tuple l 500.0 ]
                      , hpfq: epwf [ Tuple 0.0 1.0, Tuple l 1.0 ]
                      }
                  )
          )
    )
    sei

senArr :: Number -> Array (Number -> List (AudioUnit D2))
senArr os =
  ( (senSpread os "SenA" senInfo)
      <> (senEcho os "SenB" senEchoInfo)
      <> (senSpread os "SenC" $ fSI (\i -> 6.0 - i * 0.4 / 0.6) senInfo)
      <> (senEcho os "SenD" $ fSEI (\i -> 6.0 - i * 0.4 / 0.6) senEchoInfo)
      <> (senSpread os "SenE" $ fSI (\i -> 6.0 + i * 0.5) senInfo)
      <> (senEcho os "SenF" $ fSEI (\i -> 6.0 + i * 0.5) senEchoInfo)
  )

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( zero
            :| fold
                ( map ((#) time)
                    (senArr 1.0)
                )
        )
