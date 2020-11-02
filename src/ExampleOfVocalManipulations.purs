module Klank.ExampleOfVocalManipualtions where

import Prelude
import Data.Array (span, head, last)
import Data.Int (toNumber)
import Data.Lens (_1, over)
import Data.Maybe (fromMaybe, maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, defaultParam, gainT', highpassT, playBuf, runInBrowser, speaker')
import Math (pi, sin)
import Type.Klank.Dev (Klank, defaultEngineInfo, klank, makeBuffersKeepingCache)

startVol = 0.0 :: Number -- between 0.0 and 1.0

endVol = 1.0 :: Number -- between 0.0 and 1.0

pitchBend = 1.0 :: Number -- between 0.0 and 1.0

bendSpeed = 0.3 :: Number -- between 0.0 and 1.0

filtStart = 2000.0 :: Number -- between 2000.0 and 100.0

filtEnd = 100.0 :: Number -- between 2000.0 and 100.0

scene :: Number -> Behavior (AudioUnit D1)
scene time =
  pure
    ( speaker'
        ( gainT'
            ( ( epwf
                  [ Tuple 0.0 startVol
                  , Tuple (len - 0.15) endVol
                  , Tuple len 0.0
                  ]
              )
                time
            )
            ( highpassT
                ((epwf [ Tuple 0.0 filtStart, Tuple len filtEnd ]) time)
                ((epwf [ Tuple 0.0 1.0, Tuple len 1.0 ]) time)
                ( playBuf "aud"
                    ( 1.0
                        + ( 0.1 * pitchBend
                              * sin (0.01 + 3.0 * bendSpeed * time)
                          )
                    )
                )
            )
        )
    )
  where
  rad = pi * time

len = 3.0 :: Number

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , buffers =
      makeBuffersKeepingCache
        [ Tuple "aud" "https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/In/G4/0.wav"
        ]
    }

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
