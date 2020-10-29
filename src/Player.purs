module Klank.Dev where

-- New to PureScript? Check out https://www.purescript.org/ for learning resources!
-- To learn more about FRP and the behavior pattern, make sure to check out:
-- • https://github.com/paf31/purescript-behaviors
-- • https://github.com/mikesol/purescript-audio-behaviors
import Prelude
import Data.List ((:), List(..))
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioUnit, gain', playBuf, runInBrowser, sinOsc, speaker, speaker')
import Math (pi, sin, cos)
import Type.Klank.Dev (Klank, klank, makeBuffersKeepingCache)

scene :: Number -> Behavior (AudioUnit D1)
scene time =
  pure
    ( speaker' (playBuf "mel-d" 1.0)
    )
  where
  rad = pi * time

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , buffers =
      makeBuffersKeepingCache
        [ Tuple "mallet-d" "https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/TongueDrum/Roll_D2__1.ogg"
        , Tuple "mel-1" "https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/TongueDrum/Melodic-1.ogg"
        ]
    }
