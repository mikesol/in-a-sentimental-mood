module Main where

import Prelude
import Data.Array (filter)
import Data.Foldable (for_)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), indexOf)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class.Console (log)
import In.A.Sentimental.Mood.Info.Ogg (infoMap)

-- utility for data manipulation
inBase :: Array (Tuple String Int)
inBase =
  join
    ( map (\s -> [ Tuple s 0, Tuple s 1, Tuple s 2, Tuple s 3 ])
        [ "E3", "B3", "E4", "F#4", "G4", "A4", "B4", "C5", "C#5", "D5", "D#5", "E5", "F#5", "G5", "A5", "B5"
        ]
    )

main :: Effect Unit
main = for_ ((filter (\(Tuple s n) -> (indexOf (Pattern "Ramp-") s /= Nothing) && (indexOf (Pattern "-l") s == Nothing) && (indexOf (Pattern "-r") s == Nothing)) (M.toUnfoldable infoMap)) :: Array (Tuple String Number)) (\i → log $ (show i <> ","))
