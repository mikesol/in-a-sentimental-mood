module Main where

import Prelude
import Data.Foldable (for_)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Class.Console (log)
import In.A.Sentimental.Mood.Info.Ogg (infoMap)

-- utility for data manipulation
inBase :: Array Int
inBase = [ 84, 83, 82 ]

main :: Effect Unit
main = for_ inBase \i → log $ ("Tuple " <> show i <> " " <> show (fromMaybe 0.0 $ M.lookup ("Ti-D5-" <> show i) infoMap) <> ",")
