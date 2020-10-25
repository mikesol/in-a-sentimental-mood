module Klank.IASM.In where

import Prelude
import Control.Promise (toAffE)
import Data.Array (filter, foldl)
import Data.Array (fold, head, last, range, span)
import Data.Int (toNumber)
import Data.Lens (_1, _2, over, traversed)
import Data.List ((:), List(..))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (class Pos, D1, D2)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter(..), AudioUnit, EngineInfo, allpassT_, bandpassT_, decodeAudioDataFromUri, gain', gainT', gainT_', gain_', highpassT_, pannerMonoT_, pannerMono_, playBuf, playBufT_, playBufWithOffset_, playBuf_, runInBrowser, sinOsc, speaker, speaker')
import Foreign.Object as O
import Math (cos, pi, pow, sin)
import Type.Klank.Dev (Buffers, Klank, affable, klank, makeBuffersKeepingCache)

iasmEngineInfo =
  { msBetweenSamples: 100
  , msBetweenPings: 95
  , fastforwardLowerBound: 0.025
  , rewindUpperBound: 2.0
  , initialOffset: 0.5
  , doWebAudio: true
  } ::
    EngineInfo

soundsIn =
  [ Tuple 35 3.224671201814059
  , Tuple 78 17.963537414965987
  , Tuple 0 3.310566893424036
  , Tuple 3 3.8385714285714285
  , Tuple 5 3.0733333333333333
  ] ::
    Array (Tuple Int Number)

fromSoundsIn :: Int -> Number
fromSoundsIn i = fromMaybe 0.0 (M.lookup i soundsInMap)

soundsInMap :: M.Map Int Number
soundsInMap = M.fromFoldable soundsIn

soundsA =
  [ Tuple 131 8.642743764172335
  , Tuple 130 0.5119501133786848
  , Tuple 129 0.5608843537414966
  , Tuple 128 0.5382766439909297
  , Tuple 127 0.5382766439909297
  , Tuple 126 0.5834467120181406
  , Tuple 108 0.7509750566893424
  , Tuple 97 3.0095238095238095
  , Tuple 78 1.6204988662131519
  , Tuple 45 0.6662585034013605
  , Tuple 44 0.7208616780045352
  , Tuple 43 0.6361678004535147
  , Tuple 42 3.581700680272109
  , Tuple 35 2.008231292517007
  , Tuple 29 1.2421995464852609
  ] ::
    Array (Tuple Int Number)

fromSoundsA :: Int -> Number
fromSoundsA i = fromMaybe 0.0 (M.lookup i soundsAMap)

soundsAMap :: M.Map Int Number
soundsAMap = M.fromFoldable soundsA

soundsSen =
  [ Tuple 105 1.3107256235827665
  , Tuple 104 1.1829251700680272
  , Tuple 103 1.0809523809523809
  , Tuple 102 1.2127891156462585
  , Tuple 114 1.1819727891156462
  , Tuple 108 1.2045804988662132
  , Tuple 107 1.227142857142857
  , Tuple 101 1.400294784580499
  ] ::
    Array (Tuple Int Number)

fromSoundsSen :: Int -> Number
fromSoundsSen i = fromMaybe 0.0 (M.lookup i soundsSenMap)

soundsSenMap :: M.Map Int Number
soundsSenMap = M.fromFoldable soundsSen

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

soundsMen =
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

fromSoundsMen :: Int -> Number
fromSoundsMen i = fromMaybe 0.0 (M.lookup i soundsMenMap)

soundsMenMap :: M.Map Int Number
soundsMenMap = M.fromFoldable soundsMen

soundsTal =
  [ Tuple 43 6.315827664399093
  , Tuple 42 6.362267573696145
  , Tuple 32 2.2058956916099772
  , Tuple 31 2.9024943310657596
  , Tuple 24 7.604535147392291
  ] ::
    Array (Tuple Int Number)

fromSoundsTal :: Int -> Number
fromSoundsTal i = fromMaybe 0.0 (M.lookup i soundsTalMap)

soundsTalMap :: M.Map Int Number
soundsTalMap = M.fromFoldable soundsTal

data MoodIdx
  = MoodIdx (Tuple String Int) Number

soundsMood =
  [ MoodIdx (Tuple "E3" 0) 1.8808163265306121
  , MoodIdx (Tuple "E3" 1) 1.689251700680272
  , MoodIdx (Tuple "E3" 2) 1.5615419501133787
  , MoodIdx (Tuple "E3" 3) 3.2565986394557824
  , MoodIdx (Tuple "B3" 0) 5.793378684807256
  , MoodIdx (Tuple "B3" 1) 5.31156462585034
  , MoodIdx (Tuple "B3" 2) 4.481451247165533
  , MoodIdx (Tuple "B3" 3) 1.7240816326530612
  , MoodIdx (Tuple "E4" 0) 7.7148299319727895
  , MoodIdx (Tuple "E4" 1) 6.472562358276644
  , MoodIdx (Tuple "E4" 2) 7.302675736961452
  , MoodIdx (Tuple "E4" 3) 6.635102040816326
  , MoodIdx (Tuple "F#4" 0) 4.231836734693878
  , MoodIdx (Tuple "F#4" 1) 4.4117913832199545
  , MoodIdx (Tuple "F#4" 2) 4.6381859410430835
  , MoodIdx (Tuple "F#4" 3) 4.649795918367347
  , MoodIdx (Tuple "G4" 0) 4.818140589569161
  , MoodIdx (Tuple "G4" 1) 3.779047619047619
  , MoodIdx (Tuple "G4" 2) 2.7921995464852607
  , MoodIdx (Tuple "G4" 3) 3.5874829931972787
  , MoodIdx (Tuple "G4" 4) 5.491519274376417
  , MoodIdx (Tuple "A4" 0) 2.885079365079365
  , MoodIdx (Tuple "A4" 1) 3.221768707482993
  , MoodIdx (Tuple "A4" 2) 3.0203628117913834
  , MoodIdx (Tuple "A4" 3) 3.111473922902494
  , MoodIdx (Tuple "A4" 5) 8.765532879818593
  , MoodIdx (Tuple "B4" 0) 1.486077097505669
  , MoodIdx (Tuple "B4" 1) 2.948934240362812
  , MoodIdx (Tuple "B4" 2) 7.796099773242631
  , MoodIdx (Tuple "B4" 3) 5.741496598639456
  , MoodIdx (Tuple "C5" 0) 2.815419501133787
  , MoodIdx (Tuple "C5" 1) 0.6675736961451247
  , MoodIdx (Tuple "C5" 2) 5.955918367346939
  , MoodIdx (Tuple "C5" 3) 5.381224489795918
  , MoodIdx (Tuple "C#5" 0) 9.102222222222222
  , MoodIdx (Tuple "C#5" 1) 3.465578231292517
  , MoodIdx (Tuple "C#5" 2) 2.6238548752834467
  , MoodIdx (Tuple "C#5" 3) 4.284081632653061
  , MoodIdx (Tuple "D5" 0) 3.1579138321995464
  , MoodIdx (Tuple "D5" 1) 1.787936507936508
  , MoodIdx (Tuple "D5" 2) 8.695873015873016
  , MoodIdx (Tuple "D5" 3) 1.5615419501133787
  , MoodIdx (Tuple "D#5" 0) 4.284081632653061
  , MoodIdx (Tuple "D#5" 1) 6.780226757369615
  , MoodIdx (Tuple "D#5" 2) 6.757006802721088
  , MoodIdx (Tuple "D#5" 3) 1.8169614512471655
  , MoodIdx (Tuple "E5" 0) 8.59718820861678
  , MoodIdx (Tuple "E5" 1) 7.024036281179138
  , MoodIdx (Tuple "E5" 2) 7.7496598639455785
  , MoodIdx (Tuple "E5" 3) 5.932698412698413
  , MoodIdx (Tuple "F#5" 0) 9.189297052154195
  , MoodIdx (Tuple "F#5" 1) 9.549206349206349
  , MoodIdx (Tuple "F#5" 2) 5.143219954648526
  , MoodIdx (Tuple "F#5" 3) 8.858412698412698
  , MoodIdx (Tuple "G5" 0) 6.809251700680272
  , MoodIdx (Tuple "G5" 1) 4.8703854875283445
  , MoodIdx (Tuple "G5" 2) 5.282539682539682
  , MoodIdx (Tuple "G5" 3) 7.999274376417233
  , MoodIdx (Tuple "A5" 0) 4.14827664399093
  , MoodIdx (Tuple "A5" 1) 6.826666666666667
  , MoodIdx (Tuple "A5" 2) 17.925804988662133
  , MoodIdx (Tuple "A5" 3) 15.696689342403628
  ] ::
    Array MoodIdx

fromSoundsMood :: String -> Int -> Number
fromSoundsMood s i = fromMaybe 0.0 (M.lookup (s <> "-" <> show i) soundsMoodMap)

soundsMoodMap :: M.Map String Number
soundsMoodMap = M.fromFoldable (map (\(MoodIdx (Tuple x y) b) -> Tuple (x <> "-" <> show y) b) soundsMood)

kr = (toNumber iasmEngineInfo.msBetweenSamples) / 1000.0 :: Number

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
            ( ( map
                  ( \i ->
                      let
                        s = show $ fst i
                      in
                        Tuple
                          ("In-G4-" <> s <> "-l")
                          ("In/G4/" <> s <> ".l.ogg")
                  )
                  soundsIn
              )
                <> ( map
                      ( \i ->
                          let
                            s = show $ fst i
                          in
                            Tuple
                              ("A-A4-" <> s <> "-l")
                              ("A/A4/" <> s <> ".l.ogg")
                      )
                      soundsA
                  )
                <> ( map
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
                <> ( map
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
                <> ( map
                      ( \i ->
                          let
                            s = show $ fst i
                          in
                            Tuple
                              ("Men-E5-" <> s <> "-l")
                              ("Men/E5/" <> s <> ".l.ogg")
                      )
                      soundsMen
                  )
                <> ( map
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
                <> ( map
                      ( \(MoodIdx (Tuple pitch n) _) ->
                          let
                            s = show n
                          in
                            Tuple
                              ("Mood-" <> pitch <> "-" <> s <> "-l")
                              ("Mood/" <> (replace (Pattern "#") (Replacement "%23") pitch) <> "/" <> s <> ".l.ogg")
                      )
                      soundsMood
                  )
            )
        )
    , run = runInBrowser scene
    , engineInfo = iasmEngineInfo
    }

----
-- util
--
atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

------------------------------
-----------------------------
--------------------------
-- In
type PlayerInOpts
  = { tag :: String
    , pan :: Number -> AudioParameter Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

playerIn :: Int -> (Number -> PlayerInOpts) -> Number -> List (AudioUnit D2)
playerIn name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMonoT_ (opts.tag <> "_panIn") (opts.pan time)
          ( gainT_' (opts.tag <> "_gainIn")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpfIn")
                  (opts.hpff time)
                  (opts.hpfq time)
                  -- pitch shift adds interesting feel
                  -- experiment with keeping or not
                  (playBufT_ (opts.tag <> "_playerIn") name (epwf [ Tuple 0.0 0.98, Tuple len (1.0) ] time))
              )
          )
  else
    Nil
  where
  len = (fromSoundsIn name')

  opts = opts' len

  name = "In-G4-" <> show name' <> "-l"

simplIn :: Number -> Int -> String -> String -> Number -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
simplIn os snd tg t2 ps pe fs fe =
  ( atT (0.0 + os)
      $ playerIn snd
          ( \l ->
              { tag: tg <> t2
              , pan: epwf [ Tuple 0.0 ps, Tuple l pe ]
              , gain: epwf [ Tuple 0.0 0.0, Tuple (l - 1.0) 1.0, Tuple (l - 0.6) 0.0, Tuple l 0.0 ]
              , hpff: epwf [ Tuple 0.0 fs, Tuple l fe ]
              , hpfq: epwf [ Tuple 0.0 10.0, Tuple l 1.0 ]
              }
          )
  )

fadeIn :: Number -> String -> Array (Number -> List (AudioUnit D2))
fadeIn os tg =
  [ simplIn (0.0 + os) 0 tg "i0" 0.6 0.0 4000.0 1700.0
  , simplIn (0.5 + os) 3 tg "i1" 0.6 (-0.1) 3500.0 1600.0
  , simplIn (1.0 + os) 5 tg "i2" 0.6 (-0.2) 3000.0 1400.0
  , simplIn (1.5 + os) 0 tg "i3" 0.5 (-0.3) 2500.0 1300.0
  , simplIn (2.0 + os) 3 tg "i4" 0.5 (-0.4) 2000.0 1200.0
  , simplIn (2.5 + os) 5 tg "i6" 0.45 (-0.5) 1700.0 1000.0
  , simplIn (3.0 + os) 0 tg "i7" 0.4 (-0.6) 1500.0 800.0
  , simplIn (3.2 + os) 35 tg "i8" 0.3 (0.5) 1800.0 1600.0
  , simplIn (3.5 + os) 3 tg "i9" 0.3 (-0.7) 1400.0 900.0
  , simplIn (4.0 + os) 5 tg "i10" 0.35 (-0.6) 1500.0 900.0
  , simplIn (4.5 + os) 0 tg "i11" 0.4 (-0.55) 1600.0 1000.0
  , simplIn (5.0 + os) 3 tg "i12" 0.5 (-0.4) 1700.0 1200.0
  , simplIn (5.5 + os) 5 tg "i13" 0.6 (-0.3) 1900.0 1400.0
  , simplIn (6.0 + os) 0 tg "i14" 0.7 (-0.2) 2100.0 1600.0
  , simplIn (6.5 + os) 3 tg "i15" 0.8 (-0.1) 2400.0 1700.0
  , simplIn (7.0 + os) 5 tg "i16" 0.9 (0.0) 2500.0 1900.0
  , simplIn (7.5 + os) 0 tg "i17" 0.9 (0.1) 2600.0 2000.0
  , simplIn (8.0 + os) 3 tg "i18" 0.9 (0.2) 2700.0 2100.0
  , simplIn (8.5 + os) 5 tg "i19" 0.9 (0.3) 2800.0 2200.0
  , simplIn (9.0 + os) 0 tg "i20" 0.9 (0.2) 3000.0 2900.0
  , simplIn (9.5 + os) 3 tg "i21" 0.9 (0.0) 3200.0 3000.0
  , simplIn (10.0 + os) 5 tg "i22" 0.9 (0.0) 3500.0 3400.0
  , simplIn (10.5 + os) 0 tg "i23" 0.9 (-0.1) 3800.0 3500.0
  , simplIn (11.0 + os) 3 tg "i24" 0.9 (-0.2) 4000.0 3700.0
  , simplIn (11.5 + os) 5 tg "i25" 0.9 (-0.3) 4000.0 3700.0
  , simplIn (12.0 + os) 0 tg "i26" 0.9 (-0.4) 4000.0 3700.0
  , simplIn (12.5 + os) 3 tg "i27" 0.9 (-0.5) 4100.0 3700.0
  , simplIn (13.0 + os) 5 tg "i28" 0.9 (-0.6) 4200.0 3700.0
  , simplIn (13.5 + os) 0 tg "i29" 0.9 (-0.7) 4300.0 3700.0
  , simplIn (14.0 + os) 3 tg "i30" 0.9 (-0.8) 4400.0 3800.0
  , simplIn (14.5 + os) 5 tg "i31" 0.9 (-0.9) 4400.0 3900.0
  , simplIn (15.0 + os) 0 tg "i32" 0.9 (-0.9) 4400.0 4000.0
  , simplIn (15.5 + os) 3 tg "i33" 0.9 (-0.9) 4400.0 4000.0
  ]

--------------------
-------------------
--------------
---- A
type PlayerAOpts
  = { tag :: String
    , pan :: Number -> AudioParameter Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

playerA :: Int -> (Number -> PlayerAOpts) -> Number -> List (AudioUnit D2)
playerA name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMonoT_ (opts.tag <> "_panA") (opts.pan time)
          ( gainT_' (opts.tag <> "_gainA")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpfA")
                  (opts.hpff time)
                  (opts.hpfq time)
                  (playBufWithOffset_ (opts.tag <> "_playerA") name 1.0 0.2)
              )
          )
  else
    Nil
  where
  len = (fromSoundsA name')

  opts = opts' len

  name = "A-A4-" <> show name' <> "-l"

data DotInfo
  = DotInfo Int Number Number Number A_Articulation

nDotInfo :: Int -> Number -> Number -> Number -> DotInfo
nDotInfo a b c d = DotInfo a b c d A_Normal

sDotInfo :: Int -> Number -> Number -> Number -> DotInfo
sDotInfo a b c d = DotInfo a b c d A_Stacc

fast = 0.17 :: Number

data A_Articulation
  = A_Normal
  | A_Stacc

aCF = 1000.0 :: Number

aMF = 1500.0 :: Number

aGn = 2.0 :: Number

aDots :: Number -> String -> Array (Number → List (AudioUnit D2))
aDots os tg =
  map
    ( \(DotInfo x f y z art) ->
        ( atT (y + os)
            $ playerA x
                ( \l ->
                    { tag: tg <> "r" <> (show x) <> (show y)
                    , pan: epwf [ Tuple 0.0 z, Tuple l z ]
                    , gain:
                        epwf
                          [ Tuple 0.0 0.0
                          , Tuple 0.12 aGn
                          , case art of
                              A_Normal -> Tuple l 0.0
                              A_Stacc -> Tuple 0.4 0.0
                          ]
                    , hpff: epwf [ Tuple 0.0 (aCF + (f * aMF)), Tuple l (aCF + (f * aMF)) ]
                    , hpfq: epwf [ Tuple 0.0 1.0, Tuple l 1.0 ]
                    }
                )
        )
    )
    ( foldl (\{ acc, t } e@(DotInfo x f y z a) -> { acc: [ DotInfo x f t z a ] <> acc, t: t + y }) { acc: [], t: 0.0 }
          [ nDotInfo 130 1.0 0.65 0.8
          , nDotInfo 129 0.4 0.6 (-0.8)
          , nDotInfo 127 1.0 0.55 (0.0)
          , nDotInfo 130 1.0 0.5 0.2
          , nDotInfo 129 0.4 0.4 (-0.3)
          , nDotInfo 127 0.8 0.35 (0.5)
          , nDotInfo 128 0.7 0.3 (-0.7)
          , nDotInfo 130 0.2 0.25 (0.2)
          , sDotInfo 129 0.5 0.2 (-0.3)
          , sDotInfo 130 0.4 fast (-0.6)
          , sDotInfo 128 0.3 fast (0.3)
          , sDotInfo 127 0.2 fast (0.1)
          , sDotInfo 129 0.15 fast (0.6)
          , sDotInfo 130 (0.2) fast (-0.3)
          , sDotInfo 129 (0.45) fast (0.5)
          , sDotInfo 128 (0.2) fast (-0.6)
          , sDotInfo 127 (0.55) fast (0.8)
          , sDotInfo 129 (0.2) fast (0.0)
          , sDotInfo 130 (0.45) fast (-0.8)
          , sDotInfo 128 (0.2) fast (0.4)
          , sDotInfo 127 0.1 fast (-0.5)
          , sDotInfo 128 0.2 0.2 (-0.0)
          , sDotInfo 127 0.3 0.25 (-0.3)
          , sDotInfo 129 0.4 0.3 (-0.15)
          , nDotInfo 130 0.5 0.35 (0.0)
          , nDotInfo 129 0.6 0.4 (0.1)
          , nDotInfo 128 0.7 0.45 (0.2)
          , nDotInfo 127 0.8 0.50 (0.3)
          , nDotInfo 129 0.9 0.55 (0.2)
          , nDotInfo 130 1.0 0.6 (0.1)
          , nDotInfo 129 0.9 0.9 (0.0)
          , nDotInfo 127 0.9 1.2 (-0.1)
          , nDotInfo 128 0.9 1.5 (-0.2)
          , nDotInfo 130 0.9 1.8 (-0.3)
          , nDotInfo 127 0.9 2.0 (-0.4)
          , nDotInfo 127 0.9 2.5 (-0.5)
          ]
      )
      .acc

-----------------------------------------------
-----------------
------- Sen
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
      $ pannerMonoT_ (opts.tag <> "_panSen") (opts.pan time)
          ( gainT_' (opts.tag <> "_gainSen")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpfSen")
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
      <> (senSpread os "SenG" $ fSI (\i -> 11.0 - i * 0.4 / 0.6) senInfo)
      <> (senEcho os "SenH" $ fSEI (\i -> 11.0 - i * 0.4 / 0.6) senEchoInfo)
      <> (senSpread os "SenI" $ fSI (\i -> 11.0 + i * 0.6) senInfo)
      <> (senEcho os "SenJ" $ fSEI (\i -> 11.0 + i * 0.6) senEchoInfo)
  )

-------------------------------
----------------------
---------- Ti
----
type PlayerTiOpts
  = { tag :: String
    , pan :: Number -> Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

playerTi :: Int -> (Number -> PlayerTiOpts) -> Number -> List (AudioUnit D2)
playerTi name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMono_ (opts.tag <> "_panTi") (opts.pan time)
          ( gainT_' (opts.tag <> "_gainTi")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpfTi")
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

----------------------------
-------------------
----- Men
type PlayerMenOpts
  = { tag :: String
    , offset :: Number
    , pan :: Number -> AudioParameter Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

playerMen :: Int -> (Number -> PlayerMenOpts) -> Number -> List (AudioUnit D2)
playerMen name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMonoT_ (opts.tag <> "_panMen") (opts.pan time)
          ( gainT_' (opts.tag <> "_gainMen")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpfMen")
                  (opts.hpff time)
                  (opts.hpfq time)
                  (playBufWithOffset_ (opts.tag <> "_playerMen") name 1.0 opts.offset)
              )
          )
  else
    Nil
  where
  len = fromSoundsMen name'

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

---------------------------------------
-------------------------
----- Tal
type PlayerTalOpts
  = { tag :: String
    , offset :: Number
    , pan :: Number -> Number
    , gain :: Number -> AudioParameter Number
    , hpff :: Number -> AudioParameter Number
    , hpfq :: Number -> AudioParameter Number
    }

playerTal :: Int -> (Number -> PlayerTalOpts) -> Number -> List (AudioUnit D2)
playerTal name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMono_ (opts.tag <> "_panTal") (opts.pan time)
          ( gainT_' (opts.tag <> "_gainTal")
              (opts.gain time)
              ( highpassT_ (opts.tag <> "_hpfTal")
                  (opts.hpff time)
                  (opts.hpfq time)
                  (playBufWithOffset_ (opts.tag <> "_playerTal") name 1.0 opts.offset)
              )
          )
  else
    Nil
  where
  len = fromSoundsTal name'

  opts = opts' len

  name = "Tal-G5-" <> show name' <> "-l"

data TalInfo
  = TalInfo Int Number Number (Number -> AudioParameter Number)

playerTal_ :: Int -> (Number -> PlayerTalOpts) -> Number -> Behavior (AudioUnit D2)
playerTal_ name opts time = pure $ speaker (zero :| playerTal name opts time)

peak :: Number → Array (Tuple Number Number)
peak n = [ Tuple n 0.2, Tuple (n + 0.12) 1.0, Tuple (n + 0.23) 1.0, Tuple (n + 0.34) 0.2 ]

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

----------------------------------------
----------------------------------
------------------------
------------ Mood
type PlayerMoodOpts
  = { tag :: String
    , offset :: Number
    , pan :: Number -> Number
    , filt :: FiltSig
    , gain :: Number -> AudioParameter Number
    , bpff :: Number -> AudioParameter Number
    , bpfq :: Number -> AudioParameter Number
    }

playerMood :: String -> Int -> (Number -> PlayerMoodOpts) -> Number -> List (AudioUnit D2)
playerMood pitch name' opts' time =
  if time + kr >= 0.0 && time < len then
    pure
      $ pannerMono_ (opts.tag <> "_panMood") (opts.pan time)
          ( gainT_' (opts.tag <> "_gainMood")
              (opts.gain time)
              ( opts.filt (opts.tag <> "_bpfMood")
                  (opts.bpff time)
                  (opts.bpfq time)
                  (playBufWithOffset_ (opts.tag <> "_playerMood") name 1.0 opts.offset)
              )
          )
  else
    Nil
  where
  len = fromSoundsMood pitch name'

  opts = opts' len

  name = "Mood-" <> pitch <> "-" <> show name' <> "-l"

data MoodInfo
  = MoodInfo String String Int Number Number FiltSig Number (Number -> Number -> AudioParameter Number) MoodPan

data MoodPan
  = MoodPan Number Number Number

type FiltSig
  = forall ch. Pos ch => String -> AudioParameter Number -> AudioParameter Number -> AudioUnit ch -> AudioUnit ch

bypassFilt :: FiltSig
bypassFilt _ _ _ i = i

lowOs = 1.3 :: Number

moodPlayer2 :: Number -> String -> Array (Number -> List (AudioUnit D2))
moodPlayer2 os tg =
  map
    ( \(MoodInfo pitch tg x y f filt q gf (MoodPan _a _b _c)) ->
        ( atT (y + os)
            $ playerMood pitch x
                ( \l ->
                    { tag: tg <> "mp1" <> pitch <> (show x) <> tg
                    , pan: (\t -> _a * sin (_b * (t + _c) * pi))
                    , filt
                    , offset: 0.0
                    , gain: gf l
                    , bpff: epwf [ Tuple 0.0 f, Tuple 0.0 f ]
                    , bpfq: epwf [ Tuple 0.0 q, Tuple l q ]
                    }
                )
        )
    )
    ( ( map
          ( \i ->
              MoodInfo "E3" (show i) 0 (lowOs + (1.2 * (toNumber i)))
                100.0 -- (conv440 (-29))
                bandpassT_
                4.0
                (\l -> epwf [ Tuple 0.0 1.0, Tuple l 1.0 ])
                (MoodPan 0.0 0.2 0.2)
          )
          (range 0 8)
      )
        <> [ MoodInfo "E4" "-" 0 0.8
              300.0 -- (conv440 (-17))
              bypassFilt
              4.0
              (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
              (MoodPan 0.3 0.2 0.2)
          , MoodInfo "F#4" "-" 0 1.45
              400.0 -- (conv440 (-15))
              bypassFilt
              4.0
              (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
              (MoodPan 0.4 0.1 0.35)
          , MoodInfo "G4" "-" 4 1.15
              450.0 --(conv440 (22))
              bypassFilt
              4.0
              (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
              (MoodPan 0.1 0.5 (-0.7))
          , MoodInfo "A4" "-" 5 1.3
              500.0 --(conv440 (-12))
              bypassFilt
              4.0
              (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
              (MoodPan 0.05 1.0 0.05)
          , MoodInfo "B4" "-" 2 1.0
              550.0 --(conv440 (14))
              bypassFilt
              4.0
              (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
              (MoodPan 0.15 0.4 0.0)
          , MoodInfo "C5" "-" 2 1.6
              600.0 --(conv440 (3))
              bypassFilt
              4.0
              (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
              (MoodPan 0.6 0.6 (-0.8))
          , MoodInfo "D5" "-" 2 0.4
              650.0 -- (conv440 (-7))
              bypassFilt
              4.0
              (\l -> epwf [ Tuple 0.0 0.4, Tuple l 0.4 ])
              (MoodPan 0.5 0.75 0.8)
          , MoodInfo "E5" "-" 0 0.3
              700.0 --(conv440 (7))
              bypassFilt
              3.0
              (\l -> epwf [ Tuple 0.0 0.2, Tuple l 0.2 ])
              (MoodPan 0.9 0.15 (-0.2))
          , MoodInfo "F#5" "-" 0 0.3
              800.0 --(conv440 (9))
              bypassFilt
              2.0
              (\l -> epwf [ Tuple 0.0 0.2, Tuple l 0.2 ])
              (MoodPan 0.3 0.5 0.9)
          , MoodInfo "G5" "-" 3 0.5
              900.0 --(conv440 (10))
              bypassFilt
              2.0
              (\l -> epwf [ Tuple 0.0 0.3, Tuple l 0.3 ])
              (MoodPan 0.4 0.1 (-0.2))
          , MoodInfo "A5" "-" 2 0.0
              1000.0 --(conv440 0)
              bypassFilt
              0.01
              (\l -> epwf [ Tuple 0.0 1.0, Tuple 7.0 1.0, Tuple l 0.0 ])
              (MoodPan 0.3 0.1 0.5)
          ]
    )

conv440 :: Int -> Number
conv440 i = 440.0 * (2.0 `pow` ((toNumber $ 0 + i) / 12.0))

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( zero
            :| fold
                ( map ((#) time)
                    ( [ ( atT 0.0
                            $ playerIn 78
                                ( \l ->
                                    { tag: "drone"
                                    , pan: epwf [ Tuple 0.0 (-0.8), Tuple l (-0.4) ]
                                    , gain: epwf [ Tuple 0.0 0.0, Tuple 2.0 0.3, Tuple (l - 1.0) 1.0, Tuple (l - 0.6) 0.0, Tuple l 0.0 ]
                                    , hpff: epwf [ Tuple 0.0 800.0, Tuple l 400.0 ]
                                    , hpfq: epwf [ Tuple 0.0 10.0, Tuple l 1.0 ]
                                    }
                                )
                        )
                      ]
                        <> (fadeIn 0.0 "In")
                        <> (aDots 4.0 "A")
                        <> (senArr 7.0)
                        <> (tiDots 11.0 "Ti1")
                        <> (tiDots 20.0 "Ti2")
                        <> (menPlayer1 16.0 "Men1")
                        <> (menPlayer2 16.0 "Men2")
                        <> (talPlayer2 20.0 "Tal2")
                        <> (moodPlayer2 26.0 "Mood2")
                    )
                )
        )
