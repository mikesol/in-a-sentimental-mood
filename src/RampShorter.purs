module Klank.IASM.RampShorter where

import Prelude
import Control.Promise (toAffE)
import Data.Array (filter, foldl, mapWithIndex, take)
import Data.Array (fold, head, last, range, span)
import Data.Int (toNumber)
import Data.Lazy (Lazy, force)
import Data.Lazy (defer)
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
import Debug.Trace (spy)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter, AudioUnit, EngineInfo, allpassT_, bandpassT_, decodeAudioDataFromUri, defaultParam, dynamicsCompressor_, gain', gainT', gainT_, gainT_', gain_', highpassT_, highpass_, pannerMonoT_, pannerMono_, pannerT_, panner_, playBuf, playBufT_, playBufWithOffset_, playBuf_, runInBrowser, sinOsc, sinOsc_, speaker, speaker')
import Foreign.Object as O
import Math (cos, pi, pow, sin, abs)
import Test.QuickCheck.Gen (sample)
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

soundsRamp =
  [ (Tuple "A-A4-0" 2.511156462585034)
  --, (Tuple "A-A4-1" 2.6399092970521543)
  --, (Tuple "A-A4-2" 2.317981859410431)
  --, (Tuple "A-E4-0" 1.4487301587301586)
  -- , (Tuple "A-E4-1" 1.3521541950113378)
  -- , (Tuple "A-E4-2" 1.2112925170068027)
  , (Tuple "A-Fis4-0" 1.1590022675736962)
  -- , (Tuple "A-Fis4-1" 1.16702947845805)
  -- , (Tuple "A-Fis4-2" 1.2475283446712018)
  , (Tuple "A-G4-0" 1.3642403628117914)
  -- , (Tuple "A-G4-1" 1.0302267573696144)
  , (Tuple "In-D4-0" 2.559433106575964)
  , (Tuple "In-E4-0" 2.61578231292517)
  -- , (Tuple "In-E4-1" 2.6238321995464853)
  -- , (Tuple "In-E4-2" 2.7244444444444444)
  -- , (Tuple "In-Fis-3" 2.764671201814059)
  , (Tuple "In-Fis4-0" 3.050408163265306)
  -- , (Tuple "In-Fis4-1" 2.2777324263038548)
  -- , (Tuple "In-Fis4-2" 2.7606575963718822)
  , (Tuple "In-G4-0" 0.9577777777777777)
  -- , (Tuple "In-G4-1" 0.8531519274376417)
  , (Tuple "Men-A4-0" 1.5292290249433107)
  -- , (Tuple "Men-A4-1" 1.6741043083900227)
  -- , (Tuple "Men-A4-2" 1.5654421768707483)
  , (Tuple "Men-B4-0" 1.5292290249433107)
  , (Tuple "Men-C5-0" 1.3280045351473924)
  -- , (Tuple "Men-C5-1" 1.3239909297052155)
  -- , (Tuple "Men-C5-2" 1.3038775510204081)
  -- , (Tuple "Men-C5-3" 1.271655328798186)
  -- , (Tuple "Men-C5-4" 1.2555555555555555)
  , (Tuple "Men-D4-0" 1.464829931972789)
  -- , (Tuple "Men-D4-1" 1.6499546485260772)
  , (Tuple "Men-D5-0" 1.1911791383219954)
  -- , (Tuple "Men-D5-1" 1.2805668934240362)
  -- , (Tuple "Men-D5-2" 1.3924036281179137)
  -- , (Tuple "Men-D5-3" 1.3159410430839003)
  -- , (Tuple "Men-D5-4" 1.412517006802721)
  , (Tuple "Men-E4-0" 1.4769160997732427)
  -- , (Tuple "Men-Fis4-0" 1.3320408163265307)
  -- , (Tuple "Men-Fis4-1" 1.468843537414966)
  -- , (Tuple "Men-Fis4-2" 1.36421768707483)
  , (Tuple "Men-G4-0" 1.2193650793650794)
  -- , (Tuple "Men-G4-1" 1.3199773242630386)
  , (Tuple "Sen-A4-0" 1.7626303854875283)
  -- , (Tuple "Sen-A4-1" 1.7384807256235828)
  -- , (Tuple "Sen-A4-2" 1.6901814058956917)
  -- , (Tuple "Sen-A4-3" 1.5131292517006802)
  , (Tuple "Sen-B4-0" 1.782766439909297)
  -- , (Tuple "Sen-B4-1" 1.6499546485260772)
  -- , (Tuple "Sen-B4-2" 2.2898185941043083)
  , (Tuple "Sen-E4-0" 1.4044671201814058)
  -- , (Tuple "Sen-E4-1" 1.5694784580498866)
  -- , (Tuple "Sen-E4-2" 1.6741043083900227)
  , (Tuple "Sen-Fis4-0" 1.609705215419501)
  -- , (Tuple "Sen-Fis4-1" 1.412517006802721)
  , (Tuple "Sen-G4-0" 1.4769160997732427)
  -- , (Tuple "Sen-G4-1" 1.360204081632653)
  -- , (Tuple "Sen-G4-2" 1.505079365079365)
  -- , (Tuple "Sen-G4-3" 1.5251927437641724)
  , (Tuple "Tal-A4-0" 1.0543764172335601)
  -- , (Tuple "Tal-A4-1" 1.1469160997732426)
  , (Tuple "Tal-B4-0" 1.1348526077097505)
  -- , (Tuple "Tal-B4-1" 1.255578231292517)
  -- , (Tuple "Tal-B4-2" 1.2273922902494332)
  , (Tuple "Tal-C5-0" 1.255578231292517)
  -- , (Tuple "Tal-C5-1" 1.3843537414965987)
  -- , (Tuple "Tal-C5-2" 1.215328798185941)
  , (Tuple "Tal-D4-0" 1.4085034013605442)
  -- , (Tuple "Tal-D4-1" 1.271655328798186)
  , (Tuple "Tal-D5-0" 1.3561904761904762)
  -- , (Tuple "Tal-D5-1" 0.9859410430839002)
  -- , (Tuple "Tal-D5-2" 0.9859410430839002)
  -- , (Tuple "Tal-D5-3" 1.1791156462585033)
  , (Tuple "Tal-E4-0" 1.3400907029478457)
  , (Tuple "Tal-E5-0" 1.311904761904762)
  -- , (Tuple "Tal-E5-1" 1.16702947845805)
  -- , (Tuple "Tal-E5-2" 1.1227891156462586)
  -- , (Tuple "Tal-E5-3" 1.295827664399093)
  -- , (Tuple "Tal-E5-4" 1.2515646258503401)
  -- , (Tuple "Tal-E5-5" 1.2877551020408162)
  -- , (Tuple "Tal-E5-6" 1.2354421768707482)
  , (Tuple "Tal-Fis4-0" 1.1227891156462586)
  , (Tuple "Tal-Fis5-0" 1.2756916099773243)
  -- , (Tuple "Tal-Fis5-1" 1.3239909297052155)
  -- , (Tuple "Tal-Fis5-2" 0.965827664399093)
  -- , (Tuple "Tal-Fis5-3" 1.0463038548752834)
  , (Tuple "Tal-G4-0" 1.2273922902494332)
  -- , (Tuple "Tal-G4-1" 1.1831292517006802)
  , (Tuple "Tal-G5-0" 1.2112925170068027)
  -- , (Tuple "Tal-G5-1" 1.2434920634920634)
  -- , (Tuple "Tal-G5-2" 1.2917913832199546)
  -- , (Tuple "Tal-G5-3" 1.3722902494331066)
  , (Tuple "Ti-A4-0" 1.4085034013605442)
  -- , (Tuple "Ti-A4-1" 1.3682539682539683)
  -- , (Tuple "Ti-A4-2" 1.1469160997732426)
  -- , (Tuple "Ti-A4-3" 1.1590022675736962)
  -- , (Tuple "Ti-A4-4" 1.1670521541950114)
  , (Tuple "Ti-B4-0" 1.2725396825396826)
  -- , (Tuple "Ti-B4-1" 1.3400907029478457)
  -- , (Tuple "Ti-B4-2" 1.3521541950113378)
  -- , (Tuple "Ti-B4-3" 1.3119274376417234)
  -- , (Tuple "Ti-B4-4" 1.2877777777777777)
  -- , (Tuple "Ti-B4-5" 1.1590022675736962)
  , (Tuple "Ti-C5-0" 1.1951927437641723)
  -- , (Tuple "Ti-C5-1" 1.4165532879818594)
  -- , (Tuple "Ti-C5-2" 1.4406802721088436)
  -- , (Tuple "Ti-C5-3" 1.2636281179138322)
  , (Tuple "Ti-Cis5-0" 1.203265306122449)
  -- , (Tuple "Ti-Cis5-1" 1.2917913832199546)
  -- , (Tuple "Ti-Cis5-2" 1.36421768707483)
  -- , (Tuple "Ti-Cis5-3" 1.8471428571428572)
  -- , (Tuple "Ti-Cis5-4" 1.2877551020408162)
  , (Tuple "Ti-D5-0" 1.323968253968254)
  -- , (Tuple "Ti-D5-1" 1.4326530612244899)
  -- , (Tuple "Ti-D5-2" 1.215328798185941)
  , (Tuple "Ti-Fis4-0" 1.3038548752834467)
  -- , (Tuple "Ti-Fis4-1" 1.0342403628117913)
  , (Tuple "Ti-G4-0" 1.2837414965986396)
  -- , (Tuple "Ti-G4-1" 1.376281179138322)
  -- , (Tuple "Ti-G4-2" 1.3280045351473924)
  -- , (Tuple "Ti-G4-3" 1.2354648526077097)
  , Tuple "Men-E5-0" 1.8350566893424036
  --, Tuple "Men-E5-1" 1.255578231292517
  --, Tuple "Men-E5-2" 1.835079365079365
  ] ::
    Array (Tuple String Number)

fromSoundsRamp :: String -> Number
fromSoundsRamp i = fromMaybe 0.0 (M.lookup i soundsRampMap)

soundsRampMap :: M.Map String Number
soundsRampMap = M.fromFoldable soundsRamp

soundsBowl =
  [ Tuple "Beans---Bounce-Around" 7.133401360544218
  -- , Tuple "Beans---Pour-Into-Bowl" 7.428344671201814
  -- , Tuple "Beans---Roll-ARound" 14.05859410430839
  -- , Tuple "Beans---Shakey-Shakey" 2.507097505668934
  -- , Tuple "Beans---Short-Drops" 9.760068027210885
  -- , Tuple "Beans---Tiny-Pour" 5.924739229024944
  -- , Tuple "Beans---Tiny-Shakes" 9.687278911564626
  -- , Tuple "Large---Beans-Perform-1" 39.63201814058957
  -- , Tuple "Large---Beans-Perform-2" 44.304263038548754
  -- , Tuple "Large---Perform-1" 58.31650793650794
  -- , Tuple "Large---Perform-2" 59.05689342403628
  -- , Tuple "Large---Strike-1" 27.556757369614512
  -- , Tuple "Large---Strike-2" 34.679977324263035
  -- , Tuple "Large---Strike-3" 49.44018140589569
  -- , Tuple "Medium---Beans-Perform-1" 27.180068027210883
  -- , Tuple "Medium---Beans-Perform-2" 27.98140589569161
  -- , Tuple "Medium---Perform-1" 31.73292517006803
  -- , Tuple "Medium---Perform-2" 29.766802721088435
  -- , Tuple "Medium---Perform-3" 26.295396825396825
  -- , Tuple "Medium---Strike-1" 16.576825396825395
  -- , Tuple "Medium---Strike-2" 18.853968253968254
  -- , Tuple "Small---Beans-Perform-1" 20.253265306122447
  -- , Tuple "Small---Beans-Perform-2" 17.817868480725625
  -- , Tuple "Small---Perform-1" 23.331859410430837
  -- , Tuple "Small---Perform-2" 26.8878231292517
  -- , Tuple "Small---Perform-3" 24.13902494331066
  -- , Tuple "Small---Strike-1" 12.355079365079366
  , Tuple "Small---Strike-2" 11.348979591836734
  ] ::
    Array (Tuple String Number)

fromSoundsBowl :: String -> Number
fromSoundsBowl i = fromMaybe 0.0 (M.lookup i soundsBowlMap)

soundsBowlMap :: M.Map String Number
soundsBowlMap = M.fromFoldable soundsBowl

soundsChime =
  [ Tuple "Glass---Jangle-1" 2.301111111111111
  -- , Tuple "Glass---Jangle-10" 2.729433106575964
  -- , Tuple "Glass---Jangle-11" 3.7241950113378683
  -- , Tuple "Glass---Jangle-12" 5.396281179138322
  -- , Tuple "Glass---Jangle-13" 4.785215419501133
  -- , Tuple "Glass---Jangle-14" 5.109750566893424
  -- , Tuple "Glass---Jangle-15" 1.062267573696145
  -- , Tuple "Glass---Jangle-16" 2.812108843537415
  -- , Tuple "Glass---Jangle-2" 0.3187981859410431
  -- , Tuple "Glass---Jangle-3" 2.3122902494331066
  -- , Tuple "Glass---Jangle-4" 4.244852607709751
  -- , Tuple "Glass---Jangle-5" 0.3339455782312925
  -- , Tuple "Glass---Jangle-6" 4.198027210884354
  -- , Tuple "Glass---Jangle-7" 6.5266213151927435
  -- , Tuple "Glass---Jangle-8" 5.6781632653061225
  -- , Tuple "Glass---Jangle-9" 2.72281179138322
  -- , Tuple "Large-Metal---Jangle-1" 30.22986394557823
  -- , Tuple "Large-Metal---Jangle-2" 35.031451247165535
  -- , Tuple "Large-Metal---Jangle-3" 34.05777777777778
  -- , Tuple "Large-Metal---Jangle-4" 44.86671201814059
  -- , Tuple "Large-Metal---Strike-1" 8.119614512471655
  -- , Tuple "Large-Metal---Strike-10" 16.756145124716554
  -- , Tuple "Large-Metal---Strike-11" 12.678866213151927
  -- , Tuple "Large-Metal---Strike-12" 12.795895691609978
  -- , Tuple "Large-Metal---Strike-2" 9.075555555555555
  -- , Tuple "Large-Metal---Strike-3" 24.450204081632652
  -- , Tuple "Large-Metal---Strike-4" 25.26907029478458
  -- , Tuple "Large-Metal---Strike-5" 18.504943310657595
  -- , Tuple "Large-Metal---Strike-6" 19.81249433106576
  -- , Tuple "Large-Metal---Strike-7" 16.04185941043084
  -- , Tuple "Large-Metal---Strike-8" 22.769909297052155
  -- , Tuple "Large-Metal---Strike-9" 16.91108843537415
  -- , Tuple "Medium-Metal---Jangle-1" 21.74204081632653
  -- , Tuple "Medium-Metal---Jangle-2" 25.345691609977326
  -- , Tuple "Medium-Metal---Jangle-3" 25.92814058956916
  -- , Tuple "Medium-Metal---Jangle-4" 31.369206349206348
  -- , Tuple "Medium-Metal---Jangle-5" 36.10185941043084
  -- , Tuple "Medium-Metal---Jangle-6" 43.107732426303855
  -- , Tuple "Medium-Metal---Jangle-7" 32.42267573696145
  -- , Tuple "Medium-Metal---Strike-1" 19.002244897959184
  -- , Tuple "Medium-Metal---Strike-10" 13.32140589569161
  -- , Tuple "Medium-Metal---Strike-11" 11.060884353741496
  -- , Tuple "Medium-Metal---Strike-2" 19.08628117913832
  -- , Tuple "Medium-Metal---Strike-3" 27.01922902494331
  -- , Tuple "Medium-Metal---Strike-4" 26.927437641723355
  -- , Tuple "Medium-Metal---Strike-5" 14.383424036281179
  -- , Tuple "Medium-Metal---Strike-6" 8.051791383219955
  -- , Tuple "Medium-Metal---Strike-7" 12.11687074829932
  -- , Tuple "Medium-Metal---Strike-8" 15.123265306122448
  -- , Tuple "Medium-Metal---Strike-9" 13.202743764172336
  -- , Tuple "Performance---Glass" 73.4691156462585
  -- , Tuple "Performance---Large-Metal" 263.9421541950113
  -- , Tuple "Performance---Medium-Metal" 225.3205442176871
  -- , Tuple "Performance---Smol-Metal" 54.277687074829934
  -- , Tuple "Performance---Tiny-Metal" 66.87850340136055
  -- , Tuple "Smol-Metal---Jangle-1" 4.434149659863945
  -- , Tuple "Smol-Metal---Jangle-10" 5.852154195011338
  -- , Tuple "Smol-Metal---Jangle-11" 5.546961451247165
  -- , Tuple "Smol-Metal---Jangle-2" 7.981315192743764
  -- , Tuple "Smol-Metal---Jangle-3" 8.817732426303856
  -- , Tuple "Smol-Metal---Jangle-4" 8.871655328798186
  -- , Tuple "Smol-Metal---Jangle-5" 5.361428571428571
  -- , Tuple "Smol-Metal---Jangle-6" 12.680680272108843
  -- , Tuple "Smol-Metal---Jangle-7" 9.771473922902494
  -- , Tuple "Smol-Metal---Jangle-8" 5.065260770975057
  -- , Tuple "Smol-Metal---Jangle-9" 5.101473922902494
  -- , Tuple "Tiny-Metal---Jangle-1" 6.633696145124716
  -- , Tuple "Tiny-Metal---Jangle-2" 6.426961451247165
  -- , Tuple "Tiny-Metal---Jangle-3" 7.705260770975057
  -- , Tuple "Tiny-Metal---Jangle-4" 7.356031746031746
  -- , Tuple "Tiny-Metal---Jangle-5" 15.176462585034013
  -- , Tuple "Tiny-Metal---Jangle-6" 21.149274376417235
  , Tuple "Tiny-Metal---Jangle-7" 14.375873015873015
  , Tuple "Tiny-Metal---Jangle-8" 19.43750566893424
  ] ::
    Array (Tuple String Number)

fromSoundsChime :: String -> Number
fromSoundsChime i = fromMaybe 0.0 (M.lookup i soundsChimeMap)

soundsChimeMap :: M.Map String Number
soundsChimeMap = M.fromFoldable soundsChime

soundsTongue =
  [ Tuple "Drum-Strike-1" 2.068390022675737
  , Tuple "Drum-Strike-2" 2.110770975056689
  -- , Tuple "Drum-Strike-3" 1.9203628117913831
  -- , Tuple "Drum-Strike-4" 3.5199319727891156
  -- , Tuple "Drum-Strike-5" 1.952108843537415
  -- , Tuple "Drum-Strike-6" 1.6828571428571428
  -- , Tuple "Drum-Strike-7" 2.1110430839002268
  -- , Tuple "Drum-Strike-8" 4.018163265306122
  -- , Tuple "Maj-3rd" 11.66575963718821
  -- , Tuple "Mallet-A2_1" 6.090929705215419
  -- , Tuple "Mallet-A2_2" 8.503038548752835
  -- , Tuple "Mallet-A3_1" 8.948707482993198
  -- , Tuple "Mallet-A3_2" 10.401972789115646
  -- , Tuple "Mallet-B2_1" 5.32702947845805
  -- , Tuple "Mallet-B2_2" 8.435170068027212
  -- , Tuple "Mallet-B3_1" 8.41326530612245
  -- , Tuple "Mallet-B3_2" 9.287891156462585
  -- , Tuple "Mallet-D2_1" 7.039637188208617
  -- , Tuple "Mallet-D2_2" 9.193287981859411
  -- , Tuple "Mallet-D3_1" 8.432698412698413
  -- , Tuple "Mallet-D3_2" 8.818798185941043
  -- , Tuple "Mallet-D3_3" 9.813718820861679
  -- , Tuple "Mallet-Drag" 38.805192743764174
  -- , Tuple "Mallet-E1_1" 5.389523809523809
  -- , Tuple "Mallet-E1_2" 8.492766439909298
  -- , Tuple "Mallet-E2_1" 10.487573696145125
  -- , Tuple "Mallet-E2_2" 12.385555555555555
  -- , Tuple "Mallet-G1_1" 7.12734693877551
  -- , Tuple "Mallet-G1_2" 9.903786848072562
  -- , Tuple "Mallet-G1_3" 9.623219954648526
  -- , Tuple "Mallet-G2_1" 9.6
  -- , Tuple "Mallet-G2_2" 11.074285714285715
  -- , Tuple "Melodic-1" 19.294489795918366
  -- , Tuple "Melodic-2" 14.200476190476191
  -- , Tuple "Melodic-3" 21.872857142857143
  -- , Tuple "Melodic-4" 16.379478458049885
  -- , Tuple "Melodic-5" 18.92231292517007
  -- , Tuple "Melodic-6" 14.183197278911564
  -- , Tuple "Mute_A3_1" 2.227074829931973
  -- , Tuple "Mute_A3_2" 2.899863945578231
  -- , Tuple "Mute_B2_1" 1.970907029478458
  -- , Tuple "Mute_B2_2" 2.7991836734693876
  -- , Tuple "Mute_B3_1" 2.3194104308390022
  -- , Tuple "Mute_B3_2" 3.7057142857142855
  -- , Tuple "Mute_D2_1" 2.0817687074829934
  -- , Tuple "Mute_D2_2" 3.6892743764172335
  -- , Tuple "Mute_D3_1" 2.399546485260771
  -- , Tuple "Mute_D3_2" 2.628390022675737
  -- , Tuple "Mute_E1_1" 1.7348072562358277
  -- , Tuple "Mute_E1_2" 3.0092743764172334
  -- , Tuple "Mute_E2_1" 2.3523809523809525
  -- , Tuple "Mute_E2_2" 2.276281179138322
  -- , Tuple "Mute_E2_3" 4.457800453514739
  -- , Tuple "Mute_G1_1" 2.4279818594104308
  -- , Tuple "Mute_G1_2" 2.4920408163265306
  -- , Tuple "Mute_G2_1" 2.465963718820862
  -- , Tuple "Mute_G2_2" 1.9372562358276644
  -- , Tuple "Mute_G2_3" 3.0500453514739228
  -- , Tuple "RAW_Tongue-Drum-Recording" 1136.62693877551
  -- , Tuple "Roll-B2_1" 7.905963718820861
  -- , Tuple "Roll-B2_2" 6.545918367346939
  -- , Tuple "Roll_A2_1" 6.338571428571429
  -- , Tuple "Roll_A2_2" 7.154308390022676
  -- , Tuple "Roll_A3_1" 6.737482993197279
  -- , Tuple "Roll_A3_2" 5.782857142857143
  -- , Tuple "Roll_B1_1" 6.364331065759637
  -- , Tuple "Roll_B1_2" 5.6302267573696145
  -- , Tuple "Roll_D2_1" 6.191972789115646
  -- , Tuple "Roll_D2_2" 8.030589569160998
  -- , Tuple "Roll_D3_1" 4.971111111111111
  -- , Tuple "Roll_D3_2" 8.206689342403628
  -- , Tuple "Roll_E1_1" 8.627528344671202
  -- , Tuple "Roll_E2_1" 6.419115646258503
  -- , Tuple "Roll_E2_2" 10.962879818594104
  -- , Tuple "Roll_G1_1" 8.20451247165533
  -- , Tuple "Roll_G1_2" 8.304126984126984
  -- , Tuple "Roll_G2_1" 6.336689342403628
  -- , Tuple "Roll_G2_2" 5.621519274376417
  -- , Tuple "Stick-Drag-1" 13.991315192743764
  -- , Tuple "Stick-Drag-2" 2.5011337868480727
  -- , Tuple "Stick-Drag-3" 14.810725623582766
  -- , Tuple "Stick-Strike-1" 0.5592290249433106
  -- , Tuple "Stick-Strike-10" 2.26172335600907
  -- , Tuple "Stick-Strike-11" 2.3292290249433107
  -- , Tuple "Stick-Strike-12" 1.9672789115646259
  -- , Tuple "Stick-Strike-13" 4.693560090702948
  -- , Tuple "Stick-Strike-14" 4.021043083900227
  -- , Tuple "Stick-Strike-15" 3.397437641723356
  -- , Tuple "Stick-Strike-16" 5.338140589569161
  -- , Tuple "Stick-Strike-17" 5.386916099773242
  -- , Tuple "Stick-Strike-18" 5.8336507936507935
  -- , Tuple "Stick-Strike-19" 6.560294784580499
  -- , Tuple "Stick-Strike-2" 1.0129931972789115
  -- , Tuple "Stick-Strike-20" 4.647709750566894
  -- , Tuple "Stick-Strike-21" 4.977687074829932
  -- , Tuple "Stick-Strike-3" 1.263877551020408
  -- , Tuple "Stick-Strike-4" 1.0765306122448979
  -- , Tuple "Stick-Strike-5" 1.90437641723356
  -- , Tuple "Stick-Strike-6" 1.7545124716553289
  -- , Tuple "Stick-Strike-7" 1.7829931972789115
  , Tuple "Stick-Strike-8" 2.0106575963718822
  -- , Tuple "Stick-Strike-9" 2.3782766439909295
  , Tuple "Strike-Multiple" 11.801746031746031
  ] ::
    Array (Tuple String Number)

fromSoundsTongue :: String -> Number
fromSoundsTongue i = fromMaybe 0.0 (M.lookup i soundsTongueMap)

soundsTongueMap :: M.Map String Number
soundsTongueMap = M.fromFoldable soundsTongue

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
  , MoodIdx (Tuple "B5" 0) 2.3858503401360545
  , MoodIdx (Tuple "B5" 1) 3.465578231292517
  , MoodIdx (Tuple "B5" 2) 3.4771882086167802
  , MoodIdx (Tuple "B5" 3) 3.6048979591836736
  ] ::
    Array MoodIdx

fromSoundsMood :: String -> Int -> Number
fromSoundsMood s i = fromMaybe 0.0 (M.lookup (s <> "-" <> show i) soundsMoodMap)

soundsMoodMap :: M.Map String Number
soundsMoodMap = M.fromFoldable (map (\(MoodIdx (Tuple x y) b) -> Tuple (x <> "-" <> show y) b) soundsMood)

kr = (toNumber iasmEngineInfo.msBetweenSamples) / 1000.0 :: Number

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
                        s = fst i
                      in
                        Tuple
                          ("Ramp-" <> s <> "-l")
                          ("Ramp/" <> s <> ".l.ogg")
                  )
                  soundsRamp
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
                <> ( map
                      ( \i ->
                          let
                            s = fst i
                          in
                            Tuple
                              ("SingingBowls-" <> s <> "-l")
                              ("SingingBowls/" <> s <> ".l.ogg")
                      )
                      soundsBowl
                  )
                <> ( map
                      ( \i ->
                          let
                            s = fst i
                          in
                            Tuple
                              ("Windchime-" <> s <> "-l")
                              ("Windchime/" <> s <> ".l.ogg")
                      )
                      soundsChime
                  )
                <> ( map
                      ( \i ->
                          let
                            s = fst i
                          in
                            Tuple
                              ("TongueDrum-" <> s <> "-l")
                              ("TongueDrum/" <> s <> ".l.ogg")
                      )
                      soundsTongue
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

boundPlayer :: Number -> Number -> Lazy (List (AudioUnit D2)) -> List (AudioUnit D2)
boundPlayer len time a = if (time) + kr >= 0.0 && time < (len) then force a else Nil

playerIctus :: String -> String -> Number -> Number -> Number -> Number -> Number -> List (AudioUnit D2)
playerIctus tag name len vos ros tos time =
  boundPlayer (len + 1.0) time
    ( defer \_ ->
        pure
          $ panner_ (tag <> "_panIctus") 0.0
              ( gainT_' (tag <> "_gainIctus")
                  ((epwf [ Tuple 0.0 0.0, Tuple 0.15 0.0, Tuple 1.0 vos, Tuple (len - 0.15) 0.0, Tuple len 0.0 ]) time)
                  ( highpassT_ (tag <> "_highpassIctus") ((epwf [ Tuple 0.0 200.0, Tuple len 200.0 ]) time)
                      ((epwf [ Tuple 0.0 1.0, Tuple len 1.0 ]) time)
                      (playBufWithOffset_ (tag <> "_playerIctus") (name) ros tos)
                  )
              )
    )

data InASentimentalMood
  = In
  | A
  | Sen
  | Ti
  | Men
  | Tal
  | Mood

data Pitch
  = C4
  | Cis4
  | D4
  | Dis4
  | E4
  | F4
  | Fis4
  | G4
  | Gis4
  | A4
  | B4
  | C5
  | Cis5
  | D5
  | Dis5
  | E5
  | F5
  | Fis5
  | G5
  | Gis5
  | A5
  | B5
  | C6
  | Cis6
  | D6

p2s_0 :: Pitch -> String
p2s_0 C4 = "C4"

p2s_0 Cis4 = "Cis4"

p2s_0 D4 = "D4"

p2s_0 Dis4 = "Dis4"

p2s_0 E4 = "E4"

p2s_0 F4 = "F4"

p2s_0 Fis4 = "Fis4"

p2s_0 G4 = "G4"

p2s_0 Gis4 = "Gis4"

p2s_0 A4 = "A4"

p2s_0 B4 = "B4"

p2s_0 C5 = "C5"

p2s_0 Cis5 = "Cis5"

p2s_0 D5 = "D5"

p2s_0 Dis5 = "Dis5"

p2s_0 E5 = "E5"

p2s_0 F5 = "F5"

p2s_0 Fis5 = "Fis5"

p2s_0 G5 = "G5"

p2s_0 Gis5 = "Gis5"

p2s_0 A5 = "A5"

p2s_0 B5 = "B5"

p2s_0 C6 = "C6"

p2s_0 Cis6 = "Cis6"

p2s_0 D6 = "D6"

p2s_1 :: Pitch -> String
p2s_1 C4 = "C4"

p2s_1 Cis4 = "C#4"

p2s_1 D4 = "D4"

p2s_1 Dis4 = "D#4"

p2s_1 E4 = "E4"

p2s_1 F4 = "F4"

p2s_1 Fis4 = "F#4"

p2s_1 G4 = "G4"

p2s_1 Gis4 = "G#4"

p2s_1 A4 = "A4"

p2s_1 B4 = "B4"

p2s_1 C5 = "C5"

p2s_1 Cis5 = "C#5"

p2s_1 D5 = "D5"

p2s_1 Dis5 = "D#5"

p2s_1 E5 = "E5"

p2s_1 F5 = "F5"

p2s_1 Fis5 = "F#5"

p2s_1 G5 = "G5"

p2s_1 Gis5 = "G#5"

p2s_1 A5 = "A5"

p2s_1 B5 = "B5"

p2s_1 C6 = "C6"

p2s_1 Cis6 = "C#6"

p2s_1 D6 = "D6"

data CascadeEvent
  = InEntry
  | AEntry
  | SenEntry
  | TiEntry
  | MenEntry
  | TalEntry
  | MoodEntry
  | NoEvent

data Cascade
  = Cascade InASentimentalMood Pitch Number CascadeEvent

data CSN
  = CSN Cascade String Number

cascades =
  [ Cascade In G4 0.1 NoEvent
  , Cascade In Fis4 0.1 NoEvent
  , Cascade In E4 0.1 NoEvent
  , Cascade In D4 0.1 NoEvent
  , Cascade A A4 0.08 NoEvent
  , Cascade A G4 0.08 NoEvent
  , Cascade A Fis4 0.08 NoEvent
  , Cascade A E4 0.08 NoEvent
  , Cascade Sen B4 0.1 NoEvent
  , Cascade Sen A4 0.1 NoEvent
  , Cascade Sen G4 0.1 NoEvent
  , Cascade Sen Fis4 0.1 NoEvent
  , Cascade Sen E4 0.1 NoEvent
  , Cascade Sen D4 0.1 NoEvent
  , Cascade Ti D5 0.08 NoEvent
  , Cascade Ti C5 0.08 NoEvent
  , Cascade Ti B4 0.08 NoEvent
  , Cascade Ti A4 0.08 NoEvent
  , Cascade Ti G4 0.08 NoEvent
  , Cascade Ti Fis4 0.08 NoEvent
  , Cascade Men E5 0.1 NoEvent
  , Cascade Men D5 0.1 NoEvent
  , Cascade Men C5 0.1 NoEvent
  , Cascade Men B4 0.1 NoEvent
  , Cascade Men A4 0.1 NoEvent
  , Cascade Men G4 0.1 NoEvent
  , Cascade Tal G5 0.08 NoEvent
  , Cascade Tal Fis5 0.08 NoEvent
  , Cascade Tal E5 0.08 NoEvent
  , Cascade Tal D5 0.08 NoEvent
  , Cascade Tal C5 0.08 NoEvent
  , Cascade Tal B4 0.08 NoEvent
  , Cascade Mood B5 0.1 NoEvent
  , Cascade Mood A5 0.1 NoEvent
  , Cascade Mood G5 0.1 NoEvent
  , Cascade Mood Fis5 0.1 NoEvent
  , Cascade Mood E5 0.1 NoEvent
  , Cascade Mood D5 0.1 NoEvent
  , Cascade Mood C5 0.1 NoEvent
  , Cascade Mood B4 0.1 NoEvent
  , Cascade Mood B5 0.1 NoEvent
  , Cascade Mood A5 0.1 NoEvent
  , Cascade Mood G5 0.1 NoEvent
  , Cascade Mood Fis5 0.1 NoEvent
  , Cascade Mood E5 0.1 NoEvent
  , Cascade Mood D5 0.1 NoEvent
  , Cascade Mood C5 0.1 NoEvent
  , Cascade Mood B4 0.1 NoEvent
  , Cascade Mood A4 0.1 NoEvent
  , Cascade Mood B5 0.1 NoEvent
  , Cascade Mood A5 0.1 NoEvent
  , Cascade Mood G5 0.1 NoEvent
  , Cascade Mood Fis5 0.1 NoEvent
  , Cascade Mood B5 0.1 NoEvent
  , Cascade Mood A5 0.1 NoEvent
  , Cascade Mood G5 0.1 NoEvent
  , Cascade Mood Fis5 0.1 NoEvent
  , Cascade Mood E5 0.1 NoEvent
  , Cascade Mood D5 0.1 NoEvent
  , Cascade Mood C5 0.1 NoEvent
  , Cascade Mood B4 0.1 NoEvent
  , Cascade Mood B5 0.1 NoEvent
  , Cascade Mood A5 0.1 NoEvent
  , Cascade Mood G5 0.1 NoEvent
  , Cascade Mood Fis5 0.1 NoEvent
  , Cascade Mood E5 0.1 NoEvent
  , Cascade Mood D5 0.1 NoEvent
  , Cascade Mood C5 0.1 NoEvent
  , Cascade Mood B4 0.1 NoEvent
  , Cascade Mood B5 0.35 NoEvent
  , Cascade Mood A5 0.38 NoEvent
  , Cascade Mood G5 0.41 NoEvent
  , Cascade Mood Fis5 0.44 NoEvent
  , Cascade Mood E5 0.47 NoEvent
  , Cascade Mood D5 0.50 NoEvent
  , Cascade Mood C5 0.53 NoEvent
  , Cascade Mood B4 0.56 NoEvent
  ] ::
    Array Cascade

c2s :: Cascade -> String
c2s c@(Cascade word _ _ _) =
  ( case word of
      Mood -> ""
      _ -> "Ramp-"
  )
    <> c2s_1 c
    <> "-"
    <> c2s_2 c
    <> "-0-l"

c2sShort :: Cascade -> String
c2sShort c@(Cascade word _ _ _) =
  c2s_1 c
    <> "-"
    <> c2s_2 c
    <> "-0"

c2s_1 :: Cascade -> String
c2s_1 (Cascade word _ _ _) = case word of
  In -> "In"
  A -> "A"
  Sen -> "Sen"
  Ti -> "Ti"
  Men -> "Men"
  Tal -> "Tal"
  Mood -> "Mood"

c2s_2 :: Cascade -> String
c2s_2 (Cascade word pitch _ _) = case word of
  Mood -> p2s_1 pitch
  _ -> p2s_0 pitch

cascadesWithInfo =
  map
    ( \c@(Cascade word pitch t _) ->
        let
          cname = c2s c

          nameShort = c2sShort c
        in
          CSN c (cname) case word of
            Mood -> (fromSoundsMood (c2s_2 c)) 0 -- hardcode zero as quick solution
            _ -> (fromSoundsRamp (nameShort))
    )
    cascades ::
    Array CSN

cascadesWithInfoInTime =
  ( foldl
      ( \{ acc, dur } (CSN (Cascade a b c marker) d e) ->
          { acc: acc <> [ (CSN (Cascade a b (c + dur) marker) d e) ], dur: dur + c }
      )
      { acc: [], dur: 0.0 }
      cascadesWithInfo
  ) ::
    { acc :: Array CSN, dur :: Number }

conv440 :: Int -> Number
conv440 i = 440.0 * (2.0 `pow` ((toNumber $ 0 + i) / 12.0))

playerG2 :: Number -> List (AudioUnit D2)
playerG2 time =
  boundPlayer (fromSoundsTongue "Mallet-G2_1") time
    ( defer \_ ->
        pure
          $ (playBufWithOffset_ ("tongue-g2") ("TongueDrum-Mallet-G1_1-l") 1.0 0.0)
    )

playerG1 :: Number -> List (AudioUnit D2)
playerG1 time =
  boundPlayer (fromSoundsTongue "Mallet-G1_1") time
    ( defer \_ ->
        pure
          $ (gain_' "gtg1" 0.2 (playBufWithOffset_ ("tongue-g1") ("TongueDrum-Mallet-G1_1-l") 1.0 0.0))
    )

playerA1 :: Number -> List (AudioUnit D2)
playerA1 time =
  boundPlayer (fromSoundsTongue "Roll_A2_1") time
    ( defer \_ ->
        pure
          $ (gain_' "gta1" 0.2 (playBufWithOffset_ ("tongue-a1") ("TongueDrum-Roll_A2_1-l") 1.0 0.0))
    )

playerB1 :: Number -> List (AudioUnit D2)
playerB1 time =
  boundPlayer (fromSoundsTongue "Roll_B1_1") time
    ( defer \_ ->
        pure
          $ (gain_' "gta2" 0.2 (playBufWithOffset_ ("tongue-b1") ("TongueDrum-Roll_B2_1-l") 1.0 0.0))
    )

playerD2 :: Number -> List (AudioUnit D2)
playerD2 time =
  boundPlayer (fromSoundsTongue "Roll_D2_2") time
    ( defer \_ ->
        pure
          $ (gain_' "gtd2" 0.2 (playBufWithOffset_ ("tongue-d2") ("TongueDrum-Roll_D2_2-l") 1.0 0.0))
    )

playerE2 :: Number -> List (AudioUnit D2)
playerE2 time =
  boundPlayer (fromSoundsChime "Medium-Metal---Strike-11") time
    ( defer \_ ->
        pure
          $ (gain_' "gte2" 0.2 (playBufWithOffset_ ("chime-e2") ("Windchime-Medium-Metal---Strike-11-l") 1.0 0.0))
    )

-- sound going up (may need to convert to pwf)
bufexpr0 :: Number -> Behavior (AudioUnit D2)
bufexpr0 time = pure $ speaker' (playBuf ("Windchime-" <> "Performance---Tiny-Metal" <> "-l") (1.0 + 0.2 * (sin (0.1 * time))))

-------------- bowl
-- Medium---Perform-1 (B, so fine)
-- Large---Perform-2 (F#, so tune up a bit)
--------------- tongue
-- Roll_G1__2
-- Roll_A2__1
-- Roll_B2__1
-- Roll_D2__1
-- Roll_E2__1
-- Roll_G2__1
-- Melodic-1
--------------- chimes
-- Tiny-Metal---Jangle-6 (high ambiance, dither)
-- Medium-Metal---Strike-11 (E)
-- Large-Metal---Strike-11
bufv :: Number -> Behavior (AudioUnit D2)
bufv time = pure $ speaker' (playBuf ("Windchime-" <> "Glass---Jangle-1" <> "-l") 1.0)

sceneA :: Number -> Behavior (AudioUnit D2)
sceneA = bufv

coff = 1.0 :: Number

scene :: Number -> Behavior (AudioUnit D2)
scene time =
  pure
    $ speaker
        ( zero
            :| fold
                ( map ((#) time)
                    ( ( mapWithIndex (\i (CSN (Cascade a b c _) d e) -> atT (c + coff) $ playerIctus (d <> show i) d e 1.0 1.0 0.0) (cascadesWithInfoInTime.acc)
                      )
                        <> [ atT (6.5) (\t -> if t > 0.0 then pure $ pannerMono_ "pm01" 0.0 (gainT_' "xyz" ((epwf [ Tuple 0.0 0.0, Tuple 8.0 0.03 ]) time) (sinOsc_ "sso1" (conv440 (-24)))) else Nil) ]
                    )
                )
        )
