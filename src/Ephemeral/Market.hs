{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | https://en.wikipedia.org/wiki/Shekel_function
module Ephemeral.Market where

import Box.Csv
import Chart
import Control.Category
import NumHask.Prelude as P
-- import Numeric.Backprop
import NumHask.Backprop ()
import Lens.Micro
import qualified Data.Attoparsec.Text as A
import Data.Time
import Data.Mealy
import Data.Mealy.Quantiles
import qualified Control.Lens as L
import qualified Data.List as List
import System.Random.MWC
import Control.Monad.Primitive
import Data.List ((!!))
import Ephemeral.Charts

-- | yahoo csv data
-- See: https://finance.yahoo.com/quote/%5EGSPC/history?period1=-631015200&period2=1497103200&interval=1d&filter=history&frequency=1d
data YahooData
  = YahooData
      { yDate :: Day,
        yOpen :: Double,
        yHigh :: Double,
        yLow :: Double,
        yClose :: Double,
        yAdjClose :: Double,
        yVolume :: Double
      }
  deriving (Generic, Show)

pDay :: A.Parser Day
pDay = do
  y <- A.decimal
  _ <- A.char '-'
  m <- A.decimal
  _ <- A.char '-'
  d <- A.decimal
  pure $ fromGregorian y m d

-- | yahoo data format
-- >>> A.parseOnly (pYahoo ',') "1927-12-30,17.660000,17.660000,17.660000,17.660000,17.660000,0"
-- Right (YahooData {yDate = "1927-12-30", yOpen = 17.66, yHigh = 17.66, yLow = 17.66, yClose = 17.66, yAdjClose = 17.66, yVolume = 0.0})
pYahoo :: Char -> A.Parser YahooData
pYahoo c = do
  d <- pDay
  _ <- A.char c
  o <- A.double
  _ <- A.char c
  h <- A.double
  _ <- A.char c
  l <- A.double
  _ <- A.char c
  close <- A.double
  _ <- A.char c
  ac <- A.double
  _ <- A.char c
  v <- A.double
  pure (YahooData d (realToFrac o) (realToFrac h) (realToFrac l) (realToFrac close) (realToFrac ac) (realToFrac v))

yahooCsvConfig :: CsvConfig
yahooCsvConfig = defaultCsvConfig & #name .~ "data" & #fsep .~ ',' & #dir .~ "./other"

-- compute the log return from a price series
-- returns are geometric by nature, and using log(1+daily return) as the base unit of the time series leads to all sorts of benefits, not least of which is you can then add up the variates to get the cumulative return, without having to go through log and exp chicanery.  Ditto for distributional assumptions.
lret :: (ExpField a) => [a] -> [a]
lret xs = drop 1 $ scan ((\x x' -> log (x/x')) <$> id <*> delay [P.zero]) xs

getdc :: [YahooData] -> [(Day, Double)]
getdc xs = zip (drop 1 $ (L.view #yDate) <$> xs) (lret ((L.view #yClose) <$> xs))

taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse

makeReturnSeries :: Int -> IO [(Day, Double)]
makeReturnSeries n = do
  r <- runCsv yahooCsvConfig pYahoo
  pure $ taker n $ getdc $ rights r

-- * representation
data MarketConfig = MarketConfig
  { mN :: Int,
    mRunup :: Int
  } deriving (Eq, Show, Generic)

defaultMarketConfig :: MarketConfig
defaultMarketConfig = MarketConfig 10000 2000

defaultQuantiles :: [Double]
defaultQuantiles = ((0.1 *) <$> [1 .. 9])

quantileNames :: [Double] -> [Text]
quantileNames qs' = (<> "th") . comma (Just 0) . (100 *) <$> qs'

nAll :: MarketConfig -> Int
nAll c = ((c ^. #mN) + (c ^. #mRunup))

makeReturns :: MarketConfig -> IO [(Day, Double)]
makeReturns c = makeReturnSeries ((c ^. #mN) + (c ^. #mRunup))

signalize :: Double -> [Double] -> Mealy Double Double
signalize r qs' =
  (\x -> fromIntegral x/fromIntegral (length defaultQuantiles)) <$> digitize r qs'

-- | perf takes a function that produces a signal, a mealy fold, and a list of returns, and outputs a return series based on the signal.
perf :: (a -> Double) -> Mealy Double a -> [Double] -> [Double]
perf f m xs = scan ((\d x -> (f d * x)) <$> (m <<< delay [1]) <*> id) xs

-- | Fold that produces a Point representing the last return signal, and a moving average of that signal.
--
-- Abstraction hinge.
mealyp :: Double -> Double -> [Double] -> Mealy Double (Point Double)
mealyp r r' qs' = Point <$> sig <*> (ma r <<< sig)
  where
    sig = signalize r' qs'

-- | take a point and look up 2 dim parameter signal
--
-- conversion from quantile to signal given a parameter set
-- requires the last quantile cut to be 1
applyp :: MarketState Double -> Point Double -> Double
applyp ms (Point x y) = (ms ^. #params) List.!! cut
  where
    cutx = cutI qs' x
    cuty = cutI qs' y
    cut = cutx + length qs' * cuty
    qs' = ms ^. #qs <> [1]

-- | perfms takes the raw return series, and a MarketState, and outputs the accumulated return of applying the marketstate to the series.
--
-- hinge for other measures of return series attributes
perfms :: [Double] -> MarketState Double -> Double
perfms xs ms = sum $ perf (applyp ms) (mealyp (ms ^. #masig) (ms ^. #qest) defaultQuantiles) xs

-- | take a point, modify market state based on the indices, and measure performance
perfp :: [Double] -> Point Int -> MarketState Double -> Point Double -> Double
perfp xs i ms p = perfms xs (tweakpMS i p ms)

-- | MarketState is the parameters to be explored.
data MarketState a = MarketState
  { -- | rate of ma signal
    masig :: a,
    -- | rate of quantile estimation
    qest :: a,
    -- | parameters transduce a signal
    params :: [a],
    -- | quantile cuts (1 is the last cut that is baked in)
    qs :: [a]
  } deriving (Eq, Show, Generic)

defaultMarketState :: MarketState Double
defaultMarketState = MarketState 0.9 0.9 [0.5,-0.5,0.5,-0.5] [0.8]

rangeMS :: MarketState (Range Double)
rangeMS = MarketState (Range 0.9 1) (Range 0.9 1) [one, one, one, one] [((+0.5) <$> one)]

-- | convert from a MarketState to a list
--
-- > defaultMarketState == (toMS_ $ fromMS_ defaultMarketState)
fromMS_ :: MarketState a -> ((Int, Int), [a])
fromMS_ ms =
  (((length (ms ^. #params), length (ms ^. #qs)),
     [ (ms ^. #masig),
       (ms ^. #qest)] <>
     ((ms ^. #params)) <>
     ((ms ^. #qs))))

-- | convert from a list to a MarketState
--
toMS_ :: ((Int, Int), [a]) -> MarketState a
toMS_ ((lp, lq), xs) =
  MarketState
  ((xs!!0))
  ((xs!!1))
  ((take lp $ drop 2 xs))
  ((take lq $ drop (2+lp) xs))

-- | normalise MarketState elements to being a signal (-0.5 to 0.5)
scaleMS :: MarketState Double -> MarketState Double
scaleMS ms =
  ms &
  #masig %~ project (rangeMS ^. #masig) one &
  #qest %~ project (rangeMS ^. #qest) one &
  #params %~ (\ps -> zipWith (\r p -> project r one p) (rangeMS ^. #params) ps) &
  #qs %~ (\xs -> zipWith (\r p -> project r one p) (rangeMS ^. #qs) xs)

-- | tweak a parameter indexed by the list version of a MarketState
--
-- > tweakiMS 0 0.8 defaultMarketState
-- MarketState {masig = 0.8, qest = 0.99, params = [0.99,0.0,0.0,0.0], qs = [0.5,0.5]}
tweakiMS :: Int -> Double -> MarketState Double -> MarketState Double
tweakiMS n x ms = toMS_ $ ((lp,lq), (take n xs) <> [x] <> (drop (n+1) xs))
  where
    ((lp,lq), xs) = fromMS_ ms
-- | tweak 2 parameters
--
-- > tweakpMS (Point 0 1) (Point 0.8 0.8) defaultMarketState
-- MarketState {masig = 0.8, qest = 0.8, params = [0.0,0.0,0.0,0.5], qs = [0.5,1.0]}
tweakpMS :: Point Int -> Point Double -> MarketState Double -> MarketState Double
tweakpMS (Point x' y') (Point x y) ms = tweakiMS y' y (tweakiMS x' x ms)

-- | Select a change in point and climb by taking a fixed step, rejecting worse points.
climbP :: (Monad m, PrimMonad m) => Gen (PrimState m) -> Double -> (Point Double -> Double) -> Point Double -> m (Point Double)
climbP g step f p = do
  d <- uniformP g one
  let p' = p + ((step *) <$> normp d)
  pure $ bool p p' (f p' > f p)

-- | surface chart over two dimensions
marketSurface :: [Double] -> Point Int -> Int -> MarketState Double -> (HudOptions, [Hud Double], [Chart Double])
marketSurface xs i@(Point ix' iy) grain ms = surface_ grain (Ranges ((snd $ fromMS_ rangeMS)!!ix') ((snd $ fromMS_ rangeMS)!!iy)) (perfp xs i ms)

-- | Select a change in the MarketState list and climb by taking a fixed step, rejecting worse points.
climbList :: (Monad m, PrimMonad m) => Gen (PrimState m) -> Double -> Int -> ([Double] -> Double) -> [Double] -> m [Double]
climbList g step l f p = do
  xs <- uniformList g l
  let p' = zipWith (+) p ((step *) <$> normList xs)
  pure $ bool p p' (f p' > f p)

-- | Generate a random list of signals
uniformList :: (PrimMonad m, Variate a, FromRational a) => Gen (PrimState m) -> Int -> m [a]
uniformList g l = replicateM l (uniformR (-0.5, 0.5) g)

-- | climbing requires a distance metric
distanceList :: ExpField a => [a] -> a
distanceList xs = sqrt . sum $ (^ (2::Int)) <$> xs

normList :: ExpField a => [a] -> [a]
normList xs = fmap (/distanceList xs) xs

initialPop :: (PrimMonad m, Variate a, FromRational a) => Int -> m [[a]]
initialPop n = do
  g <- create
  replicateM n (uniformList g 8)

nextPop :: Gen (PrimState IO) -> [Double] -> StateT [[Double]] IO ()
nextPop g xs = do
  pop <- get
  pop' <- lift $ sequence $ climbList g 0.01 7 (perfms xs . (\x -> toMS_ ((4,1), x))) <$> pop
  put pop'


chartMS :: [[Double]] -> [[[Chart Double]]]
chartMS xss =
  (\d2 ->
     (\d1 -> ([Chart (GlyphA defaultGlyphStyle)
             ((\xs -> PointXY $ selectDim xs (Point d1 d2)) <$> xss)])) <$>
     [0..7]) <$>
  [0..7]

hudAndChart :: [Hud Double] -> [Chart Double] -> [Chart Double]
hudAndChart hs cs = runHud (fixRect $ dataBox cs) hs cs

stackedChart :: [[[Chart Double]]] -> [Chart Double]
stackedChart css = (hscols <>) $ vert 0.1 rows
  where
    rows :: [[Chart Double]]
    rows = ([hsrows] <>) (hori 0.01 <$> css)
    hsrows :: [Chart Double]
    hsrows = mconcat $ (\(h, c) -> runHudWith one one h (c <> [Chart BlankA [RectXY one]])) <$> ((\x -> makeHud (Rect (-0.5) 0.5 (x) ((x+1))) (defaultHudOptions & #hudAxes .~ [defaultAxisOptions & #place .~ PlaceLeft])) <$> [0..1])
    hscols = mconcat $ (\(h, c) -> runHudWith one one h (c <> [Chart BlankA [RectXY one]])) <$> ((\x -> makeHud (Rect x (x+1) (-0.5) 0.5) (defaultHudOptions & #hudAxes .~ [defaultAxisOptions & #place .~ PlaceLeft])) <$> [0..1])

testChart :: IO ()
testChart = do
  pop <- initialPop 1
  let cs = stackedChart $ chartMS pop
  scratch $ (mempty, [], cs)

selectDim :: [Double] -> Point Int -> Point Double
selectDim xs (Point x y) = Point (xs!!x) (xs!!y)


trimmings :: (HudOptions, [Hud Double], [Chart Double]) -> [Chart Double]
trimmings (ho, hs, cs) = runHud datarect (hs<>hoh) (cs<>hoc)
  where
    datarect = fixRect $ styleBoxes cs
    (hoh, hoc) = makeHud datarect ho

-- | testing
scratch' :: (HudOptions, [Hud Double], [Chart Double]) -> IO ()
scratch' = writeFile "other/scratch.svg" . renderCharts . trimmings

