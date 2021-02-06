{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Daily stock market data for use in testing.
module Ephemeral.Market
  ( YahooData(..),
    pYahoo,
    yahooCsvConfig,
    lret,
    rtake,
    MarketConfig(..),
    defaultMarketConfig,
    nAll,
    defaultQuantiles,
    quantileNames,
    makeReturns,
    signalize,
  ) where

import Box.Csv
import Chart
import Control.Category
import NumHask.Prelude as P
import Lens.Micro
import qualified Data.Attoparsec.Text as A
import Data.Time
import Data.Mealy
import Data.Mealy.Quantiles
import qualified Control.Lens as L

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLabels
-- >>> import Chart.Various
-- >>> rs <- makeReturns (MarketConfig 1000 0)
--

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

-- | Parser for a 'Day' in yahoo format.
--
-- >>> A.parseOnly pDay "2021-01-01"
-- Right 2021-01-01
pDay :: A.Parser Day
pDay = do
  y <- A.decimal
  _ <- A.char '-'
  m <- A.decimal
  _ <- A.char '-'
  d <- A.decimal
  pure $ fromGregorian y m d

-- | yahoo data format
--
-- >>> A.parseOnly (pYahoo ',') "1927-12-30,17.660000,17.660000,17.660000,17.660000,17.660000,0"
-- Right (YahooData {yDate = 1927-12-30, yOpen = 17.66, yHigh = 17.66, yLow = 17.66, yClose = 17.66, yAdjClose = 17.66, yVolume = 0.0})
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

-- | csv data is in other/data.csv
yahooCsvConfig :: CsvConfig
yahooCsvConfig = defaultCsvConfig & #name .~ "data" & #fsep .~ ',' & #dir .~ "./other"

-- | Compute the log return from a price series.
--
-- Returns are geometric by nature, and using log(1+daily return) as the base unit of the time series leads to all sorts of benefits, not least of which is you can then add up the variates to get the cumulative return, without having to go through log and exp chicanery.  Ditto for distributional assumptions.
lret :: (ExpField a) => [a] -> [a]
lret xs = drop 1 $ scan ((\x x' -> log (x/x')) <$> id <*> delay [P.zero]) xs

-- | convert YahooData to a daily return series
getdc :: [YahooData] -> [(Day, Double)]
getdc xs = zip (drop 1 $ (L.view #yDate) <$> xs) (lret ((L.view #yClose) <$> xs))

-- | reverse take
--
-- >>> rtake 3 [1..10]
-- [8,9,10]
rtake :: Int -> [a] -> [a]
rtake n = reverse . take n . reverse

-- | Create a tuple of the last n days returns.
makeReturnSeries :: Int -> IO [(Day, Double)]
makeReturnSeries n = do
  r <- runCsv yahooCsvConfig pYahoo
  pure $ rtake n $ getdc $ rights r
    where
-- |
--
-- >>> defaultMarketConfig
-- MarketConfig {mN = 10000, mRunup = 2000}
data MarketConfig = MarketConfig
  { -- | number of (most recent) days
    mN :: Int,
    -- | runup length
    mRunup :: Int
  } deriving (Eq, Show, Generic)

-- |
defaultMarketConfig :: MarketConfig
defaultMarketConfig = MarketConfig 10000 2000

-- |
--
-- >>> quantileNames defaultQuantiles
-- ["10th","20th","30th","40th","50th","60th","70th","80th","90th"]
defaultQuantiles :: [Double]
defaultQuantiles = (0.1 *) <$> [1 .. 9]

-- |
quantileNames :: [Double] -> [Text]
quantileNames qs' = (<> "th") . comma (Just 0) . (100 *) <$> qs'

-- | number of days plus runup
--
-- >>> nAll defaultMarketConfig
-- 12000
nAll :: MarketConfig -> Int
nAll c = ((c ^. #mN) + (c ^. #mRunup))

-- |
--
-- >>> rs <- makeReturns defaultMarketConfig
-- >>> length rs
-- 12000
--
-- >>> fixed (Just 5) $ sum (snd <$> rs) / (fromIntegral $ length rs)
-- "0.00027"
--
-- >>> exp $ sum (snd <$> rs)
-- 25.218060748460402
--
-- >>> writeChartSvg "other/allreturns.svg" $ ChartSvg defaultSvgOptions (defaultHudOptions & #hudAxes .~ tsAxes ((\x -> UTCTime x 0) . fst <$> rs)) [] (stdLineChart 0.002 [Colour 0.5 0.4 0.3 1] [scanl (+) 0 (snd <$> rs)])
--
-- ![all returns](other/allreturns.svg)
--
makeReturns :: MarketConfig -> IO [(Day, Double)]
makeReturns c = makeReturnSeries ((c ^. #mN) + (c ^. #mRunup))

-- | Fold that converts a time series into a quantile estimate series, based on recent history
--
-- eg the 10 most recent returns
--
-- >>> fixed (Just 4) <$> rtake 10 (snd <$> rs)
-- ["0.0330","-0.0161","-0.0451","0.0226","-0.0153","0.0680","-0.0016","0.0335","0.0144","-0.0102"]
--
-- and their quantiles
--
-- >>> quantileNames <$> rtake 10 $ scan (signalize 0.99 defaultQuantiles) (snd <$> rs)
-- ["90th","10th","0th","90th","10th","90th","30th","90th","80th","10th"]
--
signalize :: Double -> [Double] -> Mealy Double Double
signalize r qs' =
  (\x -> fromIntegral x/fromIntegral (length qs' + 1)) <$> digitize r qs'







{-
Old market state guff

-- | MarketState represents the current "state" of the market in quantile terms.
--
-- >>> defaultMarketState
-- MarketState {masig = 0.9, qest = 0.9, params = [0.5,0.0,0.2,-0.5], qs = [0.8]}
--
-- Meaning:
--
-- - /masig = 0.9/: set moving average decay rate to 0.9 (= 10 day average)
-- - /qest = 0.9/: set quantile estiamting on a 10 day decay rate
-- - /qs = [0.8]/: two categories of signal; quantile of 0 to 0.8 or 0.8 to 1
-- - /params = [0.5,0.0,0.2,-0.5]/:
--   current quantile is 0 to 0.8 and average quantile is 0 to 0.8: trading signal is 0.5
--   current quantile is 0 to 0.8 and average quantile is 0.8 to 1: trading signal is 0.0
--   current quantile is 0.8 to 1 and average quantile is 0 to 0.8: trading signal is 0.2
--   current quantile is 0.8 to 1 and average quantile is 0.8 to 1: trading signal is -0.5
--
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

-- |
defaultMarketState :: MarketState Double
defaultMarketState = MarketState 0.9 0.9 [0.5,0,0.2,-0.5] [0.8]

-- | Reasonable ranges for the 2-quantile case.
rangeMS :: MarketState (Range Double)
rangeMS = MarketState (Range 0.9 1) (Range 0.9 1) [one, one, one, one] [((+0.5) <$> one)]

-- | convert from a MarketState to a list
--
-- >>> defaultMarketState == (toMS_ $ fromMS_ defaultMarketState)
-- True
--
-- >>> fromMS_ defaultMarketState
-- ((4,1),[0.9,0.9,0.5,0.0,0.2,-0.5,0.8])
fromMS_ :: MarketState a -> ((Int, Int), [a])
fromMS_ ms =
  (((length (ms ^. #params), length (ms ^. #qs)),
     [ (ms ^. #masig),
       (ms ^. #qest)] <>
     ((ms ^. #params)) <>
     ((ms ^. #qs))))

-- | convert from a list to a MarketState
--
-- >>> toMS_ ((4,1),[0.9,0.9,0.5,0.0,0.2,-0.5,0.8])
-- MarketState {masig = 0.9, qest = 0.9, params = [0.5,0.0,0.2,-0.5], qs = [0.8]}
toMS_ :: ((Int, Int), [a]) -> MarketState a
toMS_ ((lp, lq), xs) =
  MarketState
  ((xs!!0))
  ((xs!!1))
  ((take lp $ drop 2 xs))
  ((take lq $ drop (2+lp) xs))

-- | normalise MarketState elements to being a signal (all values are in the range -0.5 to 0.5)
--
-- >>> scaleMS defaultMarketState
-- MarketState {masig = -0.5, qest = -0.5, params = [0.5,0.0,0.19999999999999996,-0.5], qs = [0.30000000000000004]}
scaleMS :: MarketState Double -> MarketState Double
scaleMS ms =
  ms &
  #masig %~ project (rangeMS ^. #masig) one &
  #qest %~ project (rangeMS ^. #qest) one &
  #params %~ (\ps -> zipWith (\r p -> project r one p) (rangeMS ^. #params) ps) &
  #qs %~ (\xs -> zipWith (\r p -> project r one p) (rangeMS ^. #qs) xs)

-- | tweak a parameter indexed by the list version of a MarketState
--
-- >>> tweak1dMS 0 0.8 defaultMarketState
-- MarketState {masig = 0.8, qest = 0.9, params = [0.5,0.0,0.2,-0.5], qs = [0.8]}
tweak1dMS :: Int -> Double -> MarketState Double -> MarketState Double
tweak1dMS n x ms = toMS_ $ ((lp,lq), (take n xs) <> [x] <> (drop (n+1) xs))
  where
    ((lp,lq), xs) = fromMS_ ms

-- | tweak 2 parameters
--
-- >>> tweak2dMS (Point 0 1) (Point 0.8 0.8) defaultMarketState
-- MarketState {masig = 0.8, qest = 0.8, params = [0.5,0.0,0.2,-0.5], qs = [0.8]}
tweak2dMS :: Point Int -> Point Double -> MarketState Double -> MarketState Double
tweak2dMS (Point x' y') (Point x y) ms = tweak1dMS y' y (tweak1dMS x' x ms)

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

-- | perfms takes the time series, and a MarketState, and outputs the accumulated return of applying the MarketState parameters to the series.
--
-- hinge for other measures of return series attributes
perfms :: [Double] -> MarketState Double -> Double
perfms xs ms = sum $ perf (applyp ms) (mealyp (ms ^. #masig) (ms ^. #qest) defaultQuantiles) xs

-- | perf takes a function that produces a signal, a mealy fold, and a list of returns, and outputs a return series based on the signal.
perf :: (a -> Double) -> Mealy Double a -> [Double] -> [Double]
perf f m xs = scan ((\d x -> (f d * x)) <$> (m <<< delay [1]) <*> id) xs

-- | A Fold that produces a Point representing the last return signal, and a moving average of that signal.
--
mealyp :: Double -> Double -> [Double] -> Mealy Double (Point Double)
mealyp r r' qs' = Point <$> sig <*> (ma r <<< sig)
  where
    sig = signalize r' qs'

-- | take a point, modify market state based on the indices, and measure performance
perfp :: [Double] -> Point Int -> MarketState Double -> Point Double -> Double
perfp xs i ms p = perfms xs (tweak2dMS i p ms)

-- | Select a change in point and climb by taking a fixed step, rejecting worse points.
climbP :: (Monad m, PrimMonad m) => Gen (PrimState m) -> Double -> (Point Double -> Double) -> Point Double -> m (Point Double)
climbP g step f p = do
  d <- uniformP g one
  let p' = p + ((step *) <$> normp d)
  pure $ bool p p' (f p' > f p)

-- | surface chart over two dimensions
--
-- > xs <- fmap snd <$> makeReturnSeries 1000
-- > scratch $ marketSurface xs (Point 0 1) 10 defaultMarketState
marketSurface :: [Double] -> Point Int -> Int -> MarketState Double -> (HudOptions, [Hud Double], [Chart Double])
marketSurface xs i@(Point ix' iy) grain ms =
  surface_ grain
  (Ranges ((snd $ fromMS_ rangeMS)!!ix') ((snd $ fromMS_ rangeMS)!!iy))
  (perfp xs i ms)

-- | Select a change in the MarketState list and climb by taking a fixed step, rejecting worse points.
climbList :: (Monad m, PrimMonad m) => Gen (PrimState m) -> Double -> Int -> ([Double] -> Double) -> [Double] -> m [Double]
climbList g step l f p = do
  xs <- uniformList g l
  let p' = zipWith (+) p ((step *) <$> normList xs)
  pure $ bool p p' (f p' > f p)

-- | Generate a random list of signals
uniformList :: (PrimMonad m, Variate a, FromRational a, Subtractive a) => Gen (PrimState m) -> Int -> m [a]
uniformList g l = replicateM l (uniformR (-0.5, 0.5) g)

-- | climbing requires a distance metric
distanceList :: ExpField a => [a] -> a
distanceList xs = sqrt . sum $ (^ (2::Int)) <$> xs

normList :: ExpField a => [a] -> [a]
normList xs = fmap (/distanceList xs) xs

initialPop :: (PrimMonad m, Variate a, FromRational a, Subtractive a) => Int -> m [[a]]
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

stackedChart :: Double -> [[[Chart Double]]] -> [Chart Double]
stackedChart gap css = vert gap rows
  where
    rows = (hori gap <$> css)

{-
stackedChart' :: Double -> HudOptions -> HudOptions -> [Text] -> [[[Chart Double]]] -> [Chart Double]
stackedChart' gap rowho colho ts css = vert gap rows
  where
    rows = (hori gap <$> css')
    css' = fmap
      (\c ->
          trimmings
          (defaultHudOptions &
            #hudAxes %~ fmap
            (#abar .~ Nothing &
              #adjust .~ Nothing &
              #atick . #tick .~ Nothing), [], c)) <$>
      css

-}


{-
testChart :: IO ()
testChart = do
  pop <- initialPop 1
  let cs = stackedChart $ chartMS pop
  scratch $ (mempty, [], cs)

-}

selectDim :: [Double] -> Point Int -> Point Double
selectDim xs (Point x y) = Point (xs!!x) (xs!!y)

t1 :: Int -> Double -> IO ()
t1 n gap = do
  let cs = Chart (GlyphA defaultGlyphStyle) ((\x -> PointXY (Point (fromIntegral x) (fromIntegral x))) <$> [0..10::Int])
  -- let cl = pixelLegendChart one (defaultPixelLegendOptions "title")
  --let hs = legendHud defaultLegendOptions cl
  let cs' = trimmings (scaleHudOptions (recip . fromIntegral $ n) defaultHudOptions, [], [cs])
  let css' = replicate n (replicate n cs')
  let css'' = stackedChart gap css'
  scratch $ (mempty, [], css'')

trimmings :: (HudOptions, [Hud Double], [Chart Double]) -> [Chart Double]
trimmings (ho, hs, cs) = runHud datarect (hs<>hoh) (cs<>hoc)
  where
    datarect = fixRect $ styleBoxes cs
    (hoh, hoc) = makeHud datarect ho

-- | testing
scratch' :: (HudOptions, [Hud Double], [Chart Double]) -> IO ()
scratch' = writeFile "other/scratch.svg" . renderCharts . trimmings

scaleHudOptions :: Double -> HudOptions -> HudOptions
scaleHudOptions x ho =
  ho &
  (#hudCanvas %~ fmap (scaleRectStyle x)) &
  (#hudTitles %~ fmap (scaleTitle x)) &
  (#hudAxes %~ fmap (scaleAxis x)) &
  (#hudLegend %~ fmap (first (scaleLegendOptions x) . second (fmap (first (scaleAnnotation x)))))

scaleAxis :: Double -> AxisOptions -> AxisOptions
scaleAxis x ao =
  ao &
  #abar %~ fmap (scaleBar x) &
  -- #adjust %~ fmap (scaleAdjustments x) &
  #atick %~ scaleTick x

scaleTitle :: Double -> Title -> Title
scaleTitle x t = t & ((#style %~ (#size %~ (*x)))) & (((#buff %~ (*x))))

scaleRectStyle :: Double -> RectStyle -> RectStyle
scaleRectStyle x rs = rs & (#borderSize %~ (*x))

scaleBar :: Double -> Bar -> Bar
scaleBar x b = b & (#rstyle %~ scaleRectStyle x) & (#wid %~ (*x)) & (#buff %~ (*x))

scaleAdjustments :: Double -> Adjustments -> Adjustments
scaleAdjustments x a = a & (#maxXRatio %~ (*x)) & (#maxYRatio %~ (*x)) & (#angledRatio %~ (*x))

scaleTick :: Double -> Tick -> Tick
scaleTick x t =
  t &
  (#gtick %~ fmap (first ((#size %~ (*x)) . (#borderSize %~ (*x))) . second (*x))) &
  (#ttick %~ fmap (first (#size %~ (*x)) . second (*x))) &
  (#ltick %~ fmap (first (#width %~ (*x)) . second (*x)))

scaleLegendOptions :: Double -> LegendOptions -> LegendOptions
scaleLegendOptions x lo =
  lo &
  #lscale %~ (*x)

scaleAnnotation :: Double -> Annotation -> Annotation
scaleAnnotation x (LineA ls) = LineA (ls & #width %~ (*x))
scaleAnnotation x (TextA ts t) = TextA (ts & #size %~ (*x)) t
scaleAnnotation x (GlyphA gs) = GlyphA (gs & #size %~ (*x) & #borderSize %~ (*x))
scaleAnnotation x (RectA rs) = RectA (rs & #borderSize %~ (*x))
scaleAnnotation _ BlankA = BlankA
scaleAnnotation x (PixelA ps) = PixelA (ps & #pixelRectStyle . #borderSize %~ (*x))



-}

