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
applyp :: MarketState Double -> Point Double -> Double
applyp ms (Point x y) = (ms ^. #params) List.!! cut
  where
    cutx = cutI (ms ^. #qs) x
    cuty = cutI (ms ^. #qs) y
    cut = cutx + length (ms ^. #qs) * cuty

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
    -- | quantile cuts
    qs :: [a]
  } deriving (Eq, Show, Generic)

defaultMarketState :: MarketState Double
defaultMarketState = MarketState 0.99 0.99 [0,0,0,0.5] [0.5,1]

rangeMS :: MarketState (Range Double)
rangeMS = MarketState (Range 0.9 1) (Range 0.9 1) [one, one, one, one] [((+0.5) <$> one), ((+0.5) <$> one)]

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
climbR :: (Monad m, PrimMonad m) => Gen (PrimState m) -> Double -> (Point Double -> Double) -> Point Double -> m (Point Double)
climbR g step f p = do
  d <- uniformP g one
  let p' = p + ((step *) <$> normp d)
  pure $ bool p p' (f p' > f p)

-- | surface chart over two dimensions
marketSurface :: [Double] -> Point Int -> Int -> MarketState Double -> (HudOptions, [Hud Double], [Chart Double])
marketSurface xs i@(Point ix' iy) grain ms = surface_ grain (Ranges ((snd $ fromMS_ rangeMS)!!ix') ((snd $ fromMS_ rangeMS)!!iy)) (perfp xs i ms)






