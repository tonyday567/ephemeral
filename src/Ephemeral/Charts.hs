{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ephemeral.Charts where

import NumHask.Prelude hiding (zero,one)
import qualified NumHask.Prelude as P
import Chart hiding (norm)
import Ephemeral.Shekel
import Numeric.Backprop
import Lens.Micro
import System.Random.MWC
import Control.Monad.Primitive

scratch :: ([Chart Double], [Hud Double]) -> IO ()
scratch (cs, hs) = writeFile "other/scratch.svg" $ renderHudOptionsChart defaultSvgOptions defaultHudOptions hs cs

shekel' :: Int -> [(Rect Double, Double)]
shekel' grain =
  (\p@(Point x y) ->
     (widen (Point (P.one / fromIntegral grain) (P.one / fromIntegral grain))
      (toRect (PointXY (Point x y))), (evalBP (shekp . norm') p))) <$>
  grid MidPos (P.one :: Rect Double) (Point grain grain)

shekel'' :: Int -> Rect Double -> [(Point Double, Double)]
shekel'' grain r =
  (\p@(Point x y) ->
     ((Point x y), evalBP (shekp . norm') p)) <$>
  grid MidPos r (Point grain grain)

-- > scratch $ shekelc 20 one
shekelc :: Int -> Rect Double -> ([Chart Double], [Hud Double])
shekelc grain r =
  pixelfl (evalBP (shekp . norm')) (PixelOptions defaultPixelStyle (Point grain grain) r)
  (defaultPixelLegendOptions "shekel")

norm_ :: (FromRational a, Multiplicative a, Additive a) => Point a -> Point a
norm_ (Point x y) = Point (10.0 * (x + 0.5)) (10.0 * (y + 0.5))

norm' :: (Backprop a, Distributive a, Subtractive a, Reifies s W, FromRational a) => BVar s (Point a) -> BVar s (Point a)
norm' p =
  p2t p &
  _1 %~~ (\x -> (constVar 10.0 * (x + constVar 0.5))) &
  _2 %~~ (\x -> (constVar 10.0 * (x + constVar 0.5))) &
  t2p

-- > scratch $ first (<> gradLine 0.0005 (gradShekel 10 P.one)) $ shekelc 10 P.one
gradShekel :: Int -> Rect Double -> [(Point Double, Point Double)]
gradShekel grain r =
  (\p@(Point x y) ->
     ((Point x y), gradBP (shekp . norm') p)) <$>
  grid MidPos r (Point grain grain)

gradLine :: Double -> [(Point Double, Point Double)] -> [Chart Double]
gradLine s ps = (\(p,p') -> Chart (LineA (defaultLineStyle & #width .~ 0.005 & #color .~ black)) [PointXY p, PointXY (p + fmap (s *) p')]) <$> ps

distancep :: ExpField a => Point a -> a
distancep (Point x y) = sqrt (x ^ (2::Int) + y ^ (2::Int))

normp :: ExpField a => Point a -> Point a
normp p = fmap (/distancep p) p

uniformP :: (PrimMonad m, Variate a) => Gen (PrimState m) -> Rect a -> m (Point a)
uniformP g (Rect x z y w) = Point <$> (uniformR (x,z) g) <*> (uniformR (y,w) g)

-- | source for random points
--
-- > t1 <- rvp 5 P.one
-- > scratch $ first (<> gradLine 0.001 (zip t1 (gradBP (shekp . norm') <$> t1))) $ shekelc 10 P.one
rvp :: Int -> Rect Double -> IO [Point Double]
rvp n r = do
  g <- create
  replicateM n (uniformP g r)


