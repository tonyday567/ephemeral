{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ephemeral.Charts where

import NumHask.Prelude hiding (zero,one)
import qualified NumHask.Prelude as P
import Chart hiding (norm)
import Chart.Serve
import Ephemeral.Shekel
import Numeric.Backprop
import Lens.Micro
import System.Random.MWC
import Control.Monad.Primitive
import Box
import Control.Monad.Conc.Class as C
import qualified Data.List as List

-- | Generate a point within a rectangle.
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

-- | testing
scratch :: (HudOptions, [Hud Double], [Chart Double]) -> IO ()
scratch (ho, hs, cs) = writeFile "other/scratch.svg" $ renderHudOptionsChart defaultSvgOptions ho hs cs

-- | chart a surface
surface :: Int -> Rect Double -> (forall s. (Reifies s W) => BVar s (Point Double) -> BVar s Double) -> (HudOptions, [Hud Double], [Chart Double])
surface grain r f = (\(cs,hs) -> (defaultHudOptions,hs,cs)) $
  pixelfl (evalBP f) (PixelOptions defaultPixelStyle (Point grain grain) r)
  (defaultPixelLegendOptions mempty)

-- | chart a surface
surface_ :: Int -> Rect Double -> (Point Double -> Double) -> (HudOptions, [Hud Double], [Chart Double])
surface_ grain r f = (\(cs,hs) -> (defaultHudOptions,hs,cs)) $
  pixelfl f (PixelOptions defaultPixelStyle (Point grain grain) r)
  (defaultPixelLegendOptions mempty)

-- | climb a gradient by taking a fixed step, rejecting worse points.
climbGradient :: Double -> (forall s. (Reifies s W) => BVar s (Point Double) -> BVar s Double) -> Point Double -> Point Double
climbGradient r f p = bool p p' (v' > fst bp)
  where
    bp :: (Double, Point Double)
    bp = backprop f p
    p' :: (Point Double)
    p' = p + fmap (r*) (normp (snd bp))
    v' = evalBP f p'

-- | climbing requires a distance metric
distancep :: ExpField a => Point a -> a
distancep (Point x y) = sqrt (x ^ (2::Int) + y ^ (2::Int))

normp :: ExpField a => Point a -> Point a
normp p = fmap (/distancep p) p

-- | Background pixel chart for the surface.
-- > scratch $ shekelc 20 one
shekelc :: Int -> Rect Double -> (HudOptions, [Hud Double], [Chart Double])
shekelc grain r = surface grain r (shekp . norm')

-- | chart latest population
nextShekel :: (Monad m) => Int -> Double -> (forall s. (Reifies s W) => BVar s (Point Double) -> BVar s Double) -> StateT [Point Double] m (HudOptions, [Hud Double], [Chart Double])
nextShekel g r f = do
  ps <- get
  let ps' = climbGradient r f <$> ps
  put ps'
  pure (ho, hs, cs <> chartSnail ps')
    where
      chartSnail ps = zipWith (\p c -> Chart (GlyphA (defaultGlyphStyle & #color .~ c & #size .~ 0.02)) [PointXY p]) ps (List.cycle palette1)
      (ho, hs, cs) = shekelc g P.one

nextE :: (Monad m) => Int -> Double -> (forall s. (Reifies s W) => BVar s (Point Double) -> BVar s Double) -> Emitter (StateT [Point Double] m) (HudOptions, [Hud Double], [Chart Double])
nextE g gap f = Emitter $ do
  n <- nextShekel g gap f
  pure $ Just n

emitNextChart :: (MonadConc m) => Int -> Int -> Double -> (forall s. (Reifies s W) => BVar s (Point Double) -> BVar s Double) -> [Point Double] ->
  Cont m (Emitter m (HudOptions, [Hud Double], [Chart Double]))
emitNextChart n g gap f ps0 = emitQ (\c -> flip evalStateT ps0 (glueN n (hoist lift c) (nextE g gap f)))

emitChart :: Int -> Int -> Double -> (forall s. (Reifies s W) => BVar s (Point Double) -> BVar s Double) -> SConfig -> IO (Cont IO (Emitter IO (HudOptions, [Hud Double], [Chart Double])))
emitChart n g gap f cfg = do
  ps0 <- rvp n P.one
  pure $ delaySecs (1 / cfg ^. #framerate) <$> (emitNextChart (totalFrames cfg) g gap f ps0)

toSvgText :: (HudOptions, [Hud Double], [Chart Double]) -> Text
toSvgText (ho, hs, cs) = renderHudOptionsChart defaultSvgOptions ho hs cs

emitText :: Int -> Int -> Double -> (forall s. (Reifies s W) => BVar s (Point Double) -> BVar s Double) -> SConfig -> IO (Cont IO (Emitter IO Text))
emitText n g gap f cfg = fmap (fmap toSvgText) <$> emitChart n g gap f cfg

runc :: IO ()
runc = do
  e <- emitChart 200 20 0.02 (shekp . norm') (defaultSConfig & #framerate .~ 2 & #runtime .~ 60)
  serveCharts <$.> e

