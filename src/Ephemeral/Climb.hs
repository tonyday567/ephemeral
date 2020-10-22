{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | combinators for moving around a hill
module Ephemeral.Climb where

import NumHask.Prelude hiding (zero,one)
import Chart hiding (norm)
import Numeric.Backprop
import Lens.Micro
import Box
import Control.Monad.Conc.Class as C
import qualified Data.List as List
import Ephemeral.Point ()

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

-- | norm of a 'Point'
normp :: ExpField a => Point a -> Point a
normp p = fmap (/distancep p) p

-- | chart latest population
nextChart :: (Monad m) => Double -> (forall s. (Reifies s W) => BVar s (Point Double) -> BVar s Double) -> StateT [Point Double] m [Chart Double]
nextChart r f = do
  ps <- get
  let ps' = climbGradient r f <$> ps
  put ps'
  pure $ zipWith
    (\p c ->
        Chart (GlyphA (defaultGlyphStyle & #color .~ c & #size .~ 0.02))
        [PointXY p])
    ps
    (List.cycle palette1)

nextE :: (Monad m) => Double -> (forall s. (Reifies s W) => BVar s (Point Double) -> BVar s Double) -> Emitter (StateT [Point Double] m) [Chart Double]
nextE gap f = Emitter $ do
  n <- nextChart gap f
  pure $ Just n

emitNextChart :: (MonadConc m) => Int -> Double -> (forall s. (Reifies s W) => BVar s (Point Double) -> BVar s Double) -> [Point Double] ->
  Cont m (Emitter m [Chart Double])
emitNextChart n gap f ps0 = emitQ (\c -> flip evalStateT ps0 (glueN n (hoist lift c) (nextE gap f)))

{-
runc :: IO ()
runc = do
  e <- undefined -- emitChart 200 20 0.02 (shekp . norm') (defaultSConfig & #framerate .~ 2 & #runtime .~ 60)
  serveCharts <$.> e

-}
