{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | chart combinators
module Ephemeral.Chart
  ( surface,
    scratch,
  ) where

import NumHask.Prelude hiding (zero,one)
import Chart 
import Control.Lens

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLabels
-- >>> import qualified NumHask.Prelude as P

-- | default chart of a surface
--
-- > import Ephemeral.Shekel
-- > writeChartSvg "other/surface.svg" $ surface 20 P.one shekel
--
-- ![surface chart](other/surface.svg)
--
surface ::
  Int ->
  Rect Double ->
  (Point Double -> Double) ->
  ChartSvg
surface grain r f =
  mempty &
  #hudList .~ hs &
  #chartList .~ cs &
  #svgOptions .~ (defaultSvgOptions & #cssOptions .~ UseCssCrisp)
  where
    (cs, hs) = surfacefl f (SurfaceOptions defaultSurfaceStyle (Point grain grain) r)
      (defaultSurfaceLegendOptions mempty)

-- | testing
scratch :: ChartSvg -> IO ()
scratch cs = writeChartSvg "other/scratch.svg" cs

