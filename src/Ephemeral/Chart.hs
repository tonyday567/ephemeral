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

import Ephemeral.Point
import NumHask.Prelude hiding (zero,one)
import Chart hiding (norm)

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLabels
-- >>> import qualified NumHask.Prelude as P
-- >>> import Numeric.Backprop

-- | default chart of a surface
--
-- >>> import Ephemeral.Shekel (shekelp)
-- >>> let (cs,hs) = surface 20 P.one (evalBP shekelp)
-- >>> writeFile "other/surface.svg" $ renderHudOptionsChart defaultSvgOptions defaultHudOptions hs cs
--
-- ![surface chart](other/surface.svg)
--
surface ::
  Int ->
  Rect Double ->
  (Point Double -> Double) ->
  ([Chart Double], [Hud Double])
surface grain r f =
  pixelfl f (PixelOptions defaultPixelStyle (Point grain grain) r)
  (defaultPixelLegendOptions mempty)

-- | testing
scratch :: (HudOptions, [Hud Double], [Chart Double]) -> IO ()
scratch (ho, hs, cs) = writeFile "other/scratch.svg" $ renderHudOptionsChart defaultSvgOptions ho hs cs

