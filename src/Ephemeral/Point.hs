{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Point functions
--
module Ephemeral.Point
  ( Point(..),
    toPoint,
    fromPoint,
    fromTuple,
    toTuple,
    uniformPoint,
    uniformPoints,
    distancep,
    normp,
    dir,
  ) where

import NumHask.Prelude as P
import NumHask.Space hiding (toPoint)
import Numeric.Backprop
import NumHask.Backprop ()
import System.Random
import System.Random.Stateful

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLabels
-- >>> let g = mkStdGen 42

-- | generate a random 'Point' within a 'Rect'
--
-- >>> fst $ uniformPoint P.one g
-- Point 0.43085240252163404 -6.472345419562497e-2
--
uniformPoint :: (UniformRange a, StatefulGen g f) => Rect a -> g -> f (Point a)
uniformPoint (Rect x z y w) g =
  Point <$> uniformRM (x,z) g <*> uniformRM (y,w) g

-- | source for random points
--
-- >>> uniformPoints 5 P.one g
-- [Point 0.43085240252163404 -6.472345419562497e-2,Point 0.3854692674681801 0.35219907588979993,Point 0.2501453784152332 -0.10033733661000876,Point -2.7632912108999985e-2 0.15031069068627856,Point 6.1753434058297446e-2 -0.38108041498656287]
uniformPoints :: (RandomGen g, UniformRange a) => Int -> Rect a -> g -> [Point a]
uniformPoints n r g = runStateGen_ g (replicateM n . uniformPoint r)

-- | (unsafely) convert a 'BVar' from a list to a 'Point'
toPoint :: (Reifies s W) => BVar s [Double] -> BVar s (Point Double)
toPoint = isoVar (\xs -> case xs of
                     [] -> P.zero
                     [x] -> Point x P.zero
                     (x:y:_) -> Point x y)
           (\(Point x y) -> [x,y])

-- | convert a 'BVar' from a 'Point' to a list
fromPoint :: (Reifies s W) => BVar s (Point Double) -> BVar s [Double]
fromPoint =
  isoVar
  (\(Point x y) -> [x,y])
  (\xs -> case xs of
           [] -> P.zero
           [x] -> Point x P.zero
           (x:y:_) -> Point x y)

-- orphans
instance (Ring a) => Backprop (Point a) where
  zero _ = P.zero
  add = (+)
  one _ = P.one

-- | convert a BVar from a pair tuple to a 'Point'
fromTuple :: (Backprop a, Reifies s W) => BVar s (a, a) -> BVar s (Point a)
fromTuple =
  isoVar
  (\(x,y) -> Point x y)
  (\(Point x y) -> (x,y))

-- | convert a BVar from a 'Point' to a pair tuple
toTuple :: (Subtractive a, Distributive a, Reifies s W) => BVar s (Point a) -> BVar s (a, a)
toTuple =
  isoVar
  (\(Point x y) -> (x,y))
  (\(x,y) -> Point x y)

-- | distance metric
distancep :: ExpField a => Point a -> a
distancep (Point x y) = sqrt (x^2 + y^2)

-- | norm of a 'Point'
normp :: ExpField a => Point a -> Point a
normp p = fmap (/distancep p) p

-- | direction, in degrees
dir :: Point Double -> Double
dir (Point x y) = -1 * atan2 y x * (180/pi)

