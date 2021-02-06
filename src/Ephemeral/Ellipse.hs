{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

module Ephemeral.Ellipse
  ( EllipseConfig(..),
    defaultEllipseConfig,
    ellipseProblem,
  ) where

import Chart
import Control.Lens
import NumHask.Prelude hiding (rotate, Down, next, basis)
import qualified Data.Sequence as Seq
import Ephemeral.Search

-- https://math.meta.stackexchange.com/questions/5020/mathjax-basic-tutorial-and-quick-reference/5023#5023

-- https://haskell-haddock.readthedocs.io/en/latest/markup.html

-- https://en.wikipedia.org/wiki/Ellipse

-- https://mathworld.wolfram.com/EllipticModulus.html

-- https://mathworld.wolfram.com/EccentricAnomaly.html

-- https://mathworld.wolfram.com/Ellipse.html

-- https://www.w3.org/TR/SVG/implnote.html#ArcConversionEndpointToCenter

-- $setup
--
-- >>> :set -XOverloadedLabels
-- >>> let cfg = defaultEllipseConfig

-- * ellipse problem
-- |
-- >>> defaultEllipseConfig
-- EllipseConfig {numPoints = 100, arc0 = ArcCentroid {centroid = Point -0.2864867185179476 1.6092991486979669, radius = Point 1.3866801807145206 2.8942082605509394, cphi = 1.0962340928888052, ang0 = -2.8, angdiff = -5.348100265785992}, projection = Rect -1.0 1.0 -0.5 0.5, guess = ArcCentroid {centroid = Point -0.2864867185179476 1.6092991486979669, radius = Point 1.3858025965450367 2.8858375379037695, cphi = 1.097568874016416, ang0 = -2.792315785988488, angdiff = -5.348100265785992}, tol = 1.0e-6}
data EllipseConfig = EllipseConfig
  { numPoints :: Int,
    arc0 :: ArcCentroid Double,
    projection :: Rect Double,
    guess :: ArcCentroid Double,
    tol :: Double
  }
  deriving (Eq, Show, Generic)

-- |
defaultEllipseConfig :: EllipseConfig
defaultEllipseConfig = EllipseConfig 100 ac0 (aspect 2) g3 1e-6

g3 :: ArcCentroid Double
g3 = ArcCentroid {centroid = Point -0.2864867185179476 1.6092991486979669, radius = Point 1.3858025965450367 2.8858375379037695, cphi = 1.097568874016416, ang0 = -2.792315785988488, angdiff = -5.348100265785992}

ac0 :: ArcCentroid Double
ac0 = ArcCentroid (Point -0.2864867185179476 1.6092991486979669) (Point 1.3866801807145205 2.8942082605509395) 1.0962340928888052 -2.8 -5.348100265785992

acZero :: ArcCentroid Double
acZero = ArcCentroid (Point zero zero) (Point one one) zero zero zero

acAdd :: ArcCentroid Double -> ArcCentroid Double -> ArcCentroid Double
acAdd (ArcCentroid c r a a0 ad) (ArcCentroid c' r' a' a0' ad') = ArcCentroid (c+c') (r*r') (a+a') (a0+a0') (ad+ad')

ellipseProblem :: EllipseConfig -> Problem EllipseConfig Double (SearchConfig Double)
ellipseProblem cfg = Problem basis_ fit_ (defaultSearchConfig $ view #tol cfg)

-- | computes the difference between the projected ellipse and a guess
--
-- >>> fit_ defaultEllipseConfig
-- 170.50196202337645
fit_ :: EllipseConfig -> Double
fit_ cfg =
  sum $
    norm
      <$> zipWith
        (-)
        ( projectOnP (cfg ^. #projection) one
            . ellipse c r phi
            . (\x -> 2 * pi * fromIntegral x / fromIntegral n)
            <$> [0 .. n]
        )
        (ellipse c' r' phi' . (\x -> 2 * pi * fromIntegral x / fromIntegral n) <$> [0 .. n])
  where
    n = cfg ^. #numPoints
    (ArcCentroid c r phi _ _) = view #arc0 cfg
    (ArcCentroid c' r' phi' _ _) = view #guess cfg

-- | The basis for an ArcCentroid
basis_ :: Seq Double -> EllipseConfig -> EllipseConfig
basis_ s c = foldl' (&) c $ Seq.zipWith (&) s base

base :: Seq (Double -> EllipseConfig -> EllipseConfig)
base =
  Seq.fromList
  [ \s x -> x & #guess . #radius %~ (+ Point s 0),
    \s x -> x & #guess . #radius %~ (+ Point 0 s),
    \s x -> x & #guess . #cphi %~ (+ s)
  ]


