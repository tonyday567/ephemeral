{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NegativeLiterals #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

-- |
-- reference:
--
-- - [mathjax](https://math.meta.stackexchange.com/questions/5020/mathjax-basic-tutorial-and-quick-reference/5023#5023)
-- - [haddock](https://haskell-haddock.readthedocs.io/en/latest/markup.html)
-- - [SVG Standards](https://www.w3.org/TR/SVG/implnote.html#ArcParameterizationAlternatives)
-- - [ellipse wiki entry](https://en.wikipedia.org/wiki/Ellipse)
-- - [eccentric anamoly](https://mathworld.wolfram.com/EccentricAnomaly.html)
-- - [wolfram](https://mathworld.wolfram.com/Ellipse.html)
-- - [stackexchange](https://math.stackexchange.com/questions/426150/what-is-the-general-equation-of-the-ellipse-that-is-not-in-the-origin-and-rotate)
--

module Ephemeral.Ellipse
  ( ellipse_,
    theta_,
    Ellipse(..),
    ellipse,
    EllipseSegment(..),
    run,
    run1,
    shortPi,
    shortP,
    shortE,
    shortES,
  ) where

import Chart hiding (ellipse)
import Chart.Reanimate
import Control.Lens
import NumHask.Prelude hiding (rotate, Down, next, basis)
import Reanimate as Re hiding (rotate)
import qualified Data.List as List

-- $setup
-- >>> :set -XOverloadedLabels
-- >>> let cfg = defaultEllipseConfig

-- | ellipse formulae
--
-- >>> shortP $ ellipse zero (Point 2 1) (pi/4) (pi/2)
--
-- \[\dfrac {((x-h)\cos(A)+(y-k)\sin(A))^2}{a^2}+\dfrac{((x-h) \sin(A)-(y-k) \cos(A))^2}{b^2}=1\]
--
-- \[{\displaystyle {\dfrac {(x\cos \theta -y\sin \theta - c_x)^{2}}{a^{2}}}+{\dfrac {(x\sin \theta +y\cos \theta -c_y)^{2}}{b^{2}}}=1}\]
--
ellipse_ :: (Direction b a, Affinity b a, TrigField a) => b -> a -> b -> a -> b
ellipse_ centroid rot radii theta =
  rotate rot |. (radii * ray theta) + centroid

-- (radii * ray ang0) = rotateP (-rot') k1
-- (radii * ray ang1) = rotateP (-rot') k2
-- ray ang1

-- | find the direction of a Point from the centroid of a rotated ellipse.
--
-- >>> shortPi $ theta_ zero (pi/4) (Point 2 1) (Point -0.71 0.71)
-- "0.50pi"
theta_ :: (Divisive b, Subtractive b, Direction b a, Affinity b a, TrigField a) => b -> a -> b -> b -> a
theta_ centroid rot radii pos =
  pos &
  (\x -> x - centroid) &
  (\x -> rotate (-rot) |. x) &
  (/radii) &
  angle

data Ellipse a =
  Ellipse
  { centroid :: Point a,
    radii :: Point a,
    major :: a
  } deriving (Eq, Show, Generic)

shortP :: Point Double -> Text
shortP (Point x y) = fixed (Just 2) x <> "," <> fixed (Just 2) y

shortPi :: Double -> Text
shortPi x = fixed (Just 2) (x / pi) <> "pi"

shortE :: Ellipse Double -> Text
shortE e =
  shortP (view #centroid e) <> " " <>
  shortP (view #radii e) <> " " <>
  shortPi (view #major e)

-- | find the point on the ellipse at an angle.
ellipse :: (TrigField a) => Ellipse a -> a -> Point a
ellipse c theta =
  (rotate (view #major c) |. (view #radii c * ray theta)) + view #centroid c

-- | Arc specification based on centroidal interpretation.
--
-- See: https://www.w3.org/TR/SVG/implnote.html#ArcConversionEndpointToCenter
--
data EllipseSegment a =
  EllipseSegment
  { -- | ellipse
    full :: Ellipse a,
    -- | starting point
    ang0 :: a,
    -- | ending point
    ang1 :: a
  } deriving (Eq, Show, Generic)

shortES :: EllipseSegment Double -> Text
shortES es = shortE (view #full es) <> " " <>
  fixed (Just 2) (view #ang0 es / pi) <> "pi " <>
  fixed (Just 2) (view #ang1 es / pi) <> "pi"

addTitle :: Text -> Double -> ChartSvg -> ChartSvg
addTitle t s =
  (#hudOptions . #hudTitles %~ (\ts -> ts <>
                                [defaultTitle t &
                                  #style . #size .~ s &
                                  #place .~ PlaceBottom])) .
  (#hudOptions %~ colourHudOptions light)

cPoint :: Colour -> Double -> Point Double -> [Chart Double]
cPoint cGlyph s p =
  [ Chart (GlyphA (defaultGlyphStyle &
                   #shape .~ CircleGlyph &
                    #borderSize .~ 0 &
                    #color .~ cGlyph &
                    #size .~ s))
    [PointXY p]
  ]

cLabel :: Colour -> Double -> Point Double -> Text -> Point Double -> [Chart Double]
cLabel c s gap label p =
    [Chart (TextA (defaultTextStyle &
                  #size .~ s &
                  #color .~ c)
            [label])
    [PointXY (gap + p)]]

labelledLine :: Colour -> Double -> Colour -> Double -> Point Double -> Text -> Point Double -> Point Double -> [Chart Double]
labelledLine cLine sLine cText sText textGap label p0 p1 =
    [Chart (LineA $ defaultLineStyle &
           #color .~ cLine &
           #width .~ sLine)
    [PointXY p0, PointXY p1],
    Chart (TextA (defaultTextStyle &
                  #rotation ?~ a &
                  #size .~ sText &
                  #color .~ cText &
                  #anchor .~ AnchorMiddle)
            [label])
    [PointXY $ rotateP a textGap + (0.5 .* (p0 + p1))]]
    where
      a = angle (p1 - p0)

ellipseSeg :: Int -> Colour -> EllipseSegment Double -> [Chart Double]
ellipseSeg n c es =
    [Chart (LineA (defaultLineStyle & #color .~ c))
    (PointXY . ellipse (view #full es) <$> grid OuterPos (Range (view #ang0 es) (view #ang1 es)) n)]

dThetaEllipse :: Ellipse Double -> Double -> ChartSvg
dThetaEllipse e x =
  mempty &
  #hudOptions .~ defaultHudOptions &
  (#chartList .~
  -- centroid
  cPoint (palette1 List.!! 1) 0.04 (view #centroid e) <>
  cLabel c s gap (shortP $ view #centroid e) (view #centroid e) <>
  -- major
  labelledLine (palette1 List.!! 2) 0.005 c s gap (shortPi (view #major e)) (view #centroid e) (ellipse e zero) <>
  ellipseSeg 100 (palette1 List.!! 3) (EllipseSegment e zero (2*pi)) <>

  -- theta animation
  [
    -- theta animation
    Chart (GlyphA (defaultGlyphStyle &
                   #shape .~ CircleGlyph &
                   #borderColor .~ transparent &
                   #borderSize .~ 0 &
                   #color .~ palette1 List.!! 6 &
                   #size .~ 0.01))
    [PointXY $ ellipse e x],
    Chart (TextA (defaultTextStyle &
                  #size .~ s &
                  #color .~ c)
            [shortPi x])
    [PointXY (gap + ellipse e x)]
  ]) &
  addTitle (shortE e) 0.06
  where
    gap = Point 0.0 0.2
    c = setOpac 1 light
    s = 0.03

dThetaEllipseSegment :: EllipseSegment Double -> Double -> ChartSvg
dThetaEllipseSegment es x =
  dThetaEllipse (view #full es) (project (Range zero one) (Range (view #ang0 es) (view #ang1 es)) x) &
  (#hudOptions %~
   (#hudTitles .~ [defaultTitle (shortES es) &
                   #style . #size .~ 0.06 &
                   #place .~ PlaceBottom])) &
  (#hudOptions %~
   colourHudOptions light) &
  #chartList %~ (<>
  [
    Chart (GlyphA (defaultGlyphStyle &
                   #shape .~ CircleGlyph &
                   #size .~ 0.02 &
                   #color .~ palette1 List.!! 3 &
                   #borderSize .~ 0))
    [PointXY $ ellipse (view #full es) (view #ang0 es)],
    Chart (TextA (defaultTextStyle &
                  #size .~ 0.03 & #color .~ setOpac 0.2 light) [shortPi (view #ang0 es)]) [PointXY (Point 0.1 0.2 + ellipse (view #full es) (view #ang0 es))],
    Chart (GlyphA (defaultGlyphStyle & #shape .~ CircleGlyph & #size .~ 0.02 & #color .~ palette1 List.!! 4 & #borderSize .~ 0)) [PointXY $ ellipse (view #full es) (view #ang1 es)],
    Chart (TextA (defaultTextStyle & #size .~ 0.03 & #color .~ setOpac 0.2 light) [shortPi (view #ang1 es)]) [PointXY (Point 0.1 0.2 + ellipse (view #full es) (view #ang1 es))],

    -- point information
    Chart (TextA (defaultTextStyle & #size .~ 0.03 & #color .~ setOpac 0.2 light) [shortP p0]) [PointXY (Point -0.1 -0.2 + p0)],
    Chart (TextA (defaultTextStyle & #size .~ 0.03 & #color .~ setOpac 0.2 light) [shortP p1]) [PointXY (Point -0.1 -0.2 + p1)],

    -- ellipse segment
    Chart (LineA (defaultLineStyle & #color .~ setOpac 0.2 light))
      (PointXY . ellipse (view #full es) <$> grid OuterPos (Range (view #ang0 es) (view #ang1 es)) 100)
  ])
  where
    p0 = ellipse (view #full es) (view #ang0 es)
    p1 = ellipse (view #full es) (view #ang1 es)

fixC :: Rect Double -> ChartSvg -> ChartSvg
fixC r cs =
  cs &
  #chartList %~ (<> [Chart BlankA [RectXY r]]) &
  (#hudOptions . #hudAxes) %~ fmap (#axisTick %~ #tstyle .~ TickRound (FormatComma (Just 2)) 8 NoTickExtend) &
  (#svgOptions . #outerPad .~ Just 0.2)

run :: (Double -> ChartSvg) -> IO ()
run f =
  reanimate
  (animChartSvg (defaultReanimateConfig & #duration .~ 20)
    (fixC (Rect (-3) 3 (-3) 3) . f))

run1 :: IO ()
run1 = run $
  dThetaEllipseSegment
  (EllipseSegment (Ellipse (Point 0 0) (Point 2 1) zero) zero (pi/2))
