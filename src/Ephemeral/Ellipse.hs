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
    centroid_,
    Ellipse(..),
    ellipse,
    EllipseSegment(..),
    pos0,
    pos1,
    run,
    run1,
    shortPi,
    shortP,
    shortE,
    shortES,
  ) where

import Chart hiding (rotate, ellipse)
import Chart.Reanimate
import Control.Lens
import NumHask.Prelude hiding (rotate, Down, next)
import Reanimate as Re hiding (rotate)

-- \[\dfrac {((x-h)\cos(A)+(y-k)\sin(A))^2}{a^2}+\dfrac{((x-h) \sin(A)-(y-k) \cos(A))^2}{b^2}=1\]
--
-- \[{\displaystyle {\dfrac {(x\cos \theta -y\sin \theta - c_x)^{2}}{a^{2}}}+{\dfrac {(x\sin \theta +y\cos \theta -c_y)^{2}}{b^{2}}}=1}\]

-- | FIXME: transfer to numhask-space
-- > rotate a p = norm p .* ray (a + angle p)
rotate :: (MultiplicativeAction b a, Norm b a, Direction b a) => a -> b -> b
rotate a b = norm b .* ray (a + angle b)

-- $setup
-- >>> :set -XOverloadedLabels
-- >>> let es = EllipseSegment (Ellipse (Point 1 1) (Point 2 1) 0.1) 0.2 1.0

-- | Find the point on the ellipse circumference at the supplied angle
--
-- >>> ellipse (Point 1 1) (Point 2 1) 0.1 0.2
-- Point 2.930506816327422 1.3933636016685953
--
ellipse_ :: (MultiplicativeAction b a, Norm b a, Direction b a) => b -> a -> b -> a -> b
ellipse_ centroid major radii theta =
  rotate major (radii * ray theta) + centroid

-- | find the centroid of an ellipse given a point on the circumference.
--
-- >>> centroid_ 0.1 (Point 2 1) 0.2 (Point 2.930506816327422 1.3933636016685953)
-- Point 1.0 1.0
centroid_ :: (Subtractive b, MultiplicativeAction b a, Norm b a, Direction b a) => a -> b -> a -> b -> b
centroid_ major radii theta pos = pos - rotate major (radii * ray theta)

-- | find the radii of an ellipse given a point on the circumference.
--
-- >>> radii_ (Point 1 1) 0.1 0.2 (Point 2.930506816327422 1.3933636016685953)
-- Point 2.0 1.0
radii_ :: (Subtractive a, Subtractive b, MultiplicativeAction b a, Norm b a, Direction b a, Divisive b) => b -> a -> a -> b -> b
radii_ centroid major theta pos = pos & (\x -> x - centroid) & rotate (-major) & (/ray theta)

-- | find the major axis rotation of an ellipse given a point on the circumference.
--
-- >>> major_ (Point 1 1) (Point 2 1) 0.2 (Point 2.930506816327422 1.3933636016685953)
-- 0.10000000000000002
major_ :: (Subtractive a, Subtractive b, Direction b a) => b -> b -> a -> b -> a
major_ centroid radii theta pos = pos & (\x -> x - centroid) & (\x -> angle x - angle (radii * ray theta))

-- | find the angle of a point on the circumference for a a rotated ellipse.
--
-- >>> theta_ (Point 1 1) 0.1 (Point 2 1) (Point 2.930506816327422 1.3933636016685953)
-- 0.19999999999999998
theta_ :: (Divisive b, Subtractive b, Subtractive a, MultiplicativeAction b a, Norm b a, Direction b a) => b -> a -> b -> b -> a
theta_ centroid major radii pos =
  pos &
  (\x -> x - centroid) &
  rotate (-major) &
  (/radii) &
  angle

-- >>> let es = EllipseSegment (Ellipse (Point 1 1) (Point 2 1) 0.1) 0.2 2.0
-- >>> centroid_ False es
-- Point 1.0 1.0
--
findCentroid :: Bool -> EllipseSegment Double -> Point Double
findCentroid up es = c
  where
    p0' = pos0 es & rotateP (-m) & (/r)
    p1' = pos1 es & rotateP (-m) & (/r)
    p' = (p0' + p1') /. 2
    mag = sqrt (1 - (norm (p1' - p0') / 2)^2)
    dir' = angle (p1' - p0') + bool (-pi/2) (pi/2) up
    c' = p' + mag .* ray dir'
    c = c' * r & rotateP m
    m = view (#full . #major) es
    r = view (#full . #radii) es

data Ellipse a =
  Ellipse
  { centroid :: Point a,
    radii :: Point a,
    major :: a
  } deriving (Eq, Show, Generic)

shortD :: Double -> Text
shortD = fixed (Just 2)

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
ellipse :: Ellipse Double -> Double -> Point Double
ellipse e = ellipse_ (view #centroid e) (view #major e) (view #radii e)

-- | find the angle of a point to an ellipse
angleE :: Ellipse Double -> Point Double -> Double
angleE e = theta_ (view #centroid e) (view #major e) (view #radii e)

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

pos0 :: EllipseSegment Double -> Point Double
pos0 es = ellipse (view #full es) (view #ang0 es)

pos1 :: EllipseSegment Double -> Point Double
pos1 es = ellipse (view #full es) (view #ang1 es)

-- > rotate a p = norm p .* ray (a + angle p)

-- rotate major' (radii' * ray ang0) + centroid = pos0
-- (radii' * ray ang0) = rotate (-major') (pos0 - centroid)

-- a = norm a .* ray (angle a)
--
-- norm (radii * ray ang0) .* ray (angle (radii' * ray ang0) + major') = pos0 - centroid

-- rotate major' (radii' * ray ang0) = pos0 - centroid
-- rotate major' (radii' * ray ang1) = pos1 - centroid
-- radii' = rotate (-major') (pos0 - centroid) / (ray ang0)
-- radii' = norm (pos0 - centroid) .* ray ((-major) - ang0)
-- rotate major' (norm (pos0 - centroid) .* ray ((-major) - ang0) * ray ang0)
-- norm (pos0 - centroid) .* rotate major' (ray ((-major) - ang0) * ray ang0)
-- norm (pos0 - centroid) .* rotate major' (ray (-major)) = pos0 - centroid
-- radii_ c' m ang1' pos1' = r
-- major_ c' r ang0' pos0' = m
-- major_ c' r ang1' pos1' = m
-- pos0' = + c'
scaleES :: Point Double -> EllipseSegment Double -> EllipseSegment Double
scaleES s es = EllipseSegment (Ellipse c' r' m') ang0' ang1'
  where
    pos0' = s * pos0 es
    pos1' = s * pos1 es
    c' = s * view (#full . #centroid) es
    r' = s * view (#full . #radii) es
    ang0' = angle (pos0' - c') - m'
    ang1' = angle (pos1' - c') - m'
    m' = 0
    r' = rotateP (view (#full . #major) es) (pos0' - c')

-- chart helpers
addTitle :: Text -> Colour -> Double -> ChartSvg -> ChartSvg
addTitle t c s =
  #hudOptions . #hudTitles %~ (\ts -> ts <>
                                [defaultTitle t &
                                  #style . #size .~ s &
                                  #style . #color .~ c &
                                  #place .~ PlaceBottom])

cPoint :: Colour -> Double -> Point Double -> [Chart Double]
cPoint cGlyph s p =
  [ Chart (GlyphA (defaultGlyphStyle &
                   #shape .~ CircleGlyph &
                    #borderSize .~ 0 &
                    #color .~ cGlyph &
                    #size .~ s))
    [PointXY p]
  ]

cLabel :: Colour -> Double -> Double -> Point Double -> Text -> Point Double -> [Chart Double]
cLabel c s rot gap label p =
    [Chart (TextA (defaultTextStyle &
                  #size .~ s &
                  #rotation ?~ rot &
                  #color .~ c)
            [label])
    [PointXY (rotateP rot gap + p)]]

labelledLine :: Colour -> Double -> Colour -> Double -> Point Double -> Text -> Point Double -> Point Double -> [Chart Double]
labelledLine cLine sLine cText sText textGap label p0 p1 =
    [Chart (LineA $ defaultLineStyle &
           #color .~ cLine &
           #width .~ sLine)
    [PointXY p0, PointXY p1],
    Chart (TextA (defaultTextStyle &
                  #rotation ?~ a &
                  #size .~ sText &
                  #color .~ cText)
            [label])
    [PointXY $ rotateP a textGap + (0.5 .* (p0 + p1))]]
    where
      a = angle (p1 - p0)

thetaAnimation :: Colour -> Double -> Colour -> Double -> Point Double -> Ellipse Double -> Double -> [Chart Double]
thetaAnimation c s cl sl gap e x =
  cPoint c s (ellipse e x') <>
  cLabel cl sl rot gap (shortPi x') (ellipse e x')
  where
    x' = 2 * pi * x
    rot = (-pi/2) + view #major e + x'

dThetaEllipse :: Ellipse Double -> Double -> ChartSvg
dThetaEllipse e x =
  mempty &
  #hudOptions .~ colourHudOptions c defaultHudOptions &
  (#chartList .~
  -- centroid
  cPoint (setOpac 0.5 $ palette1 3) 0.04 (view #centroid e) <>
  cLabel c 0.03 zero (Point 0 0.2) (shortP $ view #centroid e) (view #centroid e) <>
  -- major
  labelledLine (setOpac 0.5 $ palette1 1) 0.005 c 0.03 (Point 0 0.1) (shortPi (view #major e)) (view #centroid e) (ellipse e zero) <>
  ellipseSeg 100 (setOpac 0.5 $ palette1 2) (EllipseSegment e zero (2*pi)) <>
  thetaAnimation c 0.02 c 0.03 (Point 0 0.2) e x) &
  addTitle (shortE e) c 0.03
  where
    c = setOpac 0.5 light

ellipseSeg :: Int -> Colour -> EllipseSegment Double -> [Chart Double]
ellipseSeg n c es =
    [Chart (LineA (defaultLineStyle & #color .~ c))
    (PointXY . ellipse (view #full es) <$> grid OuterPos (Range (view #ang0 es) (view #ang1 es)) n)]

segAnimation :: Colour -> Double -> Colour -> Double -> Point Double -> EllipseSegment Double -> Double -> [Chart Double]
segAnimation c s cl sl gap es x =
  cPoint c s (ellipse (view #full es) x') <>
  cLabel cl sl rot gap (shortPi x') (ellipse (view #full es) x')
  where
    x' = view #ang0 es + x * (view #ang1 es - view #ang0 es)
    rot = (-pi/2) + view (#full . #major) es + x'

dThetaEllipseSegment :: EllipseSegment Double -> Double -> ChartSvg
dThetaEllipseSegment es x =
  mempty &
  #hudOptions .~ colourHudOptions c defaultHudOptions &
  ( #chartList .~
    -- centroid
    cPoint (setOpac 0.5 $ palette1 3) 0.02 (view (#full . #centroid) es) <>
    cLabel c 0.02 zero (Point 0 0.2) (shortP $ view (#full . #centroid) es) (view (#full . #centroid) es) <>
    -- major
    labelledLine (setOpac 0.5 $ palette1 1) 0.005 c 0.02 (Point 0 0.1) (shortPi (view (#full . #major) es)) (view (#full . #centroid) es) (ellipse (view #full es) zero) <>
    -- theta
    segAnimation c 0.02 c 0.02 (Point 0 0.2) es x <>
    -- segment
    ellipseSeg 100 (setOpac 0.5 $ palette1 2) es <>
    ellipseSeg 100 (setOpac 0.05 $ palette1 2) (EllipseSegment (view #full es) zero (2*pi)) <>
    -- pos0
    cPoint (palette1 6) 0.02 p0 <>
    cLabel c 0.02 zero (Point 0 0.4) (shortP p0) p0 <>
    cLabel c 0.02 zero (Point 0 0.2) (shortPi (view #ang0 es)) p0 <>
    -- pos1
    cPoint (palette1 7) 0.02 p1 <>
    cLabel c 0.02 zero (Point 0 0.4) (shortP p1) p1 <>
    cLabel c 0.02 zero (Point 0 0.2) (shortPi (view #ang1 es)) p1) &
  addTitle (shortES es) c 0.03
  where
    c = setOpac 0.5 light
    p0 = ellipse (view #full es) (view #ang0 es)
    p1 = ellipse (view #full es) (view #ang1 es)

dBisectEllipseSegment :: EllipseSegment Double -> ChartSvg
dBisectEllipseSegment es =
  mempty &
  #chartList .~
    -- bisection point
    cPoint (palette1 1) 0.02 p' <>
    cLabel light 0.02 zero (Point 0 0.2) (shortP p') p' <>
    -- line to centroid
    labelledLine
      (setOpac 0.5 $ palette1 9) 0.005 (setOpac 0.5 light) 0.02 (Point 0 0.1) "bisector"
      p'
      c
  where
    p0 = ellipse (view #full es) (view #ang0 es)
    p1 = ellipse (view #full es) (view #ang1 es)
    p' = (p1+p0) /. 2
    c = findCentroid True es

-- | bisection method to find centroid
--
-- >>> (EllipseSegment (Ellipse (Point 1 1) (Point 2 1) (pi/4)) (pi/2) (3*pi/2))
dBisect :: EllipseSegment Double -> Double -> ChartSvg
dBisect es x =
  dThetaEllipseSegment es x <>
  dBisectEllipseSegment es

fixC :: Rect Double -> ChartSvg -> ChartSvg
fixC r cs =
  cs &
  #svgOptions . #chartAspect .~ CanvasAspect 1 &
  #chartList %~ (<> [Chart BlankA [RectXY r]]) &
  (#hudOptions . #hudAxes) %~ fmap (#axisTick %~ #tstyle .~ TickRound (FormatComma (Just 2)) 6 NoTickExtend) &
  (#svgOptions . #outerPad ?~ 0.2)

run :: (Double -> ChartSvg) -> IO ()
run f =
  reanimate
  (animChartSvg defaultReanimateConfig
    (fixC (Rect (-3) 3 (-3) 3) . f))

run1 :: IO ()
run1 = run $
  dThetaEllipseSegment
  (EllipseSegment (Ellipse (Point 0 0) (Point 2 1) zero) zero (pi/2))
