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
    ellipse,
    EllipseConfig(..),
    defaultEllipseConfig,
    run1,
  ) where

import Chart hiding (ellipse)
import Chart.Reanimate
import Control.Lens
import NumHask.Prelude hiding (rotate, Down, next, basis)
import qualified Data.Sequence as Seq
import Ephemeral.Search
import Reanimate as Re hiding (rotate)

-- $setup
-- >>> :set -XOverloadedLabels
-- >>> let cfg = defaultEllipseConfig

-- | ellipse formulae
--
-- >>> ellipse zero (Point 1 2) (pi/6) pi
-- Point -0.8660254037844388 -0.4999999999999997
--
-- Compare this "elegent" definition from 
--
-- \[\dfrac {((x-h)\cos(A)+(y-k)\sin(A))^2}{a^2}+\dfrac{((x-h) \sin(A)-(y-k) \cos(A))^2}{b^2}=1\]
--
-- with the haskell code:
--
-- > c + (rotate phi |. (r * ray theta))
--
-- 
-- \[{\displaystyle {\dfrac {(x\cos \theta -y\sin \theta - c_x)^{2}}{a^{2}}}+{\dfrac {(x\sin \theta +y\cos \theta -c_y)^{2}}{b^{2}}}=1}\]
--
-- See also: [wolfram](https://mathworld.wolfram.com/Ellipse.html)
--
ellipse_ :: (Direction b a, Affinity b a, TrigField a) => b -> a -> b -> a -> b
ellipse_ centroid rot radii theta =
  rotate rot |. (radii * ray theta) + centroid

data Ellipse a =
  Ellipse
  { centroid :: Point a,
    radii :: Point a,
    major :: a
  } deriving (Eq, Show, Generic)

-- | find the point on the ellipse at an angle.
ellipse :: Ellipse a -> a -> Point a
ellipse c theta =
  rotate (view #major c) |. (view #radii c * ray theta) + view #centroid c

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

surfaceP :: ArcPosition Double -> Double -> Point Double
surfaceP p x = ellipse_ (view #centroid c) (view #cphi c) (view #radius c) (view #ang0 c + x * view #angdiff c)
  where
    c = arcCentroid p



fullP :: ArcPosition Double -> Double -> Point Double
fullP p x = ellipse_ (view #centroid c) (view #cphi c) (view #radius c) (zero + x * 2 * pi)
  where
    c = arcCentroid p

arcThetaExample :: Int -> ArcCentroid Double -> Double -> ChartSvg
arcThetaExample n c x =
  mempty &
  #hudOptions .~ colourHudOptions light defaultHudOptions &
  #svgOptions .~ (defaultSvgOptions & #chartAspect .~ CanvasAspect 1) &
  #chartList .~
  [ -- centroid
    Chart (GlyphA (defaultGlyphStyle & #shape .~ CircleGlyph)) [PointXY $ view #centroid c],
    Chart (TextA (defaultTextStyle & #size .~ 0.03 & #color .~ light) [(\(Point x y) -> "(" <> fixed (Just 2) x <> "," <> fixed (Just 2) y <> ")") $ view #centroid c]) [PointXY (Point 0.1 0.1 + view #centroid c)],

    -- pos1
    Chart (GlyphA defaultGlyphStyle) [PointXY $ ellipse c (view #ang0 c)],
    Chart (TextA (defaultTextStyle & #size .~ 0.03 & #color .~ light) [fixed (Just 2) (view #ang0 c / pi) <> "pi"]) [PointXY (Point 0.1 0.1 + ellipse c (view #ang0 c))],
    -- pos2
    Chart (GlyphA defaultGlyphStyle) [PointXY $ ellipse c $
                                      view #ang0 c + view #angdiff c],
    Chart (TextA (defaultTextStyle & #size .~ 0.03 & #color .~ light) [fixed (Just 2) ((view #ang0 c + view #angdiff c)/ pi) <> "pi"]) [PointXY (Point -0.1 0.1 + ellipse c (view #ang0 c + view #angdiff c))],

    -- major axis
    Chart (LineA $ defaultLineStyle & #color .~ setOpac 0.5 light & #width .~ 0.005) [PointXY (view #centroid c), PointXY (ellipse c zero)],
    Chart (TextA (defaultTextStyle & #rotation ?~ view #cphi c & #size .~ 0.03 & #color .~ light) ["major axis"]) [PointXY (Point 0.02 0.1 + 0.5 .* (view #centroid c + ellipse c zero))],

    -- ellipse chord
    Chart (LineA (defaultLineStyle & #color .~ setOpac 0.2 light))
    (PointXY . ellipse c <$> grid'),

    -- theta animation
    Chart (GlyphA (defaultGlyphStyle & #shape .~ CircleGlyph & #borderColor .~ transparent & #borderSize .~ 0 & #color .~ Colour 0.8 0.5 0.1 1 & #size .~ 0.02)) [PointXY $ ellipse c (x * 2 * pi)],
    Chart (TextA (defaultTextStyle & #size .~ 0.03 & #color .~ light) [fixed (Just 2) (x * 2) <> "pi"]) [PointXY (Point 0.1 0.1 + ellipse c (x * 2 * pi))]
  ]
  where
    grid' = grid OuterPos (Range (view #ang0 c) (view #ang0 c + view #angdiff c)) n

run :: IO ()
run =
  reanimate (animChartSvg defaultReanimateConfig (ellipseExample 100 (pi/4) (Point 2 1)))

fixC :: Rect Double -> ChartSvg -> ChartSvg
fixC r cs = cs & #chartList %~ (<> [Chart BlankA [RectXY r]])

arcExamplePhi :: ArcCentroid Double -> Double -> Double -> ChartSvg
arcExamplePhi c xPhi xTheta = arcThetaExample 100 c' xTheta
  where
    c' = c & set #cphi (project (Range zero one) (Range 0 (2*pi)) xPhi)

run1 :: IO ()
run1 =
  reanimate
  (animChartSvg (defaultReanimateConfig & #duration .~ 20)
    (\x -> fixC (Rect (-3) 3 (-3) 3) $
      arcExamplePhi
      (ArcCentroid (Point 0 0) (Point 2 1) zero 0 (3 * pi / 2)) x (4*x)))

animTheta :: Animation
animTheta = animChartSvg defaultReanimateConfig
  (\x -> #svgOptions . #chartAspect .~ CanvasAspect 1 $
    ellipseExample 100 (pi/4) (Point 2 1) x <>
   (mempty & #chartList %~ (<> [Chart (TextA (defaultTextStyle & #color .~ light & #anchor .~ AnchorStart) ["angle: " <> fixed (Just 2) (x*2) <> " pi"]) [P -2.0 2.0]]))
  )

animRot :: Animation
animRot =
  animChartSvg defaultReanimateConfig
  (\x -> ellipseExample 100 (project (Range zero one) (Range 0 (2*pi)) x) (Point 2 1) 0 <>
   (mempty & #chartList %~ (<> [Chart (TextA (defaultTextStyle & #color .~ light & #anchor .~ AnchorStart) ["rotation: " <> fixed (Just 2) (x*2) <> " pi"]) [P -2.0 2.0]])))

animRadii :: Animation
animRadii =
  animChartSvg defaultReanimateConfig
  (\x -> ellipseExample 100 (pi/4) (Point (project (Range zero one) (Range one 2) x) one) zero <>
   (mempty & #chartList %~ (<> [Chart (TextA (defaultTextStyle & #color .~ light & #anchor .~ AnchorStart) ["eccentricity: " <> fixed (Just 2) (1+x)]) [P -2.0 2.0]])))

-- | animates across the surface of the ellipse segment
animPos :: ArcCentroid Double -> Animation
animPos c = animChartSvg defaultReanimateConfig (arcThetaExample 100 c)

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
            . ellipse_ c rot r
            . (\x -> 2 * pi * fromIntegral x / fromIntegral n)
            <$> [0 .. n]
        )
        (ellipse_ c' rot' r' . (\x -> 2 * pi * fromIntegral x / fromIntegral n) <$> [0 .. n])
  where
    n = cfg ^. #numPoints
    (ArcCentroid c r rot _ _) = view #arc0 cfg
    (ArcCentroid c' r' rot' _ _) = view #guess cfg

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


{-
-- | computes the difference between the projected ellipse and a guess
--
-- >>> ellipseFit defaultEllipseConfig
-- 104.27998932156083
ellipseFit :: EllipseConfig -> Double
ellipseFit cfg c =
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
    (ArcCentroid c r phi _ _) = cfg ^. #arc0
    (ArcCentroid c' r' phi' _ _) = g

-- |
defaultEllipseConfig :: EllipseConfig
defaultEllipseConfig = EllipseConfig 100 ac0 (aspect 2)

ac0 :: ArcCentroid Double
ac0 = ArcCentroid (Point -0.2864867185179476 1.6092991486979669) (Point 1.3866801807145205 2.8942082605509395) 1.0962340928888052 -2.8 -5.348100265785992

acZero :: ArcCentroid Double
acZero = ArcCentroid (Point 0 0) (Point 1 1) 0 0 0

data UpTo = UpTo { config :: ProjectionConfig, best :: (ArcCentroid Double, Double), log :: [Text], count :: Int } deriving (Eq, Show, Generic)

upto :: ProjectionConfig -> UpTo
upto cfg = UpTo cfg (g, fit cfg g) ["start"] 0
  where
    g = view #pop cfg

-- | computes the difference between the projected ellipse and a guess
--
-- >>> fit defaultProjectionConfig (fst $ view #best u)
-- 104.27998932156083
fit :: ProjectionConfig -> ArcCentroid Double -> Double
fit cfg g =
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
    (ArcCentroid c r phi _ _) = cfg ^. #arc0
    (ArcCentroid c' r' phi' _ _) = g

-- | The mutation basis for an ArcCentroid
acBasis :: NonEmpty (Double -> ArcCentroid Double -> ArcCentroid Double)
acBasis =
  N.fromList
    [ \s x -> x & #radius %~ (+ Point s 0),
      \s x -> x & #radius %~ (+ Point 0 s),
      \s x -> x & #cphi %~ (+ s)
    ]

-- | return the values of all possible jumps, given a candidate
--
-- >>> allBasis cfg (fst $ view #best u)
-- 7.105427357601002e-14 :| [1.1368683772161603e-13,1.8758328224066645e-12]
allBasis :: ProjectionConfig -> ArcCentroid Double -> NonEmpty Double
allBasis cfg g =
  (+ negate (fit cfg g)) .
  fit cfg .
  (\f -> f (view #lim cfg) g) <$>
  acBasis

-- | pick the next candidate
--
-- Returns the jump index and direction
--
-- >>> flip evalState u next 
-- (2,Down)
next :: State UpTo (Int, Dir)
next = do
  cfg <- config <$> get
  g <- fst . best <$> get
  pure $
    second (bool Up Down . (>0)) $
    maximumBy (comparing (abs . snd))
    (N.zip (N.iterate (+ 1) 0) (allBasis cfg g))

-- | find the best marginal basis jump and bracket this.
--
-- >>> flip evalState u nextRange
-- Just (2,(-0.9999999999999998,-1.0e-14))
nextRange :: State UpTo (Maybe (Int, (Double, Double)))
nextRange = do
  cfg <- config <$> get
  g <- fst . best <$> get
  (i, dir') <- next
  let d = view #lim cfg
  let bump = bool (-d) d (dir'==Up)
  let b = findBracket bump 10 1e12 (negate . delta1 cfg g (acBasis N.!! i))
  pure $
    bool (Just (i, (b, -epsilon))) (Just (i, (epsilon, b))) (b > 0)

-- | test the marginal change in the function (gradient) given a candidate
--
delta1 :: ProjectionConfig -> ArcCentroid Double -> (Double -> ArcCentroid Double -> ArcCentroid Double) -> Double -> Double
delta1 cfg g f x = fit cfg (f x g) - fit cfg (f zero g)

-- |
-- `findBracket 1 10 1e12 f` finds the 
findBracket :: Double -> Double -> Double -> (Double -> Double) -> Double
findBracket x0 xp xm f = go x0
  where
    go x = bool (bool (go (x * xp)) x (f x <= 0)) 0.001 (x >= xm)

-- | Search for the next guess by finding the root of the minimisation equation using the best jump candidate
--
-- >>> fst $ flip runState u findr
-- Right (2,ArcCentroid {centroid = Point -0.2864867185179476 1.6092991486979669, radius = Point 1.3866801807145206 2.8942082605509394, cphi = 0.29581201292739345, ang0 = -2.8, angdiff = -5.348100265785992})
findr :: State UpTo (Either Text (Int, ArcCentroid Double))
findr = do
  cfg <- config <$> get
  g <- fst . best <$> get
  nr <- fmap (\(i, bs) -> (i, root bs (delta1 cfg g (acBasis N.!! i)))) <$> nextRange
  pure $ case nr of
    (Just (_, NotBracketed)) -> Left "NotBracketed"
    Just (_, SearchFailed) -> Left "SearchFailed"
    Just (i', Root a) -> Right (i', (acBasis N.!! i') a g)
    Nothing -> Left "brackets not found"

-- | root finder
--
-- >>> (Just (_,bs)) = flip evalState u nextRange
-- >>> root bs (delta1 defaultProjectionConfig (fst $ view #best u) (acBasis N.!! 2))
-- Root (-0.8004220799614118)
root :: (Double, Double) -> (Double -> Double) -> Root Double
root = ridders (RiddersParam 100 (RelTol $ epsilon * 1.0))


-- | Search for better guesses
--
-- > search defaultProjectionConfig
--
-- TODO: polymorphise ArcCentroid
search :: ProjectionConfig -> IO ()
search cfg = join $ pPrintNoColor <$> (flip execStateT (upto cfg) (loop pPrintNoColor))

loop :: (UpTo -> IO ()) -> StateT UpTo IO ()
loop l = do
  check <- hoist generalize step
  bool (do
           u <- get
           liftIO (l u)
           loop l)
    (pure ()) (check==Stop)

data Check = Continue | Stop deriving (Eq, Show)

step :: StateT UpTo Identity Check
step = do
  cfg <- use #config
  (_, f') <- use #best
  c <- use #count
  mi <- use (#config . #maxIterations)
  tol <- use (#config . #tol)
  bool
    (do
        x <- findr
        case x of
          Left e -> #log %= (<> ["error: " <> e]) >> pure Stop
          Right (_, g) -> do
            let f = fit cfg g
            #log %= (<> ["f: " <> show f])
            bool
              (do
                  #best .= (g,f)
                  #log %= (<> ["step: " <> show c <> ":" <> show (g,f)])
                  pure Continue)
              (#log %= (<> ["tolerance reached"]) >> pure Stop)
              (f' - f < tol))
    (#log %= (<> ["maximum iterations reached"]) >> pure Stop)
    (c >= mi)

-- | TODO: check this
chartGuess :: ProjectionConfig -> ArcCentroid Double -> IO ()
chartGuess cfg g = do
  let (ArcCentroid c r phi ang0 angd) = cfg ^. #arc0
  let (ArcCentroid c' r' phi' ang0' angd') = g
  let n = cfg ^. #numPoints
  let nd = fromIntegral n
  let ellGuess = Chart (LineA $ defaultLineStyle & #width .~ 0.03 & #color .~ Colour 1 0 0.3 0.3) (PointXY . ellipse c' r' phi' . (\x -> ang0' + angd' * fromIntegral x / nd) <$> [0 .. n])
  let ell = Chart (LineA $ defaultLineStyle & #width .~ 0.04 & #color .~ Colour 0.7 0.2 0.8 0.3) (PointXY . ellipse c r phi . (\x -> ang0 + angd * fromIntegral x / nd) <$> [0 .. n])
  writeChartSvgDefault "other/g1.svg" (projectXYsWith (cfg ^. #projection) one [ell] <> [ellGuess])

main :: IO ()
main = pure ()

{- TODO: Why doesn't this work?
-- | ellipse radii formulae
--
-- p = c + rotate phi (r * ray theta)
-- rotate (-phi) (p - c) / ray theta = r
ellipseR' :: (Direction b a, Affinity b a, Field b, TrigField a) => b -> b -> a -> a -> b
ellipseR' p c phi theta = (rotate (-phi) |. (p - c)) / ray theta

-}

-- FIXME: this works:
-- >>> search $ defaultProjectionConfig & set #pop g3
g3 :: ArcCentroid Double
g3 = ArcCentroid {centroid = Point -0.2864867185179476 1.6092991486979669, radius = Point 1.3858025965450367 2.8858375379037695, cphi = 1.097568874016416, ang0 = -2.792315785988488, angdiff = -5.348100265785992}


-}
