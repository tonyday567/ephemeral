{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

module Ephemeral.Search where

import Chart
-- import Control.Lens
import NumHask.Prelude hiding (rotate, Down, next)
-- import Numeric.RootFinding
import qualified Data.Sequence as Seq
import NumHask.Array.Fixed
import Data.Functor.Rep

data Dir = Up | Down deriving (Eq, Show)

data Stops a =
  MaxIterations Int |
  Converged a |
  NotConverging
  deriving (Eq, Show, Generic)

data (KnownNat n) => Problem n config a = Problem {
    field :: Array '[n] (Range Double),
    fit :: Array '[n] Double -> a,
    search :: config
    }

newtype SearchConfig a =
  SearchConfig
  { stops :: Seq (Stops a)
  } deriving (Generic, Eq, Show)

defaultSearchConfig :: a -> SearchConfig a
defaultSearchConfig eps =
  SearchConfig (Seq.fromList [MaxIterations 100, Converged eps, NotConverging])

-- problem :: b -> (Seq a -> b) -> (b -> Double) -> Range (Seq a) -> Problem a b
-- problem cfg make fit r = fit <$> max r <*> min r <*> (bool 1 -1 . (==Up)) <$> [Up, Down]


{-
-- | pick the next candidate
--
-- Returns the jump index and direction
--
-- >>> flip evalState u next 
-- (2,Down)
next :: State (Exp Identity Double Double) (Int, Dir)
next = undefined {-do
  cfg <- config <$> get
  g <- fst . best <$> get
  pure $
    second (bool Up Down . (>0)) $
    maximumBy (comparing (abs . snd))
    (N.zip (N.iterate (+ 1) 0) (allBasis cfg g))--}

-- | find the best marginal basis jump and bracket this.
--
-- >>> flip evalState u nextRange
-- Just (2,(-0.9999999999999998,-1.0e-14))
nextRange :: State (Exp Identity Double) (Maybe (Int, (Double, Double)))
nextRange = undefined {-
do
  cfg <- config <$> get
  g <- fst . best <$> get
  (i, dir') <- next
  let d = view #lim cfg
  let bump = bool (-d) d (dir'==Up)
  let b = findBracket bump 10 1e12 (negate . delta1 cfg g (acBasis N.!! i))
  pure $
    bool (Just (i, (b, -epsilon))) (Just (i, (epsilon, b))) (b > 0)

-}
-- | test the marginal change in the function (gradient) given a candidate
--
delta1 :: Seq Double -> a -> Double
delta1 cfg x = undefined -- f b x - f x (b Seq.empty)

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
findr :: State (Exp Identity Double) (Either Text a)
findr = undefined
{-do
  cfg <- config <$> get
  g <- fst . best <$> get
  nr <- fmap (\(i, bs) -> (i, root bs (delta1 cfg g (acBasis N.!! i)))) <$> nextRange
  pure $ case nr of
    (Just (_, NotBracketed)) -> Left "NotBracketed"
    Just (_, SearchFailed) -> Left "SearchFailed"
    Just (i', Root a) -> Right (i', (acBasis N.!! i') a g)
    Nothing -> Left "brackets not found"
-}

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
-- search :: c -> IO ()
-- search cfg = pPrintNoColor =<< execStateT (loop pPrintNoColor) (upto cfg)

loop :: (Exp Identity Double -> IO ()) -> StateT (Exp Identity Double) IO ()
loop l = do
  check <- hoist generalize step
  bool (do
           u <- get
           liftIO (l u)
           loop l)
    (pure ()) (check==Stop)

data Check = Continue | Stop deriving (Eq, Show)

step :: StateT (Exp Identity Double) Identity Check
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

-}
