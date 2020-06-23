{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-

A computer program is said to learn from experience E with respect to some task T and some performance measure P, if its performance on T, as measured by P, improves with experience E. ~ Tom Mitchell

-}
module Ephemeral.Learning where

import Ephemeral.Funks
import Ephemeral.Heuristic
import NumHask.Prelude
import Numeric.Backprop

newtype Experience f e = Experience {unexperience :: (Foldable f) => f e}

newtype Performance a = Performance {unperformance :: a} deriving (Eq, Ord)

newtype Task e a = Task {untask :: e -> Performance a}

test :: e -> Task e a -> Performance a
test e (Task t) = t e

newtype Learn f e a =
  Learn { unlearn :: (Foldable f) => Experience f e -> Task e a -> Task e a }

newtype Step e a = Step {unstep :: e -> Task e a -> Task e a}

learn :: Step e a -> Learn f e a
learn (Step s) = Learn $ \(Experience e) task -> foldr s task e

isGoodLearn ::
  (Ord a, Foldable f) =>
  Step e a ->
  Experience f e ->
  Task e a ->
  e ->
  Bool
isGoodLearn s es task0 e0 =
  test e0 task' > test e0 task0
  where
    (Learn l) = learn s
    task' = l es task0

data LearningType = Supervised | Unsupervised | Reinforcement | RecommenderSystems

{-
For Supervised Learning, Experience is organized as Features and the Result, and called a Training Set.

-}

type TrainingSet f e a = Experience f (e, a)

newtype Features a fs = Features {unfeatures :: a -> fs}

data PredictionType a c =
  Regression {unregress :: (Ord a, Num a) => a} |
  Classification {unclassify :: (Eq c) => c}

data HypothesisType = Linear

data Hypothesis f a =
  Hypothesis
  { hypothesisType :: HypothesisType,
    alpha :: a,
    betas :: f a
  }

guess :: (Additive a) => Hypothesis [] a -> [a] -> a
guess (Hypothesis Linear a bs) xs = a + sum (zipWith (+) bs xs)

cost :: (Distributive a, Subtractive a, Traversable f) => Hypothesis [] a -> f ([a], a) -> a
cost h@(Hypothesis Linear _ _) es = sum $ fmap ((^ (2 :: Int)) . (\(xs, y) -> guess h xs - y)) es
