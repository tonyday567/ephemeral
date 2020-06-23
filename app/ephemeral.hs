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

import Ephemeral.Funks
import Ephemeral.Heuristic
import Ephemeral.Learning
import NumHask.Prelude
import Numeric.Backprop

cost :: (Distributive a, Subtractive a, Traversable f) => Hypothesis [] a -> f ([a], a) -> a
cost h@(Hypothesis Linear _ _) es = sum $ fmap ((^ (2 :: Int)) . (\(xs, y) -> guess h xs - y)) es

type Model p a b = p -> a -> b

type ModelG p a b =
  forall z.
  Reifies z W =>
  BVar z p ->
  BVar z a ->
  BVar z b

{-
linReg :: ModelG [Double] [Double] Double
linReg bs xs = sum (zipWith (+) (sequenceVar bs) ((auto 1) : sequenceVar xs))

sqerr :: (Backprop p, Backprop b, Num b) => ModelG p a b -> a -> b -> p -> p
sqerr f x targ = gradBP $ \p -> (f p (auto x) - auto targ) ^ (2 :: Int)

sgd :: (Backprop b, Backprop p, Num p, Num b) => p -> ModelG p a b -> p -> [(a, b)] -> p
sgd r f = foldl' $ \p (x, y) -> p - r * sqerr f x y p

sqerrs :: (Backprop p, Backprop b, Num b) => ModelG p a b -> [(a, b)] -> p -> p
sqerrs f xs = gradBP $ \p -> sum $ (\(x, targ) -> (f p (auto x) - auto targ) ^ (2 :: Int)) <$> xs

batchgd :: (Backprop b, Backprop p, Num p, Num b) => p -> ModelG p a b -> p -> [(a, b)] -> p
batchgd r f = foldl' $ \p (x, y) -> p - r * sqerr f x y p

-}

main :: IO ()
main = do
  let n = 10
  let answer = product [1 .. n :: Integer]
  print answer
