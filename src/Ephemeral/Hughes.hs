{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

module Ephemeral.Hughes where

import Control.Comonad.Cofree
import Data.Functor.Identity
import NumHask.Prelude

-- Cofree Identity
type Free :: (Type -> Type) -> (Type -> Type)
data Free f a = Pure a | Free (f (Free f a))

-- https://www.reddit.com/r/haskell/comments/mf8588/deriving_via_free_and_cofree/
-- Hopes <~> Free Maybe
type Hopes :: Type -> Type
data Hopes a = Confirmed a | Failed | Possible (Hopes a)

-- type Cofree :: (Type -> Type) -> (Type -> Type)
-- data Cofree f a = a :< f (Cofree f a)

limit :: (Ord a, Subtractive a, Absolute a) => a -> Cofree Identity a -> a
limit eps (x0 :< Identity xs0) = go x0 xs0
  where
    go x (x' :< Identity xs) = bool (go x' xs) x' (abs (x - x') < eps)

-- limitf :: (Ord a, Num a) => a -> Cofree Identity a -> a
limitf :: (Ord a, Subtractive a, Absolute a) => a -> Cofree Identity a -> a
limitf eps = fix (\rec (x :< Identity xs@(x' :< _)) -> bool (rec xs) x' (abs (x - x') < eps))

approach :: (Ord a, Subtractive a, Absolute a) => a -> (a -> a) -> a
approach eps f = limit eps (coiter (Identity . f) one)

approachf :: (Ord a, Subtractive a, Absolute a) => a -> (a -> a) -> a
approachf eps f = limitf eps (coiter (Identity . f) one)

sqrootStep :: (Additive a, Divisive a) => a -> a -> a
sqrootStep a x = (x + a / x) / two

sqroot'' :: (Ord a, Absolute a, Field a) => a -> a -> a
sqroot'' eps a = approach eps (sqrootStep a)

sqrootf :: (Ord a, Absolute a, Field a) => a -> a -> a
sqrootf eps a = approachf eps (sqrootStep a)

approach' :: (Ord a, Absolute a, Field a) => a -> (a -> a) -> a
approach' eps f = limit_ eps (iterate f one)

sqroot' :: (Ord a, Absolute a, Field a) => a -> a -> a
sqroot' eps a = approach' eps (sqrootStep a)

limit_ :: (Ord a, Absolute a, Field a) => a -> [a] -> a
limit_ _ [] = error ("empty" :: String)
limit_ eps (x0 : xs0) = go x0 xs0
  where
    go x [] = x
    go x (x' : xs) = bool (go x' xs) x' (abs (x - x') < eps)

limitm :: (Ord a, Absolute a, Field a) => a -> [a] -> a
limitm _ [] = error ("empty" :: String)
limitm eps (x0 : xs0) = go x0 xs0
  where
    go x [] = x
    go x (x' : xs) = bool (go x' xs) x' (abs (x / x' - one) < eps)

-- >>> sqroot 0.0001 2
-- 1.4142135623746899
sqroot :: (Ord a, Absolute a, Field a) => a -> a -> a
sqroot eps a = limit_ eps (iterate next one)
  where
    next x = (x + a / x) / two

-- >>> deriv 0.00001 (\x -> x * x) 3
-- 6.000007629394531
--
-- >>> deriv 0.00001 (sqroot 0.00001) 2
-- 0.35354799598098907
--
deriv :: (Ord a, Absolute a, Field a) => a -> (a -> a) -> a -> a
deriv eps f x =
  limit_ eps (map slope (iterate (/ two) one))
  where
    slope h = (f (x + h) - f x) / h

-- >>> integrate 0.00001 (sqroot 0.00001) 1 2
-- 1.2189450960900354
-- >>> f x = x ** 1.5 / 1.5
-- >>> f 2 - f 1
-- 1.2189450960900354
integrate :: (Ord a, Absolute a, QuotientField a, Whole a ~ Int) => a -> (a -> a) -> a -> a -> a
integrate eps f a b =
  limit_ eps (map area (iterate (/ two) one))
  where
    area h = sum $ (h *) . f <$> take (floor $ (b - a) / h) (iterate (+ h) a)

integrate' :: (Absolute a, QuotientField a, Whole a ~ Int) => (a -> a) -> a -> a -> [a]
integrate' f a b =
  map area (iterate (/ two) one)
  where
    area h = sum $ (h *) . f <$> take (floor $ (b - a) / h) (iterate (+ h) a)

-- | Hughes' improve idea
--
-- A + B * h^n
-- A + B * (h/2)^n
improve :: (Absolute a, Field a) => Int -> [a] -> [a]
improve _ [] = []
improve _ [x] = [x]
improve n (a : b : rest) = (b * two ^ n - a) / (two ^ n - one) : improve n (b : rest)

-- improveall :: (Fractional a, Num t) => t -> (Int -> [a] -> [a]) -> [Int -> [a] -> [a]]
improveall :: (Absolute a, Field a) => Int -> [a] -> [[a]]
improveall n s = s : improveall (n + 1) (improve n s)

-- >>> limit_ 0.00000001 $ super (integrate' sin 0 4)
-- 1.6536436208622662
super :: (Absolute a, Field a) => [a] -> [a]
super s = map head (improveall 1 s)

-- | Newton-Raphson method for finding roots.
newtonRaphson :: Double -> (Double -> Double) -> Double
newtonRaphson guess f
  | difference <= epsilon = newguess
  | otherwise = newtonRaphson newguess f
  where
    newguess = guess - f guess / fprime guess
    difference = abs (newguess - guess)
    fprime = derivative f

newtonRaphson2 :: Double -> (Double -> Double) -> (Double -> Double) -> Double
newtonRaphson2 guess f fprime
  | difference <= epsilon = newguess
  | otherwise = newtonRaphson2 newguess f fprime
  where
    newguess = guess - f guess / fprime guess
    difference = abs (newguess - guess)

-- | Newton's method for finding optimization of functions.
optimize :: (Double -> Double) -> Double -> Double
optimize f guess = newtonRaphson guess g
  where
    g x = derivative2 f x / derivSecond f x

-- | Numerical method for finding the square root.
mysqrt :: Double -> Double -> Double
mysqrt a x
  | difference <= epsilon = newguess
  | otherwise = mysqrt a newguess
  where
    newguess = (1 / 2) * (x + a / x)
    difference = abs (newguess - x)

-- | returns an approximation of the derivative
-- using forward differences.
derivative :: (Double -> Double) -> Double -> Double
derivative f x = (f (x + epsilon) - f x) / epsilon

-- | returns an approximation of the derivative using the symmetric
-- difference quotient.
derivative2 :: (Double -> Double) -> Double -> Double
derivative2 f x = (f (x + epsilon) - f (x - epsilon)) / (2 * epsilon)

-- finite central differences.
derivative3 :: (Double -> Double) -> Double -> Double
derivative3 f x = (d - 8 * c + 8 * b - a) / (12 * epsilon)
  where
    a = f (x + 2 * epsilon)
    b = f (x + epsilon)
    c = f (x - epsilon)
    d = f (x - 2 * epsilon)

-- | returns the second derivative of a function.
derivSecond :: (Double -> Double) -> Double -> Double
derivSecond f x = (f (x + epsilon) - 2 * f x + f (x - epsilon)) / epsilon ** 2

deriv' :: (Double -> Double) -> Double -> Double
deriv' f x = (f (x + dx) - f x) / dx
  where
    dx = epsilon * x

-- root finding

-- f (root f) = 0
-- inverse f y = root (\x -> f x - y)
-- extremum f = root (derivative f)
-- https://hackage.haskell.org/package/math-functions-0.3.4.2/docs/Numeric-RootFinding.html#g:3

-- bisection
-- f a * f b < 0

bisection ::
  (Absolute a, Field a, Ord a, Epsilon a, Field b, Ord b) =>
  (a -> b) ->
  a ->
  a ->
  a
bisection f a b = let av = (b + a) / two in bool (bool (bisection f av b) (bisection f a av) (f a * f av < zero)) av (b - a < epsilon)

newton :: (Epsilon a, Absolute a, Field a, Ord a) => (a -> a) -> (a -> a) -> a -> a
newton f f' guess =
  let newGuess = guess - (f guess / f' guess)
      err = abs (newGuess - guess)
   in bool (newton f f' newGuess) newGuess (err < epsilon)

towardRoot :: (Field a) => (a -> a) -> (a -> a) -> a -> a
towardRoot f f' x = x - f x / f' x

secant :: (Epsilon a, Absolute a, Field a, Ord a) => (a -> a) -> a -> a -> a
secant f guess1 guess0 =
  let newGuess = guess1 - f guess1 * (guess1 - guess0) / (f guess1 - f guess0)
      err = abs (newGuess - guess1)
   in if err < epsilon
        then newGuess
        else secant f newGuess guess1
