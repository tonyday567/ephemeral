{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

{- |

Functional machinery, rapid learning.

-}

module Ephemeral where

import NumHask.Prelude hiding (evaluate)
import Data.Profunctor

{- |

A computer program is said to learn from experience E with respect to some task T and some performance measure P, if its performance on T, as measured by P, improves with experience E. ~ Tom Mitchell

-}

-- | learning is to use experience to change the performance of a task.
newtype Learn f e p =
  Learn { change :: Foldable f => Experience f e -> Task e p -> Task e p }

-- | An experience is an accumulation of e, the carrier of some underlying grammar.
newtype Experience f e = Experience { set :: f e }

-- | A task is a pure function that performance measures an experience singleton.
--
-- Both performance measures and experiences will need to remain flexible.
--
-- Which makes this type a unification point.
newtype Task e p = Task { measure :: e -> p } deriving (Profunctor)

-- | To progress, is to transduce a Task
newtype Progress e p = Progress { step :: e -> Task e p -> Task e p}

-- | to learn, is to make Progress (good, bad or ugly) from an Experience.
learn :: Progress e p -> Learn f e p
learn p = Learn $ \(Experience e) task -> foldr (step p) task e

-- | to improve, given a way to change a task by experience, you need to choose the better performace way over an experience set.
improve ::
  (Ord (f p), Foldable f, Functor f) =>
  Progress e p ->
  Experience f e ->
  Task e p ->
  Task e p
improve progress es task =
  bool task task' (p' > p)
  where
    task' = change (learn progress) es task
    p = measure task <$> set es
    p' = measure task' <$> set es

data LearningType a c =
  Regression {fit :: (Ord a, Num a) => Params a} |
  Classification { enumerate :: (Eq c) => Params c}

{-

If we take a linear regression, with parameters of alpha and betas unified as (a:bs), gives a carrier e of ([a], a). We take an [a] (the dependent variables) and produce an a, our guess as to the independent variable. We then take the second element of the tuple and know it is the underlying true answer to our guess. The difference between our guess and the correct answer is our measure, with the closer to zero the better.

-}

newtype Params a = Params { act :: [a] } deriving (Eq, Show, Generic)

error :: (Ring a) => Params a -> ([a], a) -> a
error (Params bs) (es, y) = sum (zipWith (+) bs (one:es)) - y

errors :: (Functor f, Ring a) => Params a -> Experience f ([a],a) -> f a
errors p (Experience es) = error p <$> es



-- population level

newtype Population f a = Population { individuals :: f (Params a) } deriving (Generic)

-- | The value of a population, for each experience.
-- value :: Population f a -> Experience g ([a], a) -> g a
value :: (Traversable p, Traversable e, Applicative e, Ring a) =>
  Population p a ->
  Experience e ([a], a) ->
  (e (p a) -> p a) ->
  p a
value (Population ps) (Experience es) f = f $ traverse (\p -> error p <$> es) ps

-- | A heuristic is any approach to problem solving or *self-discovery* that employs a practical method, not guaranteed to be optimal, perfect, or rational.
--
-- populations evolve according to heuristics.
--
-- They are natural transformations, with a carrier phenotype.
newtype Heuristic f g a = Heuristic { evolve :: Population f a -> Population g a }

-- cofunctor, like an average
type Neighbourhood = Heuristic Identity

-- mutation is isomorphic to (->).
type Mutation = Heuristic Identity Identity

-- crossover is foldable
type Crossover f = Heuristic f Identity

-- an individual can also be a carrier of the algebra
type Individual a = Population Identity a

