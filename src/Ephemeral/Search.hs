{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

module Ephemeral.Search where

import Prelude
import Data.Profunctor
import GHC.Generics
import Data.Functor.Identity
import Data.Bool

{- |
A computer program is said to learn from experience E with respect to some task T and some performance measure P, if its performance on T, as measured by P, improves with experience E. ~ Tom Mitchell
-}

{-
I initial wrote this up as I was working through basic Machine Learning concepts.

The end result seems to be an API focused on `composting`: on a mechanical process of reducing old, old data in massive quantities, to permanent statistic regularities that may explain the behaviour of living computations.

So I'm sitting with it, contemplating how to abstract that out, and find another API.

-}

-- | learning is to use experience to change the performance of a task.
newtype Learn f e p =
  Learn { change :: Foldable f => Experience f e -> Task e p -> Task e p }

-- | An experience is an accumulation of e, the carrier of some underlying grammar.
--
newtype Experience f e = Experience { set :: f e }

-- | A task is a pure function that performance measures an experience singleton.
--
-- Both performance measures and experiences will need to remain flexible.
--
-- TODO: How would this hook into the nucleus of a profunctor memes?
--
newtype Task e p = Task { measure :: e -> p } deriving (Profunctor)

-- | To progress, is to transduce a Task
newtype Progress e p = Progress { step :: e -> Task e p -> Task e p}

-- | population
newtype Population f a = Population { individuals :: f a } deriving (Generic)

-- | A heuristic is any approach to problem solving or *self-discovery* that employs a practical method, not guaranteed to be optimal, perfect, or rational.
--
-- populations evolve according to heuristics.
--
-- They are natural transformations, with a carrier phenotype.
--
newtype Heuristic f g a = Heuristic { evolve :: Population f a -> Population g a }

-- | cofunctor, like an average
--
-- isomorphic to a coalgebra (a -> g a)
type Neighbourhood = Heuristic Identity

-- | mutation is isomorphic to (->).
--
type Mutation = Heuristic Identity Identity

-- | foldable/an algebra
--
-- f a -> a
type Crossover f = Heuristic f Identity

-- | an individual can also be a carrier of the algebra
--
type Individual a = Population Identity a

-- | to learn, is to make Progress (good, bad or ugly) from an Experience.
--
-- In an online setting, processing streamed data, say, this is an update of the state of a Moore machine.
--
-- It doesn't allow for Mealy machine networks, where the Mealy's can fire independently of their inputs.
learn :: Progress e p -> Learn f e p
learn p = Learn $ \(Experience e) task -> foldr (step p) task e

-- | to improve, given a way to change a task by experience, you need to choose the better way over an experience set.
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

{- * sub-component as topologist

The whole problem with this setup is the `(>)` on the measured fitness value. It introduces an a priori: that the only thing worth knowing is which way is up, and that the only way forward is to proceed to the summit. There may be some trickiness needed to jump over local hills and find the ultimate high spot, but we will get there faster or more reliably than anyone else can. Finally, it tightly couples progress with computing the next value.

If we asked our sub-components, such as improve, to instead report on the topology and statistics of the space, then the usual learning processes (under the hood of every machine learning algorithm ever built) is actually a degenerate version, providing a summary consisting of the minimum or maximum value and where it's located.

IN this refactor, `next` would never know, as a sub-component, how many times it will be called. The first few suggestions might be random or grid-spaced if it's well-bounded. The next few would certainly try to extrapolate the results and search the highest and lowest gradient paths to see the local peaks and troughs. What then?

The AI solution, where a more topological approach is used, usually advocates a "shared fitness" approach where fitness for an individual is computed by also taking into account neighbouring results. K-means clustering is the iconic approach. With this reorientation, this appraoch can be seen as a clunky way to compensate for the API focus on the best individual and subsequent hinge point created for progress to use fitness as the sole criteria.

The shared fitness approach includes a rule of thumb that parameter space sections consisting of broad, high tableland shapes should be preferentially explored. This may be a natural outcome of this refactor.

An interesting design is where next is not supplied with any notion of distance for individual parameter elements and must make it's way by supplying it's own empirical notion of parameter distance. In this way, the statistics that it uses are topological as well. Associated to this is for next to form it's own ideas about the value of K in K-Means.

Bulk processing of populations using this structure seems more about the heuristic shortcuts in peoples minds than any formal feature of the problem domain. The API is simplified by generating just one suggestion at a time, and allowing bulk efficiencies to be explored above upstream.

-}

data Topology a p

-- | generate the next parameter choice to be tested.
next :: Topology a p -> a
next = undefined

-- | update the space summary
--
-- Is Topology really Task in disguise but a higher-kinded layer up?
-- newtype Progress e p = Progress { step :: e -> Task e p -> Task e p}
--
-- Actual computation of the next suggestion is not guaranteed, so the fitness, p, is a Maybe to allow for the potential for delays in feeding the result of an individual test back, or if there is another process that rejects the suggestion.
--
-- as an alternative there could be a direct conversion to Topology, with Topology being monoidal.
-- toTop :: (a, Maybe p) -> Topology a p
--
-- so update becomes `(<>) . toTop`
--
update :: (a, Maybe p) -> Topology a p -> Topology a p
update = undefined

-- also useful would be to back out a point from the topology, if, for example, you were replacing a previous suggestion, with one that now included a fitness value.
delete :: (a, Maybe p) -> Topology a p -> Topology a p
delete = undefined

-- Evaluation of good search to a single value potentially recovers the old notion of optimisation or maximisation, given the broader objective.
searched :: Topology a p -> Double
searched = undefined

init :: Topology a p
init = undefined

{-

A good example may be the degenerate case of exploring the surface of what turns out to be a sphere. Suggested candidate parameters should quickly find their way to near the surface and then fan out across it with a smooth denisity.

-}

-- * linear regression examples

-- | This is a major categorization in machine learning, and probably a silly way if  you were to start fromn scratch.
data LearningType a c =
  Regression {fit :: (Ord a, Num a) => Params a} |
  Classification { enumerate :: (Eq c) => Params c}

{- |
If we take a linear regression, with parameters of alpha and betas unified as (a:bs), gives a carrier e of ([a], a). We take an [a] (the dependent variables) and produce an a, our guess as to the independent variable. We then take the second element of the tuple and know it is the underlying true answer to our guess. The difference between our guess and the correct answer is our measure, with the closer to zero the better.
-}
newtype Params a = Params { act :: [a] } deriving (Eq, Show, Generic)

-- | linear regression error
error' :: (Num a) => Params a -> ([a], a) -> a
error' (Params bs) (es, y) = sum (zipWith (*) bs (1:es)) - y

-- | errors over an Experience set
errors :: (Functor f, Num a) => Params a -> Experience f ([a],a) -> f a
errors p (Experience es) = error' p <$> es

-- | The value of a population, for each experience.
value :: (Traversable p, Traversable e, Applicative e, Num a) =>
  Population p (Params a) ->
  Experience e ([a], a) ->
  (e (p a) -> p a) ->
  p a
value (Population ps) (Experience es) f = f $ traverse (\p -> error' p <$> es) ps

-- * complex convergence
--
-- but this is tuned to the value-centric cycle approach.
--
data Dir = Up | Down deriving (Eq, Show)

data Stops a b =
  MaxIterations Int |
  Converged a |
  NotConverging
  deriving (Eq, Show, Generic)

defaultStops :: a -> [Stops a b]
defaultStops eps = [MaxIterations 100, Converged eps, NotConverging]
