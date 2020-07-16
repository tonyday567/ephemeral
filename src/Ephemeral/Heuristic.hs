module Ephemeral.Heuristic where

import Data.Functor.Identity

newtype HyperHeuristic f g h a b = HyperHeuristic { unHH :: f (Heuristic g h a) -> f (Heuristic g h b) }

-- data HeuristicType = Constructive | Perturbative

-- any approach to problem solving or *self-discovery* that employs a practical method, not guaranteed to be optimal, perfect, or rational, but instead sufficient for reaching an immediate goal.
newtype Heuristic f g h a = Heuristic { unH :: Population f h a -> Population g h a }

-- Alg is an algebra and Population the carrier.
newtype Alg f g a b = Alg { unpop :: Population f g a -> b }

newtype Params f a = Params { unParams :: f a }
type Population f g a = f (Params g a)

-- cofunctor, like an average
type Neighbourhood = Heuristic Identity

-- mutation is isomorphic to (->).
type Mutation = Heuristic Identity Identity

-- crossover is foldable
type Crossover f = Heuristic f Identity

-- an individual can also be a carrier of the algebra
type Individual g a = Population Identity g a
