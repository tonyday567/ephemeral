{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

{- |

Functional machinery, rapid learning.

-}

module Ephemeral where

import NumHask.Prelude
import Data.Functor.Contravariant
import Data.Profunctor

{- |

A computer program is said to learn from experience E with respect to some task T and some performance measure P, if its performance on T, as measured by P, improves with experience E. ~ Tom Mitchell

The thing about Haskell types I like is how they help me think.

So the quote above looks like this:

-}

-- | to learn, is to use experience to change the performance of a task.
newtype Learn f e p =
  Learn { change :: (Foldable f) => Experience f e -> Task e p -> Task e p }

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

-- | to learn, is to make Progress from an Experience.
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
