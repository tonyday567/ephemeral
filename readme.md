[![img](https://img.shields.io/hackage/v/ephemeral.svg)](https://hackage.haskell.org/package/ephemeral)
[![img](https://github.com/tonyday567/ephemeral/actions/workflows/haskell-ci.yml/badge.svg?branch=main)](https://github.com/tonyday567/ephemeral/actions)

Functional
Machinery,
Rapid
Learning

# What is Learning?

> A computer program is said to learn from experience E with respect to
> some task T and some performance measure P, if its performance on T,
> as measured by P, improves with experience E. ~ Tom Mitchell

Here&rsquo;s how I unpacked this into types:

    -- | learning is to use experience to
    -- change the performance of a task.
    newtype Learn f e p =
      Learn {
        change ::
            (Foldable f) =>
            Experience f e ->
            Task e p ->
            Task e p }

A common approach to newtypes is to label the unwrapping as a reversal
of the newtype (such as \`unlearn\` or \`unLearn\`) but this misses an
important naming opportunity: to use a Learn is to change.

    -- | An experience is a set of e's
    newtype Experience f e =
      Experience { set :: f e }

    -- | A task takes an e and measures
    -- performance.
    newtype Task e p =
      Task { measure :: e -> p }
      deriving (Profunctor)

This could be thought of (and coded) as an ordinary function with type
(e -> p). The advantage of using a newtype, however, is to highlight the
two ways that a task can change:

-   changing the way a task is performed (the \`e\`).
-   changing the way a task is measured (the \`p\`).

Acting on a learning, you can change the way a task is performed, such
as changing parameters of a function, or change the way you view what
performance is.

To change the way a task is performed, taking experience into account,
is to make progress. Progress is made one step at a time.

    -- | To progress, is to transduce
    -- a Task, via a carrier
    newtype Progress e p =
      Progress {
        step :: e -> Task e p -> Task e p
      }

Putting these above types together in a useful way, to learn, is then to
repeatedly apply a progressive step to a set of experiences.

    -- | to learn, is to make Progress
    -- from an Experience.
    learn :: Progress e p -> Learn f e p
    learn p =
      Learn $
      \(Experience e) task ->
        foldr (step p) task e

To \`machine learn\`, we change the way a \`machine\` does a task, based on
learning, measuring whether performance improves, and adopt the new way
to do a task if it does.

    -- | to improve, given a way to change
    -- a task by experience,
    -- you need to choose the better
    -- performance way over an experience
    -- set.
    improve ::
      ( Ord (f p),
        Foldable f,
        Functor f) =>
      Progress e p ->
      Experience f e ->
      Task e p ->
      Task e p
    improve progress es task =
      bool task task' (p' > p)
      where
        task' =
          change (learn progress) es task
        p = measure task <$> set es
        p' = measure task' <$> set es

The constraints contained in the above code are mostly based on
suggestions from ghc to get a compile, and I didn&rsquo;t think about them
much in drafting. Type constraints provide useful guide-rails for future
development of these ideas.


# Machine Learning in Haskell?

In the above example of improving we are left with an \`Ord (f p)\`
constraint - that the performance over an experience set is ordinal. If
we narrow the constraint to a \`Num (f p)\`, in other words, assume that
the size of the \`f p\` is meaningful, then we have wandered in to the
traditional numerical methods domain, and can, for example, start to
think about gradients to speed up our improvement.

By insisting on useful and concrete names (aka types), Haskell supplies
a constrained domain we can further explore. For example, are there
machine learning examples that don&rsquo;t fit the candidate types and
constraints?

Naming things is hard, and other ways of coding tend to avoid the task.
Types bring me joy and I shall keep them, thank you very much, and
continue to curate collections of them, for fun and profit. In this
example, I now have as good a definition of machine learning as you&rsquo;ll
find out in the wild, and my progress on machine learning will be more
comprehensive, faster and safer than bashing away in python.

And yet, to a first degree, machine learning in Haskell does not exist,
swamped by the easiness of dynamic types. If the promises being made by
ML are to be eventually honoured, however, it may need less code bashing
and better naming.


