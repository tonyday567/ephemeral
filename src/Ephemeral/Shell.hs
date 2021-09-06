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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

{-* The Shell around a profunctor -}

module Ephemeral.Shell where

import Prelude
import Control.Monad
import Data.Profunctor
import GHC.Generics
import Data.Functor.Identity
import Data.String
import Data.Bool
import Data.Functor
import Yaya.Functor

{-

Take a Profunctor and slit it into three:

- the committer, the incoming contravariant input.
- the emitter, the outgoing convariant output, and
- the egg, which transforms the input to the output.

-}

-- | a Committer a "commits" values of type a. A Sink and a Consumer are some other metaphors for this.
--
-- A Committer absorbs the value being committed; the value disappears into the opaque thing that is a Committer from the pov of usage.
--
-- TODO: 
newtype Comm f a b = Comm { comm :: a -> f b }

newtype Committer f a = Committer
  { commit :: a -> f Bool
  }

instance HFunctor Committer where
  hmap nat (Committer c) = Committer $ nat . c

-- | an `Emitter` "emits" values of type a. A Source & a Producer (of a's) are the two other alternative but overloaded metaphors out there.
--
-- An Emitter "reaches into itself" for the value to emit, where itself is an opaque thing from the pov of usage.  An Emitter is named for its main action: it emits.

newtype Em g b c = Em { em :: g (Either c b)}

newtype Emitter f a = Emitter
  { emit :: f (Maybe a)
  }

instance HFunctor Emitter where
  hmap nat (Emitter e) = Emitter (nat e)

-- | A Shell is a product of a Committer m and an Emitter. It is it's own dual in the sense that it describes the connection points that can turn a nucleus into a profunctor, but can also wire up the profunctor to its outside context (it's closure). The Shell doesn't care which is which.
--
-- And either way, the committer is contravariant and the emitter covariant
-- so it forms a profunctor.
--
data Shell f g a c = Shell
  { committer :: Committer f a,
    emitter :: Emitter g c
  }

data Sh f g a c = forall b. Sh
  { c' :: Comm f a b,
    e' :: Em g b c
  }

hmap' :: (forall a. f a -> f' a) -> (forall a. g a -> g' a) -> Shell f g c e -> Shell f' g' c e
hmap' natf natg (Shell c e) = Shell (hmap natf c) (hmap natg e)

-- | Connect an emitter directly to a committer of the same type.
--
-- The monadic action returns when the committer finishes.
closeM :: (Monad m) => Shell m m a a -> m ()
closeM (Shell c e) = go
  where
    go = do
      a <- emit e
      c' <- maybe (pure False) (commit c) a
      when c' go

closeA :: (Applicative f, Functor g) => Shell f g a a -> g (f Bool)
closeA (Shell c e) = fmap (maybe (pure False) (commit c)) (emit e)

closeI :: Functor g => Shell Identity g a a -> g Bool
closeI (Shell c e) = fmap (runIdentity . maybe (Identity False) (commit c)) (emit e)

-- * composition

-- | composition of monadic shells
dotM :: (Monad m) => Shell m m a b -> Shell m m b c -> m (Shell m m a c)
dotM (Shell c e) (Shell c' e') = closeM (Shell c' e) $> Shell c e'

-- | composition of applicative boxes
dotA :: (Applicative f, Functor g) => Shell f g a b -> Shell f g b c -> g (Shell f g a c)
dotA (Shell c e) (Shell c' e') = closeA (Shell c' e) $> Shell c e'

-- |
dotI :: (Functor g) => Shell Identity g a b -> Shell Identity g b c -> g (Shell Identity g a c)
dotI (Shell c e) (Shell c' e') = closeI (Shell c' e) $> Shell c e'






