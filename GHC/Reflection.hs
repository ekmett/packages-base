{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-------------------------------------------------------------------------
-- |
-- Module      :  GHC.Reflection
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  not portable
--
-- 'reify' turns a term into a type that you can later 'reflect' back
-- down to a term.
--
-- This provides an optimized implementation of the ideas presented in the paper
-- \"Functional Pearl: Implicit Configurations\" by Oleg Kiselyov and
-- Chung-chieh Shan.
--
-- The original paper can be obtained from
-- <http://www.cs.rutgers.edu/~ccshan/prepose/prepose.pdf>.
--
-- /Since: 4.7.0.0/
-------------------------------------------------------------------------

module GHC.Reflection
  ( Reifying(..)
  , reify#
  ) where

import GHC.Prim (Proxy#, proxy#, unsafeCoerce#)

class Reifying s where
  type Reified s :: *
  -- | Recover a reified value.
  reflect# :: Proxy# s -> Reified s

newtype Magic a r = Magic (forall (s :: *). (Reifying s, Reified s ~ a) => Proxy# s -> r)

-- TODO: Make this wiredIn
reify# :: forall a r. a -> (forall s. (Reifying s, Reified s ~ a) => Proxy# s -> r) -> r
reify# a k = unsafeCoerce# (Magic k :: Magic a r) (\_ -> a) proxy#
{-# INLINE reify# #-}
