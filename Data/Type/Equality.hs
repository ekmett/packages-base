{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE PolyKinds          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Equality
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  not portable
--
-- Definition of propositional equality @(:~:)@. Pattern-matching on a variable
-- of type @(a :~: b)@ produces a proof that @a ~ b@.
--
-- /Since: 4.7.0.0/
-----------------------------------------------------------------------------



module Data.Type.Equality where

import Data.Maybe
import GHC.Enum
import GHC.Show
import GHC.Read
import GHC.Base

infix 4 :~:

-- | Propositional equality. If @a :~: b@ is inhabited by some terminating
-- value, then the type @a@ is the same as the type @b@. To use this equality
-- in practice, pattern-match on the @a :~: b@ to get out the @Refl@ constructor;
-- in the body of the pattern-match, the compiler knows that @a ~ b@.
--
-- /Since: 4.7.0.0/
data a :~: b where
  Refl :: a :~: a

-- with credit to Conal Elliott for 'ty', Erik Hesselink & Martijn van
-- Steenbergen for 'type-equality', Edward Kmett for 'eq', and Gabor Greif
-- for 'type-eq'

-- | Symmetry of equality
sym :: (a :~: b) -> (b :~: a)
sym Refl = Refl

-- | Transitivity of equality
trans :: (a :~: b) -> (b :~: c) -> (a :~: c)
trans Refl Refl = Refl

-- | Type-safe cast, using propositional equality
subst :: (a :~: b) -> a -> b
subst Refl x = x

-- | Lift equality into a unary type constructor
liftEq :: (a :~: b) -> (f a :~: f b)
liftEq Refl = Refl

-- | Lift equality into a binary type constructor
liftEq2 :: (a :~: a') -> (b :~: b') -> (f a b :~: f a' b')
liftEq2 Refl Refl = Refl

-- | Lift equality into a ternary type constructor
liftEq3 :: (a :~: a') -> (b :~: b') -> (c :~: c') -> (f a b c :~: f a' b' c')
liftEq3 Refl Refl Refl = Refl

-- | Lift equality into a quaternary type constructor
liftEq4 :: (a :~: a') -> (b :~: b') -> (c :~: c') -> (d :~: d')
        -> (f a b c d :~: f a' b' c' d')
liftEq4 Refl Refl Refl Refl = Refl

-- | Lower equality from a parameterized type into the parameters
lower :: (f a :~: f b) -> a :~: b
lower Refl = Refl

deriving instance Eq   (a :~: b)
deriving instance Show (a :~: b)
deriving instance Ord  (a :~: b)

instance a ~ b => Read (a :~: b) where
  readsPrec d = readParen (d > 10) (\r -> [(Refl, s) | ("Refl",s) <- lex r ])

instance a ~ b => Enum (a :~: b) where
  toEnum 0 = Refl
  toEnum _ = error "Data.Type.Equality.toEnum: bad argument"

  fromEnum Refl = 0

instance a ~ b => Bounded (a :~: b) where
  minBound = Refl
  maxBound = Refl

-- | This class contains types where you can learn the equality of two types
-- from information contained in /terms/. Typically, only singleton types should
-- inhabit this class.
class EqualityT f where
  -- | Conditionally prove the equality of @a@ and @b@.
  equalsT :: f a -> f b -> Maybe (a :~: b)

instance EqualityT ((:~:) a) where
  equalsT Refl Refl = Just Refl

