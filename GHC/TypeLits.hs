{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-| This module is an internal GHC module.  It declares the constants used
in the implementation of type-level natural numbers.  The programmer interface
for working with type-level naturals should be defined in a separate library.

/Since: 4.6.0.0/
-}

module GHC.TypeLits
  ( -- * Kinds
    Nat, Symbol

    -- * Linking type and value level
  , Reifying(..)
  , reify#
  , SomeNat(..), SomeSymbol(..)
  , someNatVal, someSymbolVal

    -- * Functions on type nats
  , type (<=), type (<=?), type (+), type (*), type (^), type (-)

  ) where

import GHC.Base(Eq(..), Ord(..), Bool(True), otherwise)
import GHC.Num(Integer)
import GHC.Base(String)
import GHC.Show(Show(..))
import GHC.Prim(Proxy#, proxy#)
import GHC.Read(Read(..))
import GHC.Reflection(Reifying(..), reify#)
import Data.Maybe(Maybe(..))

-- | (Kind) This is the kind of type-level natural numbers.
data Nat

-- | (Kind) This is the kind of type-level symbols.
data Symbol

-- | This intance gives the integer associated with a type-level natural.
-- There are instances for every concrete literal: 0, 1, 2, etc.
instance SingI n => Reifying (n :: Nat) where
  type Reified n = Integer
  reflect# _ = case sing :: Sing n of
    SNat x -> x

-- | This instance gives the string associated with a type-level symbol.
-- There are instances for every concrete literal: "hello", etc.
instance SingI n => Reifying (n :: Symbol) where
  type Reified (n :: Symbol) = String
  reflect# _ = case sing :: Sing n of
    SSym x -> x

--------------------------------------------------------------------------------

-- | This type represents unknown type-level natural numbers.
data SomeNat    = forall (n :: Nat).    Reifying n => SomeNat    (Proxy# n)

-- | This type represents unknown type-level symbols.
data SomeSymbol = forall (n :: Symbol). Reifying n => SomeSymbol (Proxy# n)

-- | Convert an integer into an unknown type-level natural.
someNatVal :: Integer -> Maybe SomeNat
someNatVal n
  | n >= 0    = Just (reify# n SomeNat)
  | otherwise = Nothing

-- | Convert a string into an unknown type-level symbol.
someSymbolVal :: String -> SomeSymbol
someSymbolVal n = reify# n SomeSymbol

instance Eq SomeNat where
  SomeNat x == SomeNat y = reflect# x == reflect# y

instance Ord SomeNat where
  compare (SomeNat x) (SomeNat y) = compare (reflect# x) (reflect# y)

instance Show SomeNat where
  showsPrec p (SomeNat x) = showsPrec p (reflect# x)

instance Read SomeNat where
  readsPrec p xs = do (a,ys) <- readsPrec p xs
                      case someNatVal a of
                        Nothing -> []
                        Just n  -> [(n,ys)]


instance Eq SomeSymbol where
  SomeSymbol x == SomeSymbol y = reflect# x == reflect# y

instance Ord SomeSymbol where
  compare (SomeSymbol x) (SomeSymbol y) = compare (reflect# x) (reflect# y)

instance Show SomeSymbol where
  showsPrec p (SomeSymbol x) = showsPrec p (reflect# x)

instance Read SomeSymbol where
  readsPrec p xs = [ (someSymbolVal a, ys) | (a,ys) <- readsPrec p xs ]


--------------------------------------------------------------------------------

infix  4 <=?, <=
infixl 6 +, -
infixl 7 *
infixr 8 ^

-- | Comparison of type-level naturals, as a constraint.
type x <= y = (x <=? y) ~ True

-- | Comparison of type-level naturals, as a function.
type family (m :: Nat) <=? (n :: Nat) :: Bool

-- | Addition of type-level naturals.
type family (m :: Nat) + (n :: Nat) :: Nat

-- | Multiplication of type-level naturals.
type family (m :: Nat) * (n :: Nat) :: Nat

-- | Exponentiation of type-level naturals.
type family (m :: Nat) ^ (n :: Nat) :: Nat

-- | Subtraction of type-level naturals.
--
-- /Since: 4.7.0.0/
type family (m :: Nat) - (n :: Nat) :: Nat

--------------------------------------------------------------------------------
-- PRIVATE:

-- | This is an internal GHC class.  It has built-in instances in the compiler.
class SingI a where
  sing :: Sing a

-- | This is used only in the type of the internal `SingI` class.
data family      Sing (n :: k)
newtype instance Sing (n :: Nat)    = SNat Integer
newtype instance Sing (n :: Symbol) = SSym String
