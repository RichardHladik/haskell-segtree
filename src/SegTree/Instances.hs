{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-|
 Module      : SegTree
 Copyright   : (c) Richard Hladík, 2018
 License     : BSD3
 Maintainer  : rihl@uralyx.cz

 Specific SegTree, Monoid and Segmentable instances for common use cases.
-}

module SegTree.Instances (
    Exponentiable(..), CommutativeMonoid(..), Change(..), Apply(..), Min(..), Sum(..)
  ) where

import SegTree
import Data.Monoid (Sum(..))


-- | Class of monoids whose elements can be raised to (nonnegative integer)
-- powers. Provides a sane O(log n) default implementation.
class (Monoid t) => Exponentiable t where
    -- | Raises an element to (nonnegative integer) power. The default
    -- implementation uses O(log n) `mappend` applications.
    power :: t -> Int -> t
    power base n
        | n == 0 = mempty
        | n < 0  = error "Negative powers not allowed"
        | odd n  = let a = power base (n `div` 2) in a <> a
        | even n = base <> power base (n - 1)

-- | The class of commutative monoids. Instances should satisfy @a <> b = b <>
-- a@ (this isn't checked by the compiler).
class (Monoid t) => CommutativeMonoid t


-- | Represents the operation of changing an interval to a constant value.
data Change a = Change a | NoChange
instance (Show a) => Show (Change a) where
    show NoChange = "NoChange"
    show (Change a) = show a
instance (Monoid a) => Monoid (Change a) where
    mempty = NoChange
    a `mappend` NoChange = a
    NoChange `mappend` a = a
    a `mappend` b = b
instance (Monoid a, Exponentiable a) => Segmentable a (Change a) where
    apply NoChange (SegSummary a _) = a
    apply (Change new) (SegSummary _ len) = new `power` len

-- | Represents the operation of performing "<> const" to every element of an interval
newtype Apply a = Apply a
instance (Show a) => Show (Apply a) where
    show (Apply a) = show a
instance (Monoid a) => Monoid (Apply a) where
    mempty = Apply mempty
    (Apply a) `mappend` (Apply b) = Apply (a `mappend` b)
instance (CommutativeMonoid a, Exponentiable a) => Segmentable a (Apply a) where
    apply (Apply a) (SegSummary val len) = val <> (a `power` len)


-- | Monoid under `min`.
data Min a = MinInfinity | Min a
instance (Show a) => Show (Min a) where
    show MinInfinity = "MinInfinity"
    show (Min a) = show a
instance (Ord a) => Monoid (Min a) where
    mempty = MinInfinity
    MinInfinity `mappend` a = a
    a `mappend` MinInfinity = a
    Min a `mappend` Min b = Min (a `min` b)


instance Exponentiable (Sum Int) where
    (Sum a) `power` b = Sum $ a * b
instance CommutativeMonoid (Sum Int)

instance (Ord a) => Exponentiable (Min a) where
    a `power` b = a
instance (Ord a) => CommutativeMonoid (Min a)
