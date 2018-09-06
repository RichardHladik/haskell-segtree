{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-|
 Module      : SegTree
 Copyright   : (c) Richard Hladík, 2018
 License     : BSD3
 Maintainer  : rihl@uralyx.cz
-}

module SegTree (
    -- * Base classes and types
    Segmentable(..), SegTree(..), SegSummary(..),
    -- * Intervals
    Interval(..), intersect, intervalLength, unitInterval, normInterval,
    -- * Query/update
    initTree, query, update, setPoint, queryPoint, updatePoint,
    -- * Helper functions
    fromList, fromList', sliceToList, toList,
    -- * Reexports
    (<>)
  ) where

import Data.Monoid ((<>))


-- | A half-closed interval open on the right. `Null` represents an empty
-- interval, `Everything` represents its complement.
data Interval = Interval {-# UNPACK #-} !Int {-# UNPACK #-} !Int | Null | Everything
    deriving (Eq)

instance Show Interval where
    show Null = "Null"
    show Everything = "Everything"
    show (Interval a b) = "[" ++ show a ++ " " ++ show b ++ ")"


-- | Summary of a given segment: the number of elements and their "`mconcat`".
data SegSummary t = SegSummary !t {-# UNPACK #-} !Int

-- | The `Segmentable` type class is used for type pairs that can be (together)
-- used in a `SegTree`. The values of the sequence the `SegTree` operates on
-- are of type `t`, the updates performed on them are of type `u`.
--
-- It must be possible to `mconcat` the values and also to `mappend` multiple
-- updates. Moreover, `apply` provides a way to apply the updates to a segment.
class (Monoid t, Monoid u) => Segmentable t u where
    -- | Calculates the result of applying the operation to the given range of
    -- elements, given its length and the current value of `mconcat` of that
    -- segment.
    apply :: u -> SegSummary t -> t


-- | A segment tree node. For almost all operation, @t u@ is assumed to be an
-- instance of `Segmentable` @t u@. Represents a figurative, integer-indexed
-- list of values of type @t@. The supported operations are: perform an update
-- of type @u@ to all values in an interval and calculate the result of
-- performing `mconcat` on an interval.
data SegTree t u = Empty |
                 SegTree {
                     interval :: !Interval, -- ^ The interval managed by this node
                     value :: !t, -- ^ The result of calling `mconcat` on the given interval (disregarding `lazyOp`)
                     lazyOp :: !u, -- ^ The operation that is to be applied to each element in the interval
                     lson :: !(SegTree t u), -- ^ Left son.
                     rson :: !(SegTree t u)  -- ^ Right son.
                 }
    deriving (Show)


----- Interval functions -----

-- | Converts zero- and negative-length intervals to `Null`
normInterval :: Interval -> Interval
normInterval (Interval a b)
    | a >= b = Null
normInterval a = a

-- | Intersects two intervals.
intersect :: Interval -> Interval -> Interval
intersect Everything i = i
intersect i Everything = i
intersect (Interval a b) (Interval c d) = normInterval $ Interval l r
    where
        l = max a c
        r = min b d
intersect _ _ = Null

-- | Measures the length of an interval.
intervalLength :: Interval -> Int
intervalLength Null = 0
intervalLength Everything = error "Can't measure the intervalLength of an infinite interval."
intervalLength (Interval a b) = b - a

-- | Checks whether the given `SegTree` node is fully, partially, or not at all
-- covered by given interval. Returns `Everything` if the given interval is a
-- superset of the node's interval, otherwise returns the intersection of the
-- two.
getNodeCoverage :: Interval -> SegTree t u -> Interval
getNodeCoverage _ Empty = Everything
getNodeCoverage qInterval node
    | isection == nodeInterval = Everything
    | otherwise                = isection
    where
        isection = intersect nodeInterval qInterval
        nodeInterval = interval node

-- | Returns the interval [x, x+1).
unitInterval :: Int -> Interval
unitInterval i = Interval i (i + 1)

----- Tree construction -----

-- | Creates an empty `SegTree` managing a given interval. Internal use only
-- (might blow up later if the interval length isn't a power of two).
blankNode :: (Monoid t, Monoid u) => Interval -> SegTree t u
blankNode ival = SegTree ival mempty mempty Empty Empty

-- | Creates an empty `SegTree`, padding the given interval from the right to
-- the nearest greater power of two.
initTree :: (Monoid t, Monoid u) => Interval -> SegTree t u
initTree ival = blankNode adjusted
    where
        len = intervalLength ival
        twoPower = head [2 ^ i | i <- [0..], 2 ^ i >= len]
        Interval l _ = ival
        adjusted = Interval l (l + twoPower)


----- Query/update -----

-- | Internal worker function for `query`. Expects the `SegTree` to be
-- unlazied, see unlazy.
query' :: (Monoid t) => Interval -> SegTree t u -> t
query' _ Empty = mempty
query' qInterval node = case getNodeCoverage qInterval node of
    Null       -> mempty
    Everything -> value node
    _          -> lres <> rres
        where
            subq = query' qInterval
            lres = subq $ lson node
            rres = subq $ rson node

-- | Given an interval and a `SegTree`, calculates `mconcat` of the elements in
-- that interval. The interval is truncated to the given `SegTree`'s `interval`.
query :: (Segmentable t u) => Interval -> SegTree t u -> t
query qInterval = query' qInterval . unlazy qInterval


-- | Internal worker function for `update`. Expects the `SegTree` to be
-- unlazied, see unlazy.
update' :: (Segmentable t u) => u -> Interval -> SegTree t u -> SegTree t u
update' _ _ Empty = Empty
update' op qInterval node = case getNodeCoverage qInterval node of
    Null       -> node
    Everything -> addOpToWhole op node
    _          -> node'
        where
            node' = node { lson = lson', rson = rson', value = val }
            -- Since node has been unlazied, lazyOp = mempty.
            lson' = recurse $ lson node
            rson' = recurse $ rson node
            recurse = update' op qInterval
            lval = interpretedVal lson'
            rval = interpretedVal rson'
            val = lval <> rval

-- | Given an update operation, an interval and a `SegTree`, lazily performs
-- the operation to every element in the interval. The interval is truncated to
-- the given `SegTree`'s interval.
update :: (Segmentable t u) => u -> Interval -> SegTree t u -> SegTree t u
update op qInterval = update' op qInterval . unlazy qInterval


-- | Internal worker function for `setPoint`. Expects the `SegTree` to be
-- unlazied, see unlazy.
setPoint' :: (Segmentable t u) => t -> Int -> SegTree t u -> SegTree t u
setPoint' _ _ Empty = Empty
setPoint' val index node = case getNodeCoverage (unitInterval index) node of
    Null       -> node
    Everything -> node { value = val }
    _          -> node' { value = val' }
        where
            node' = node { lson = recurse $ lson node, rson = recurse $ rson node }
            recurse = setPoint' val index
            val' = (interpretedVal $ lson node') <> (interpretedVal $ rson node')

-- | Sets a value at a given index to a given value.
setPoint :: (Segmentable t u) => t -> Int -> SegTree t u -> SegTree t u
setPoint val index = setPoint' val index . unlazy (Interval index (index + 1))

----- Internals -----

-- | Applies a operation to the whole list -- in effect, adds a lazy operation
-- to the root.
addOpToWhole :: (Monoid u) => u -> SegTree t u -> SegTree t u
addOpToWhole _ Empty = Empty
addOpToWhole op node = node { lazyOp = (lazyOp node) <> op }

-- | Returns the node's `value` with the `lazyOp` `apply`'d to it
interpretedVal :: (Segmentable t u) => SegTree t u -> t
interpretedVal node = appliedVal
    where
        val = value node
        op = lazyOp node
        summary = SegSummary val (intervalLength $ interval node)
        appliedVal = op `apply` summary


-- | Unlazies the SegTree, preparing it for the given query. Basically, the
-- function ensures that when traversing the tree according to the given
-- interval (in `query`, `update`, …), all the visited nodes have their
-- `lazyOp`s already applied (= set to `mempty`)
unlazy :: (Segmentable t u) => Interval -> SegTree t u -> SegTree t u
unlazy _ Empty = Empty
unlazy qInterval node = case getNodeCoverage qInterval node of
    Null       -> node
    Everything -> node'
    _          -> node''
        where
            node'' = node' { lson = recurse lson', rson = recurse rson' }
            recurse = unlazy qInterval
    where
        op = lazyOp node
        passOp = addOpToWhole op
        nodeWithChildren = createChildren node
        lson' = passOp $ lson nodeWithChildren
        rson' = passOp $ rson nodeWithChildren
        node' = node { lson = lson', rson = rson', value = interpretedVal node, lazyOp = mempty }


-- Converts node's `Empty` children to `SegTree` children with empty `value`
-- and `lazyOp` and correct `interval`. Leaves non`Empty` children untouched.
createChildren :: (Monoid t, Monoid u) => SegTree t u -> SegTree t u
createChildren Empty = error "Can't create children for an empty node"
createChildren node
    | intervalLength (interval node) <= 1 = node
    -- Base case, otherwise the recursion would go indefinitely.
createChildren node = node { lson = leftSon, rson = rightSon }
    where
        leftSon = newNodeIfEmpty (l,m) $ lson node
        rightSon = newNodeIfEmpty (m,r) $ rson node
        (l, m, r) = bisect $ interval node
        bisect (Interval a b) = (a, (a + b) `div` 2, b)
        newNodeIfEmpty (left,right) Empty = blankNode $ Interval left right
        newNodeIfEmpty _ a = a

----- Utility functions -----

-- | Returns the value at the given index.
queryPoint :: (Segmentable t u) => Int -> SegTree t u -> t
queryPoint index = query $ unitInterval index

-- | Performs an update operation at the given index.
updatePoint :: (Segmentable t u) => u -> Int -> SegTree t u -> SegTree t u
updatePoint op index = update op $ unitInterval index

-- | Sequentially copies the values from the list to the `SegTree`, starting at
-- a given offset. Silently discards values that aren't inside the `SegTree`'s
-- `interval`.
fromList' :: (Segmentable t u) => SegTree t u -> [t] -> Int -> SegTree t u
fromList' emptyTree list offset = foldr doNext emptyTree ops
    where
        ops = zip list [offset..]
        doNext :: (Segmentable t u) => (t, Int) -> SegTree t u -> SegTree t u
        doNext (val, index) = setPoint val index

-- | Given a list, constructs a zero-indexed `SegTree` on top of it.
fromList :: (Segmentable t u) => [t] -> SegTree t u
fromList list = fromList' emptyTree list 0
    where
        emptyTree = initTree $ Interval 0 (length list)

-- | Extracts a given interval from the `SegTree` to a list.
sliceToList :: (Segmentable t u) => SegTree t u -> Interval -> [t]
sliceToList node ival = foldr work [] [start..end - 1]
    where
        Interval start end = ival
        work index = (queryPoint index node :)

-- | Extracts the values from a `SegTree` to a list.
toList :: (Segmentable t u) => SegTree t u -> [t]
toList node = sliceToList node $ interval node
