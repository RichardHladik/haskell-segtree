{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module SegTree where

import Data.Monoid ((<>), Sum(..))

data Interval = Interval Int Int | Null | Everything
    deriving (Eq)

instance Show Interval where
    show (Interval a b) = "[" ++ show a ++ " " ++ show b ++ ")"


-- Summary of a given segment: the number of elements and its "sum"
data SegSummary t = SegSummary t Int

class (Monoid t, Monoid u) => Segmentable t u where
    apply :: u -> SegSummary t -> t

-- (Segmentable t u) =>
data SegTree t u = Empty |
                 SegTree {
                     interval :: Interval,
                     value :: t,
                     lazyOp :: u,
                     lson :: SegTree t u,
                     rson :: SegTree t u
                 }
    deriving (Show)

empty :: SegTree t u
empty = Empty

blankNode :: (Segmentable t u) => Interval -> SegTree t u
blankNode ival = SegTree ival mempty mempty Empty Empty


intersect :: Interval -> Interval -> Interval
intersect Everything i = i
intersect i Everything = i
intersect (Interval a b) (Interval c d)
    | l < r = Interval l r
    where
        l = max a c
        r = min b d
intersect _ _ = Null

intervalLength :: Interval -> Int
intervalLength Null = 0
intervalLength Everything = error "Can't measure the intervalLength of an infinite interval."
intervalLength (Interval a b) = b - a

-- Checks whether the given node is fully, partially, or not at all covered by
-- given interval
getNodeCoverage :: Interval -> SegTree t u -> Interval
getNodeCoverage _ Empty = Everything
getNodeCoverage qInterval node
    | isection == Null         = Null
    | isection == nodeInterval = Everything
    | otherwise                = isection
    where
        isection = intersect nodeInterval qInterval
        nodeInterval = interval node


-- Is assumed to be run on an unlazied SegTree, see unlazy below.
query' :: (Segmentable t u) => Interval -> SegTree t u -> t
query' _ Empty = mempty
query' qInterval node = case coverage of
    Null       -> mempty
    Everything -> value node
    _          -> lres <> rres
        where
            subquery = query' qInterval
            lres = subquery $ lson node
            rres = subquery $ rson node
    where
        coverage = getNodeCoverage qInterval node

-- Wrapper for query' that unlazies its argument first.
query :: (Segmentable t u) => Interval -> SegTree t u -> t
query qInterval node = query' qInterval unlazied
    where
        unlazied = unlazy qInterval node

-- Applies a operation to the whole segment tree -- in effect, adds a lazy
-- operation to the root
addOpToWhole :: (Segmentable t u) => u -> SegTree t u -> SegTree t u
addOpToWhole _ Empty = Empty
addOpToWhole op node = node { lazyOp = (lazyOp node) <> op }

applyLazy :: (Segmentable t u) => SegTree t u -> SegTree t u
applyLazy node = node { value = newVal, lazyOp = mempty }
    where
        val = value node
        op = lazyOp node
        summary = SegSummary val (intervalLength $ interval node)
        newVal = op `apply` summary


-- Unlazies the SegTree, preparing it for the given query.
unlazy :: (Segmentable t u) => Interval -> SegTree t u -> SegTree t u
unlazy _ Empty = Empty
unlazy qInterval node = case coverage of
    Null       -> node
    Everything -> node'
    _          -> node''
        where
            node'' = node' { lson = recurse lson', rson = recurse rson' }
            recurse = unlazy qInterval
    where
        coverage = getNodeCoverage qInterval node
        op = lazyOp node
        passOp = addOpToWhole op
        nodeWithChildren = createChildren node
        lson' = passOp $ lson nodeWithChildren
        rson' = passOp $ rson nodeWithChildren
        node' = (applyLazy node) { lson = lson', rson = rson' }


createChildren :: (Segmentable t u) => SegTree t u -> SegTree t u
createChildren Empty = error "Can't create children for an empty node"
createChildren node
    | intervalLength (interval node) <= 1 = node
createChildren node = node { lson = leftSon, rson = rightSon }
    where
        leftSon = newNodeIfEmpty l m $ lson node
        rightSon = newNodeIfEmpty m r $ rson node
        (l, m, r) = bisect $ interval node
        bisect (Interval a b) = (a, (a + b) `div` 2, b)
        newNodeIfEmpty left right Empty = blankNode (Interval left right)
        newNodeIfEmpty _ _ a = a

-- Assumed to be run on an unlazied SegTree.
update' :: (Segmentable t u) => u -> Interval -> SegTree t u -> SegTree t u
update' _ _ Empty = Empty
update' op qInterval node = case coverage of
    Null       -> node
    Everything -> node'
    _          -> node''
        where
            node'' = node { lson = recurse $ lson node, rson = recurse $ rson node }
            recurse = update' op qInterval
    where
        coverage = getNodeCoverage qInterval node
        node' = addOpToWhole op node

-- Wrapper for the former that unlazies the SegTree.
update :: (Segmentable t u) => u -> Interval -> SegTree t u -> SegTree t u
update op qInterval node = update' op qInterval unlazied
    where
        unlazied = unlazy qInterval node

instance Segmentable (Sum Int) (Sum Int) where
    apply (Sum op) (SegSummary (Sum val) len) = Sum (op * len + val)
