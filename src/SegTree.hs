{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module SegTree (
    Interval(..), SegSummary(..), Segmentable(..), SegTree(..), intersect,
    intervalLength, getNodeCoverage, initTree, query, update, setPoint,
    queryPoint, updatePoint, fromList, fromList', sliceToList, toList
  ) where

import Data.Monoid ((<>), Sum(..))


data Interval = Interval Int Int | Null | Everything
    deriving (Eq)

instance Show Interval where
    show Null = "Null"
    show Everything = "Everything"
    show (Interval a b) = "[" ++ show a ++ " " ++ show b ++ ")"


-- Summary of a given segment: the number of elements and its "sum"
data SegSummary t = SegSummary t Int

-- The values of the sequence are of type t, the updates performed on them are
-- of type u. We have to both be able to "sum" the values and to compose
-- multiple updates. Additionally, we want to be able to apply the updates to
-- the values at some point.
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


----- Interval functions -----

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
    | isection == nodeInterval = Everything
    | otherwise                = isection
    where
        isection = intersect nodeInterval qInterval
        nodeInterval = interval node

----- Tree construction -----

empty :: SegTree t u
empty = Empty

-- Creates an empty SegTree with a given interval
blankNode :: (Monoid t, Monoid u) => Interval -> SegTree t u
blankNode ival = SegTree ival mempty mempty Empty Empty

-- Creates an empty SegTree, padding the given interval to the nearest greater
-- power of two.
initTree :: (Monoid t, Monoid u) => Interval -> SegTree t u
initTree ival = blankNode adjusted
    where
        len = intervalLength ival
        twoPower = head [2 ^ i | i <- [0..], 2 ^ i >= len]
        Interval l _ = ival
        adjusted = Interval l (l + twoPower)


----- Query/update -----

-- Is assumed to be run on an unlazied SegTree, see unlazy below.
query' :: (Monoid t) => Interval -> SegTree t u -> t
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
query qInterval = query' qInterval . unlazy qInterval


-- Assumed to be run on an unlazied SegTree.
update' :: (Segmentable t u) => u -> Interval -> SegTree t u -> SegTree t u
update' _ _ Empty = Empty
update' op qInterval node = case coverage of
    Null       -> node
    Everything -> addOpToWhole op node
    _          -> node'
        where
            node' = node { lson = lson', rson = rson', value = val }
            lson' = recurse $ lson node
            rson' = recurse $ rson node
            recurse = update' op qInterval
            val = lval <> rval
            lval = interpretedVal lson'
            rval = interpretedVal rson'
    where
        coverage = getNodeCoverage qInterval node

-- Wrapper for the former that unlazies the SegTree first.
update :: (Segmentable t u) => u -> Interval -> SegTree t u -> SegTree t u
update op qInterval = update' op qInterval . unlazy qInterval


-- Assumed to be run on an unlazied SegTree.
setPoint' :: (Segmentable t u) => t -> Int -> SegTree t u -> SegTree t u
setPoint' _ _ Empty = Empty
setPoint' val index node = case coverage of
    Null       -> node
    Everything -> node { value = val }
    _          -> node'' { value = val' }
        where
            node'' = node { lson = recurse $ lson node, rson = recurse $ rson node }
            recurse = setPoint' val index
            val' = (interpretedVal $ lson node'') <> (interpretedVal $ rson node'')
    where
        coverage = getNodeCoverage (Interval index (index + 1)) node

setPoint :: (Segmentable t u) => t -> Int -> SegTree t u -> SegTree t u
setPoint val index = setPoint' val index . unlazy (Interval index (index + 1))

----- Internals -----

-- Applies a operation to the whole segment tree -- in effect, adds a lazy
-- operation to the root
addOpToWhole :: (Monoid u) => u -> SegTree t u -> SegTree t u
addOpToWhole _ Empty = Empty
addOpToWhole op node = node { lazyOp = (lazyOp node) <> op }

-- Returns the node's val with the lazy operation applied to it
interpretedVal :: (Segmentable t u) => SegTree t u -> t
interpretedVal node = appliedVal
    where
        val = value node
        op = lazyOp node
        summary = SegSummary val (intervalLength $ interval node)
        appliedVal = op `apply` summary


-- Unlazies the SegTree, preparing it for the given query. Basically, the function ensures
-- that when traversing the tree according to the given query, all the visited
-- nodes have their lazyOp's already applied (= set to mempty)
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
        node' = node { lson = lson', rson = rson', value = interpretedVal node, lazyOp = mempty }


-- Converts node's Empty children to SegTree children with empty value and
-- correct bounds.
createChildren :: (Monoid t, Monoid u) => SegTree t u -> SegTree t u
createChildren Empty = error "Can't create children for an empty node"
createChildren node
    | intervalLength (interval node) <= 1 = node
    -- Base case, otherwise the recursion would go indefinitely.
createChildren node = node { lson = leftSon, rson = rightSon }
    where
        leftSon = newNodeIfEmpty l m $ lson node
        rightSon = newNodeIfEmpty m r $ rson node
        (l, m, r) = bisect $ interval node
        bisect (Interval a b) = (a, (a + b) `div` 2, b)
        newNodeIfEmpty left right Empty = blankNode (Interval left right)
        newNodeIfEmpty _ _ a = a

----- Utility functions -----

queryPoint :: (Segmentable t u) => Int -> SegTree t u -> t
queryPoint index = query $ Interval index (index + 1)

updatePoint :: (Segmentable t u) => u -> Int -> SegTree t u -> SegTree t u
updatePoint op index = update op $ Interval index (index + 1)

fromList' :: (Segmentable t u) => SegTree t u -> [t] -> Int -> SegTree t u
fromList' emptyTree list offset = foldr doNext emptyTree ops
    where
        ops = zip list [offset..]
        doNext :: (Segmentable t u) => (t, Int) -> SegTree t u -> SegTree t u
        doNext (val, index) = setPoint val index

fromList :: (Segmentable t u) => [t] -> SegTree t u
fromList list = fromList' emptyTree list 0
    where
        emptyTree = initTree $ Interval 0 (length list)

sliceToList :: (Segmentable t u) => SegTree t u -> Interval -> [t]
sliceToList node ival = foldr work [] [start..end - 1]
    where
        Interval start end = ival
        work index = (queryPoint index node :)

toList :: (Segmentable t u) => SegTree t u -> [t]
toList node = sliceToList node $ interval node
