This file is a literate Haskell program providing example usage of the SegTree
module.

First, imports: apart from importing the main module, we also import its
Instances submodule, in order to take advantage of predefined SegTree
instances.

> import SegTree
> import SegTree.Instances

The core data structure is SegTree. Its purpose is to represent a contiguous,
integer-indexed sequence of values, and to perform certain read and update
queries on it.

The most basic example of a segment tree is a sum segment tree, which operates
on a sequence of integers, supporting following operations:

1) set the value at position i to value v
2) calculate the sum of values in range [l, r)
3) add a constant x to every element in range [l, r)


In the general case, the values in the segment tree can be of arbitrary Monoid
type. The segment tree then supports the queries 1–3 with the following
changes:

2) calculate the `mconcat` of values in range [l, r) (that is, calculate their
sum, using <> instead of +)
3) apply modification m to each value in range [l, r)

Due to the way the segment tree works internally, the type of the modification
used in query 3) must also be an instance of Monoid, to allow the combination
of multiple not-yet-applied modifications. The value and modification types
must also be "compatible" in the sense that application of an modification to a
value is well-defined.


For the sake of simplicity, we have chosen the sum segment tree as an example
on which we will demonstrate basic library usage. See SegTree.Instances for
other common use cases.


We construct an empty SegTree by calling initTree, passing the interval we want
to operate on as an argument:

> tree = initTree $ Interval 0 10 :: SegTree (Sum Int) (Apply (Sum Int))

Apply is defined in SegTree.Instances. It performs '<> x' to every value in the
queried interval, where x is of the same type as the values.

Initially, all values in the tree are mempty:

> q1 = query (Interval 3 5) tree -- Sum 0

query performs the operation 2). The operation 1) is performed by setPoint:

> tree1 = setPoint (Sum 5) 3 tree
> q2 = query (Interval 3 5) tree1 -- Sum 5
> q3 = query (Interval 5 8) tree1 -- Sum 0

The operation 3) is performed by update:

> tree2 = update (Apply (Sum 2)) (Interval 5 7) tree1
> tree3 = update (Apply (Sum 1)) (Interval 1 6) tree2
> q4 = query (Interval 5 6) tree3 -- Sum 3

Both query and update truncate the supplied interval argument so that it fits
within the SegTree's interval. That means that updates outside the range the
tree was initTree'd with are discarded.

> t' = initTree $ Interval 0 3 :: SegTree (Sum Int) (Apply (Sum Int))
> t'' = update (Apply $ Sum 1) (Interval (-10) 10) t'
> q5 = query (Interval (-5) 5) t'' -- Sum 3


There are also a few utility functions: queryPoint returns a value at a given
point and updatePoint applies an operation to a single point. Of interest may
be fromList and toList, which convert between "normal" and SegTree
representation of the sequence, and their more general counterparts, fromList'
and sliceToList, which take additional arguments such as the interval we are
interested in to create a list from.

> l1 = map Sum [2..10]
> t1 = fromList l1 :: SegTree (Sum Int) (Apply (Sum Int))
> q6 = query (Interval 2 6) t1 -- Sum 22
> l2 = toList t1 -- l1 == l2
> l3 = sliceToList t1 (Interval 0 5) -- map Sum [2..6]
