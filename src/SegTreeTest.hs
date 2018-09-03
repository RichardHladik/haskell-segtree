{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

import Test.QuickCheck
import SegTree
import SegTree.Instances
import Data.List (foldl')
import Text.Printf

instance (Arbitrary a) => Arbitrary (Apply a) where
    arbitrary = Apply <$> arbitrary

instance Arbitrary Interval where
    arbitrary = sized $ \n -> do
        x1 <- arbitrary
        x2 <- arbitrary
        let a = min x1 x2
        let b = 1 + max x1 x2 -- discard the possibility of zero-length Intervals
        frequency 
            [ (1, return Null)
            , (1, return Everything)
            , (n, return $ Interval a b)
            ]

-- Doesn't generate "sparse" SegTrees, but suffices for our purposes
instance (Segmentable t u, Arbitrary t, Arbitrary u) => Arbitrary (SegTree t u) where
    arbitrary = do
        offset <- arbitrary
        list <- arbitrary
        let len = length list
        return $ fromList' (initTree $ Interval offset (offset + len)) list offset

-- We choose a particular type to make things easier
type SumTree = SegTree (Sum Int) (Apply (Sum Int))

arbitraryEmptyTree :: Gen SumTree
arbitraryEmptyTree = initTree <$> arbitrary `suchThat` (notSpecial)
    where
        notSpecial Null = False
        notSpecial Everything = False
        notSpecial _ = True

data Query = Query Interval | Update (Apply (Sum Int)) Interval
    deriving (Show)
instance Arbitrary Query where
    arbitrary = do
        arbitraryQuery <- Query <$> arbitrary
        arbitraryUpdate <- Update <$> arbitrary <*> arbitrary
        elements [arbitraryQuery, arbitraryUpdate]

-- A single "problem instance": an empty SumTree, along with queries to process
data QuerySet = QuerySet SumTree [Query]
    deriving (Show)
instance Arbitrary QuerySet where
    arbitrary = QuerySet <$> arbitraryEmptyTree <*> arbitrary


isPowerOfTwo :: Int -> Bool
isPowerOfTwo x = x `elem` [2 ^ i | i <- [0..], 2 ^ i <= x]

inInterval :: Int -> Interval -> Bool
inInterval _ Everything = True
inInterval _ Null = False
inInterval num (Interval a b) = a <= num && b > num


----- Fake SegTree -----
-- FakeTree is a simple list-based data structure which mimicks the SegTree.

data FakeTree = FakeTree { getOffset :: Int, getValues :: [Sum Int] }

zipFakeTree :: FakeTree -> [(Int, Sum Int)]
zipFakeTree (FakeTree offset xs) = zip [offset..] xs

fakeMap :: (Int -> Sum Int -> Sum Int) -> FakeTree -> FakeTree
fakeMap f ft = ft { getValues = map (uncurry f) $ zipFakeTree ft }

fakeUpdate :: Apply (Sum Int) -> Interval -> FakeTree -> FakeTree
fakeUpdate (Apply val) ival ft = fakeMap go ft
    where
        go ix = if inInterval ix ival then (val <>) else id

fakeQuery :: Interval -> FakeTree -> Sum Int
fakeQuery ival ft = foldl' (<>) mempty filtered
    where
        filterIn = filter ((`inInterval` ival) . fst)
        filtered = snd . unzip . filterIn $ zipFakeTree ft


realToFake :: SumTree -> FakeTree
realToFake st = FakeTree offset $ toList st
    where
        Interval offset _ = interval st

fakeProcess :: QuerySet -> [Sum Int]
fakeProcess (QuerySet tr qs) = reverse . snd $ foldr go (realToFake tr,[]) $ reverse qs
    where
        go (Query iv) (t,res) = (t,fakeQuery iv t : res)
        go (Update val iv) (t,res) = (fakeUpdate val iv t, res)

realProcess :: QuerySet -> [Sum Int]
realProcess (QuerySet tree qs) = reverse . snd $ foldr go (tree,[]) $ reverse qs
    where
        go (Query iv) (t,res) = (t,query iv t : res)
        go (Update val iv) (t,res) = (update val iv t, res)


----- Properties -----

prop_fromToList list = len_poweroftwo && start_same && end_mempty
    where
        len_poweroftwo = isPowerOfTwo $ length list'
        tree = fromList list :: SumTree
        list' = toList tree
        start_same = and $ zipWith (==) list list'
        end_mempty = all (== mempty) $ drop (length list) list'

prop_sumsCorrectly list = query Everything tree == listSum
    where
        listSum = mconcat list
        tree = fromList list :: SumTree

prop_intervalsPowersOfTwo tree = go tree
    where
        go Empty = True
        go n     = checkNode n && go (lson n) && go (rson n)
        checkNode = isPowerOfTwo . intervalLength . interval
        types = ( tree :: SumTree )

prop_realIsFake queries = realProcess queries == fakeProcess queries

tests =
    [(property prop_fromToList, "fromList and toList")
    ,(property prop_sumsCorrectly, "sum sums")
    ,(property prop_intervalsPowersOfTwo, "|intervals| = 2 ^ k")
    ,(property prop_realIsFake, "real is fake (bruteforce)")
    ]

main = do
    mapM go tests
    where go (t, s) = printf "%-25s: " s >> quickCheck t
