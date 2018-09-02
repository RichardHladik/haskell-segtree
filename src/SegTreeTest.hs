{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

import Test.QuickCheck
import SegTree
import SegTree.Instances
import Data.Monoid
import Text.Printf

instance (Arbitrary a) => Arbitrary (Apply a) where
    arbitrary = Apply <$> arbitrary

instance Arbitrary Interval where
    arbitrary = sized $ \n -> do
        x1 <- arbitrary
        x2 <- arbitrary
        let a = min x1 x2
        let b = 1 + max x1 x2
        frequency 
            [ (1, return Null)
            , (1, return Everything)
            , (n, return $ Interval a b)
            ]

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

data QuerySet = QuerySet SumTree [Query]
    deriving (Show)
instance Arbitrary QuerySet where
    arbitrary = QuerySet <$> arbitraryEmptyTree <*> arbitrary


isPowerOfTwo :: Int -> Bool
isPowerOfTwo x = x `elem` [2 ^ i | i <- [0..], 2 ^ i <= x]

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

prop_intervalsPowersOfTwo tree = work tree
    where
        work Empty = True
        work n     = checkNode n && work (lson n) && work (rson n)
        checkNode = isPowerOfTwo . intervalLength . interval
        types = ( tree :: SumTree )

tests =
    [(property prop_fromToList, "fromList and toList")
    ,(property prop_sumsCorrectly, "sum sums")
    ,(property prop_intervalsPowersOfTwo, "|intervals| = 2 ^ k")
    ]

main = do
    mapM work tests
    where work (t, s) = printf "%-25s: " s >> quickCheck t
