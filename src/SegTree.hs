module SegTree where

data Interval = Interval Int Int | Null | All
    deriving (Eq)

instance Show Interval where
    show (Interval a b) = "[" ++ show a ++ " " ++ show b ++ "]"

data SegTree t u = Empty |
                   SegTree {
                       interval :: Interval,
                       value :: t,
                       lazyVal :: Maybe u,
                       lson :: SegTree t u,
                       rson :: SegTree t u
                   }

empty :: SegTree t u
empty = Empty
