module Webbi.Utils.ListZipper where

data ListZipper a = ListZipper [a] a [a]
    deriving (Show, Eq, Ord)
    deriving (Functor)
