module Webbi.Utils.RoseTree where

import           Webbi.Utils.Trie               ( Trie )
import qualified Webbi.Utils.Trie              as T

import qualified Data.Map                      as M
import Data.List


data Forest a = Forest [RoseTree a]
    deriving (Eq, Ord, Show)
    deriving (Functor)
    

findRoseTree :: Eq a => a -> Forest a -> Maybe (RoseTree a)
findRoseTree a (Forest xs) = find (\x -> a == (datum x)) xs


data RoseTree a = RoseTree a [RoseTree a]
    deriving (Eq, Ord, Show)
    deriving (Functor)


eqDatum :: (Eq a) => RoseTree a -> RoseTree a -> Bool
eqDatum x y = (datum x) == (datum y)


datum :: RoseTree a -> a
datum (RoseTree x _) = x


children :: RoseTree a -> [RoseTree a]
children (RoseTree _ xs) = xs


fromTrie :: Trie a -> Forest a
fromTrie trie = Forest $ children trie
  where
    children = fmap (\(k, v) -> RoseTree k (children v)) . M.toList . T.map
