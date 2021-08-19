module Webbi.Utils.RoseTree where

import Webbi.Utils.Trie (Trie)
import qualified Webbi.Utils.Trie                      as T

import qualified Data.Map                      as M


data RoseTree a = RoseTree a [RoseTree a]
    deriving (Eq, Ord, Show)


datum :: RoseTree a -> a
datum (RoseTree x _) = x


children :: RoseTree a -> [RoseTree a]
children (RoseTree _ xs) = xs


fromTrie :: String -> Trie String -> RoseTree String
fromTrie root trie = RoseTree root (children trie)
    where
        children = fmap (\(k,v) -> RoseTree k (children v)) . M.toList . T.map
