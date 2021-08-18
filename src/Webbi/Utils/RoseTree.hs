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


fromTrie :: Trie String -> [RoseTree String]
fromTrie x = fmap (\(k,v) -> RoseTree k (fromTrie v)) xs -- safeHead $ fmap (\(k,v) -> if (null (T.map v)) then lllll ) xs -- Branch (M. undefined -- Branch "/" (fmap (\(k, (T.Trie b v)) -> if null v then Leaf k else Branch k (mkRoseTree' (M.toList v))) xs)
    where
        m = T.map x
        xs = M.toList m

    {-
fromTrie :: Trie String -> RoseTree String String
fromTrie (T.Trie b m) = Branch "/" (fmap (\(k, (T.Trie b v)) -> if null v then Leaf k else Branch k (mkRoseTree' (M.toList v))) xs)
    where
        xs = M.toList m

mkRoseTree' :: [(String, Trie String)] -> [RoseTree String String]
mkRoseTree' (xs) = (fmap (\((k, T.Trie b v)) -> if null v then Leaf k else Branch k (mkRoseTree' (M.toList v))) xs)
-}
