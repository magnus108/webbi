module Webbi.Utils.TreeZipper where

import Webbi.Utils.Trie (Trie)
import Webbi.Utils.RoseTree as RT


data Context a = Context [RoseTree a] a [RoseTree a]
    deriving (Show, Eq, Ord)


data TreeZipper a = TreeZipper (RoseTree a) [Context a]
    deriving (Show, Eq, Ord)


toRoseTree :: TreeZipper a -> RoseTree a
toRoseTree (TreeZipper item _) = item


toContext :: TreeZipper a -> [Context a]
toContext (TreeZipper _ item) = item


fromRoseTree :: RoseTree a -> TreeZipper a
fromRoseTree x = TreeZipper x []


fromTrie :: Trie String -> TreeZipper String
fromTrie = fromRoseTree . head . RT.fromTrie


down :: (Show a, Eq a) => a -> TreeZipper a -> Maybe (TreeZipper a)
down x (TreeZipper (RoseTree parent items) bs) =
    let
        (ls, rs) = break (\item -> (datum item) == x) items
    in
        case rs of
            y:ys -> Just (TreeZipper y (Context ls parent ys:bs))
            _ -> Nothing


up :: TreeZipper a -> Maybe (TreeZipper a)
up (TreeZipper item (Context ls x rs:bs)) =
    Just (TreeZipper (RoseTree x (ls <> [item] <> rs)) bs)
