module Webbi.Utils.TreeZipper where

import Webbi.Utils.Trie (Trie)
import Webbi.Utils.RoseTree as RT

data Context b l = Context [RoseTree b l] b [RoseTree b l]
    deriving (Show, Eq, Ord)


data TreeZipper b l = TreeZipper (RoseTree b l) [Context b l]
    deriving (Show, Eq, Ord)


toRoseTree :: TreeZipper b l -> RoseTree b l
toRoseTree (TreeZipper item _) = item


toContext :: TreeZipper b l -> [Context b l]
toContext (TreeZipper _ item) = item


fromRoseTree :: RoseTree b l -> TreeZipper b l
fromRoseTree x = TreeZipper x []


fromTrie :: Trie -> TreeZipper String String
fromTrie = fromRoseTree . RT.fromTrie


down :: (Show b, Show l, Eq b, Eq l) => Either b l -> TreeZipper b l -> Maybe (TreeZipper b l)
down x (TreeZipper (Branch parent items) bs) =
    let
        (ls, rs) = break (\item -> (datum item) == x) items
    in
        case rs of
            y:ys -> Just (TreeZipper y (Context ls parent ys:bs))
            _ -> Nothing
down _ _ = Nothing


up :: TreeZipper b l -> Maybe (TreeZipper b l)
up (TreeZipper item (Context ls x rs:bs)) =
    Just (TreeZipper (Branch x (ls <> [item] <> rs)) bs)
up _ = Nothing
