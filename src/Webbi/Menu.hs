module Webbi.Menu
    ( Menu(..)
    , empty
    , insert
    , fromTrie
    , fromList
    )
where

import           Webbi.Utils.RoseTree
import           Webbi.Utils.Trie

import qualified Data.Map                      as M

data Menu = Menu (TreeZipper FilePath FilePath)
    deriving (Show)


fromTrie :: Trie -> Menu
fromTrie t = Menu (mkTreeZipper (mkRoseTree t))

mkRoseTree :: Trie -> RoseTree String String
mkRoseTree (Trie b m) = Branch "/" (fmap (\(k, (Trie b v)) -> if null v then Leaf k else Branch k (mkRoseTree' (M.toList v))) xs)
    where
        xs = M.toList m

mkRoseTree' :: [(String, Trie)] -> [RoseTree String String]
mkRoseTree' (xs) = (fmap (\((k, Trie b v)) -> if null v then Leaf k else Branch k (mkRoseTree' (M.toList v))) xs)


