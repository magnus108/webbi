module Webbi.Menu
    ( Menu(..)
    , empty
    , insert
    , fromTrie
    )
where

import qualified Data.Map                      as M

import           Webbi.Utils.RoseTree

data Menu = Menu (TreeZipper FilePath FilePath)


fromTrie :: Trie String -> Menu
fromTrie t = Menu (mkTreeZipper (mkRoseTree t))

mkRoseTree :: Trie String -> RoseTree String String
mkRoseTree (Trie m) = Branch "/" (fmap (\(k, (Trie v)) -> if null v then Leaf k else Branch k (fmap mkRoseTree (M.elems v))) xs)
    where
        xs = M.toList m


data Trie a = Trie (M.Map a (Trie a))
    deriving Show


empty :: Trie a
empty = Trie $ M.empty


insert :: Ord a => [a] -> Trie a -> Trie a
insert []       t        = t
insert (x : xs) (Trie t) = Trie $ M.alter
    (\t' -> case t' of
        Nothing  -> Just $ insert xs empty
        Just t'' -> Just $ insert xs t''
    )
    x
    t
