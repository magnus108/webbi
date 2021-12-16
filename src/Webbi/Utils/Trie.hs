module Webbi.Utils.Trie where


import qualified Data.Map                      as M

import Debug.Trace


data Trie a = Trie
    { exists :: Bool
    , map :: M.Map a (Trie a)
    } deriving (Show, Eq)


instance (Show a, Ord a) => Semigroup (Trie a) where
    (Trie True m1) <> (Trie _    m2) = Trie True (M.unionWith (<>) m1 m2)
    (Trie _    m1) <> (Trie True m2) = Trie True (M.unionWith (<>) m1 m2)
    (Trie _    m1) <> (Trie _    m2) = Trie False (M.unionWith (<>) m1 m2)


empty :: Trie a
empty = Trie False M.empty


fromList :: (Trie b -> a -> Trie b) -> [a] -> Trie b
fromList f = foldl f empty


insert :: Trie String -> [String] -> Trie String
insert (Trie _ m) []       = Trie True m
insert (Trie b m) (x : xs) = Trie b $ M.insertWith (<>) x (insert empty xs) m
