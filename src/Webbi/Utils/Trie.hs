module Webbi.Utils.Trie where


import qualified Data.Map                      as M


data Trie a = Trie
    { exists :: Bool
    , map :: M.Map a (Trie a)
    } deriving (Show, Eq)


instance Ord a => Semigroup (Trie a) where
    (Trie True m1) <> (Trie _ m2) = Trie True (m1 <> m2)
    (Trie _ m1) <> (Trie True m2) = Trie True (m1 <> m2)
    (Trie _ m1) <> (Trie _ m2) = Trie False (m1 <> m2)


empty :: Trie a
empty = Trie False M.empty


fromList :: (a -> Trie b -> Trie b) -> [a] -> Trie b
fromList f = foldr f empty
