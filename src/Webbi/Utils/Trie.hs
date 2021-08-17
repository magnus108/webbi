module Webbi.Utils.Trie where


import qualified Data.Map                      as M
import           System.FilePath                ( splitPath, joinPath )


data Trie = Trie Bool (M.Map String Trie)
    deriving Show


instance Semigroup Trie where
    (Trie True m1) <> (Trie _ m2) = Trie True (m1 <> m2)
    (Trie _ m1) <> (Trie True m2) = Trie True (m1 <> m2)
    (Trie _ m1) <> (Trie _ m2) = Trie False (m1 <> m2)


empty :: Trie
empty = Trie False M.empty


fromList :: [FilePath] -> Trie
fromList = foldr insert empty . fmap splitPath


insert :: [String] -> Trie -> Trie
insert [] (Trie _ m) = Trie True m
insert ["index.html"] (Trie _ m) = Trie True m
insert (x:xs) (Trie b m) = Trie b $ M.insertWith (<>) x (insert xs empty) m
