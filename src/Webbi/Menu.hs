module Webbi.Menu
    ( Menu(..)
    , empty
    , insert
    , fromTrie
    , fromList
    )
where

import           Debug.Trace

import           System.FilePath                ( splitPath, joinPath )
import qualified Data.Map                      as M

import           Webbi.Utils.RoseTree

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


data Trie = Trie Bool (M.Map String Trie)
    deriving Show


empty :: Trie
empty = Trie False M.empty

-- index.html
-- posts/index.html
-- posts/post1.html
-- posts/post2.html
--       / true
--      /
--    posts true
--   /          \
--  post1,true post2, true

fromList :: [FilePath] -> Trie
fromList = foldr insert empty


insert :: FilePath -> Trie -> Trie
insert "" (Trie _ m) = Trie True m
insert "index.html" (Trie _ m) = Trie True m
insert fp (Trie b m) = Trie b $ M.alter (Just . maybe (insert subPath empty) (insert subPath)) x m
    where
        (x:xs) = splitPath fp
        subPath = joinPath xs
