module Webbi.Menu
    ( Menu(..)
    , fromList
    , navigateTo
    , showMenu
    )
where

import Debug.Trace

import           System.FilePath                ( splitPath )
import           Data.Maybe
import qualified Text.Blaze.Html5              as H

import qualified Webbi.Utils.TreeZipper        as TZ
import qualified Webbi.Utils.RoseTree          as R
import qualified Webbi.Utils.Trie              as T


import qualified Data.Map                      as M


data Menu = Menu (TZ.TreeZipper FilePath)
    deriving (Show)


insert :: [String] -> T.Trie String -> T.Trie String
insert [] (T.Trie _ m) = T.Trie True m
insert ["index.html"] (T.Trie _ m) = T.Trie True m
insert (x:xs) (T.Trie b m) = T.Trie b $ M.insertWith (<>) x (insert xs T.empty) m


fromList :: [FilePath] -> Menu
fromList = Menu . TZ.fromTrie "/" . T.fromList insert . fmap splitPath


navigateTo :: FilePath -> Menu -> Menu
navigateTo route menu = find routes menu
  where
    routes = splitPath route
    find [] m = m
    find ["index.html"] m = m
    find (x : xs) (Menu m) = find xs (Menu (fromJust (TZ.down x m)))


showMenu :: Menu -> H.Html
showMenu (Menu s) = TZ.showMenu s
