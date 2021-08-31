module Webbi.Menu
    ( Menu(..)
    , fromList
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


fromList :: [FilePath] -> Menu
fromList = Menu . TZ.fromList


showMenu :: Menu -> H.Html
showMenu (Menu s) = TZ.showMenu s
