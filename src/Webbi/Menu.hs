module Webbi.Menu
    ( Menu(..)
    , showMenu
    , fromTreeZipper
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


fromTreeZipper :: (TZ.TreeZipper FilePath) -> Menu
fromTreeZipper  = Menu


showMenu :: Maybe Menu -> H.Html
showMenu Nothing = return ()
showMenu (Just (Menu s)) = TZ.showMenu s
