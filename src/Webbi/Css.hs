module Webbi.Css
    ( Css(..)
    , fromTreeZipper
    , showCss
    )
where

import Debug.Trace
import Data.String
import           System.FilePath                ( splitPath )
import           Data.Maybe
import qualified Text.Blaze.Html5              as H

import qualified Webbi.Utils.TreeZipper        as TZ
import qualified Webbi.Utils.RoseTree          as RT
import qualified Webbi.Utils.Trie              as T


import qualified Data.Map                      as M

import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A



data Css = Css (TZ.TreeZipper FilePath)
    deriving (Show)


fromTreeZipper :: TZ.TreeZipper FilePath -> Css
fromTreeZipper  = Css


showCss :: Css -> H.Html
showCss (Css tz) = mapM_ link (catMaybes (links =<< (parents tz)))
    where
        links tz = fmap TZ.path $ TZ.leafs $ TZ.navigateTo "css/" tz
        link y = do 
            traceShowM y
            H.link ! A.rel "stylesheet" ! A.href (fromString y)
        parents tz = case TZ.up tz of
                                Nothing -> tz : []
                                Just p -> tz : parents p
