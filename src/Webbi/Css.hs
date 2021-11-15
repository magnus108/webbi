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


fromTreeZipper :: (TZ.TreeZipper FilePath) -> Css
fromTreeZipper  = Css


showCss :: Maybe Css -> H.Html
showCss Nothing = return ()
showCss (Just (Css tz)) = mapM_ link (links =<< parents tz)
    where
        links tz = fmap TZ.path $ TZ.leafs $ TZ.navigateTo "css/" tz
        link y = H.link ! A.rel "stylesheet" ! A.href (fromString y)
        parents tz = tz : case TZ.up tz of
                                        Nothing -> []
                                        Just p -> parents p
