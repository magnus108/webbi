module Webbi.Css
    ( Css(..)
    , fromList
    , showCss
    )
where

import Debug.Trace
import Data.String
import           System.FilePath                ( splitPath )
import           Data.Maybe
import qualified Text.Blaze.Html5              as H

import qualified Webbi.Utils.TreeZipper        as TZ
import qualified Webbi.Utils.RoseTree          as R
import qualified Webbi.Utils.Trie              as T


import qualified Data.Map                      as M

import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A



data Css = Css (TZ.TreeZipper FilePath)
    deriving (Show)



fromList :: [FilePath] -> Css
fromList = Css . TZ.fromTrie "/" . T.fromList T.insert . fmap splitPath


showCss :: Css -> H.Html
showCss (Css x) = mapM_ link (TZ.foldup x)
    where
        link y = H.link ! A.rel "stylesheet" ! A.href (fromString y)
