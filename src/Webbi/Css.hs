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


showLink :: FilePath -> H.Html
showLink path = H.link ! A.rel "stylesheet" ! A.href (fromString ("/"++ path))


showCss :: Css -> H.Html
showCss (Css tz) = mapM_ showLink (TZ.path' =<< (TZ.collectLeafs ["css/"] =<< (TZ.parents tz)))
