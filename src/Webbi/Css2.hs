module Webbi.Css2
    ( Css(..)
    , fromTreeZipper
    , showCss
    )
where

import           Debug.Trace
import           Data.String
import           System.FilePath                ( splitPath )
import           Data.Maybe
import qualified Text.Blaze.Html5              as H

import qualified Webbi.Utils.TreeZipper2       as TZ
import qualified Webbi.Utils.RoseTree          as RT
import qualified Webbi.Utils.Trie              as T


import qualified Data.Map                      as M

import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import Data.Functor ((<&>))


data Css = Css (TZ.TreeZipper FilePath)
    deriving (Show)


fromTreeZipper :: TZ.TreeZipper FilePath -> Css
fromTreeZipper = Css


showLink :: FilePath -> H.Html
showLink path = H.link ! A.rel "stylesheet" ! A.href (fromString path)


showCss :: Css -> H.Html
showCss (Css tz) = mapM_ showLink paths
    where paths = catMaybes (collect "css/" tz) >>= TZ.children >>= TZ.path <&> ("/" ++)


collect :: FilePath -> TZ.TreeZipper FilePath -> [Maybe (TZ.TreeZipper FilePath)]
collect path tz = collectLeafs path tz : case TZ.up tz of
        Nothing  -> [collectRoot path tz]
        Just tz' -> collect path tz'


collectLeafs :: FilePath -> TZ.TreeZipper FilePath -> Maybe (TZ.TreeZipper FilePath)
collectLeafs path tz = TZ.down path tz


collectRoot :: FilePath -> TZ.TreeZipper FilePath -> Maybe (TZ.TreeZipper FilePath)
collectRoot path tz = TZ.navigateTo [path] (TZ.toForest tz)
