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


data Css = Css (TZ.TreeZipper FilePath)
    deriving (Show)


fromTreeZipper :: TZ.TreeZipper FilePath -> Css
fromTreeZipper = Css


showLink :: FilePath -> H.Html
showLink path = H.link ! A.rel "stylesheet" ! A.href (fromString ("/" ++ path))


showCss :: Css -> H.Html
showCss (Css tz) = mapM_ showLink (TZ.path =<< (collect "css/" tz))


collect :: FilePath -> TZ.TreeZipper FilePath -> [TZ.TreeZipper FilePath]
collect path tz =
    collectLeafs path tz
        ++ (case TZ.up tz of
               Nothing  -> collectRoot path tz
               Just tz' -> collect path tz'
           )

collectRoot :: FilePath -> TZ.TreeZipper FilePath -> [TZ.TreeZipper FilePath]
collectRoot path tz = TZ.children =<< maybeToList
    (fmap TZ.fromRoseTree (RT.findRoseTree path (TZ.toForest tz)))


collectLeafs :: FilePath -> TZ.TreeZipper FilePath -> [TZ.TreeZipper FilePath]
collectLeafs path tz = TZ.children =<< (maybeToList (TZ.down path tz))
