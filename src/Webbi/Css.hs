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
fromList = Css . TZ.fromList


showCss :: FilePath -> Css -> H.Html
showCss r (Css css) = mapM_ link (findCss (Css css'))
    where
        css' = TZ.navigateTo r css
        link y = H.link ! A.rel "stylesheet" ! A.href (fromString y)


findCss :: Css -> [String]
findCss (Css tz) = links ++ rest
    where 
        links = fmap TZ.path (TZ.leafs (TZ.navigateTo "css/" tz))
        rest = case TZ.up tz of
                    Nothing -> []
                    Just parent -> findCss (Css parent)

