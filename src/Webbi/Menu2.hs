{-# LANGUAGE TupleSections #-}
module Webbi.Menu2
    ( Menu(..)
    , showMenu
    , fromTreeZipper
    )
where

import           Data.Char
import           Data.String
import           System.FilePath                ( takeFileName
                                                , takeExtension
                                                , dropTrailingPathSeparator
                                                , splitPath
                                                , joinPath
                                                )
import qualified Webbi.Utils.RoseTree          as RT
import           Debug.Trace
import           Webbi.Utils.ListZipper

import           System.FilePath                ( splitPath )
import           Data.Maybe
import qualified Text.Blaze.Html5              as H

import qualified Webbi.Utils.TreeZipper2       as TZ
import qualified Webbi.Utils.RoseTree          as R
import qualified Webbi.Utils.Trie              as T


import qualified Data.Map                      as M

import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Webbi.Utils.Free              as F

import qualified Data.DList                    as D


data Menu = Menu (TZ.TreeZipper FilePath)
    deriving (Show)


fromTreeZipper :: TZ.TreeZipper FilePath -> Menu
fromTreeZipper = Menu


style :: H.AttributeValue
style = "menu"


showHeader :: H.Html -> H.Html
showHeader nav = H.header ! A.class_ "header" $ nav


showMenu :: Menu -> H.Html
showMenu (Menu tz) =
    showHeader $ H.nav ! A.class_ style $ mapM_ showItems $ collect tz


collect
    :: TZ.TreeZipper FilePath -> D.DList (ListZipper (TZ.TreeZipper FilePath))
collect tz = case TZ.up tz of
    Nothing  -> D.singleton (makeRoot tz)
    Just tz' -> D.snoc (collect tz') (makeLevel tz)


makeLevel :: TZ.TreeZipper FilePath -> ListZipper (TZ.TreeZipper FilePath)
makeLevel tz = TZ.siblings tz


makeRoot :: TZ.TreeZipper FilePath -> ListZipper (TZ.TreeZipper FilePath)
makeRoot (TZ.TreeZipper x _ ls rs) = fmap TZ.fromRoseTree $ ListZipper ls x rs


showItems :: ListZipper (TZ.TreeZipper String) -> H.Html
showItems (ListZipper ls x rs) = H.ul ! A.class_ (style <> "-level") $ level
  where
    itemStyle = style <> "-link"
    lss       = fmap (itemStyle, ) ls
    rss       = fmap (itemStyle, ) rs
    xx        = (itemStyle <> "-selection", x)
    items = filter (\x -> TZ.datum (snd x) /= "index.html") $ lss ++ (xx : rss)
    level     = foldMap showItem items


showItem :: (H.AttributeValue, TZ.TreeZipper String) -> H.Html
showItem (itemStyle, tz) = item
  where
    link = F.link ("/" ++ (foldl (++) "" (TZ.path tz)))
    text =
        fmap toUpper $ dropTrailingPathSeparator $ RT.datum $ TZ.toRoseTree tz
    item =
        H.li
            ! A.class_ (style <> "-item")
            $ H.a
            ! A.class_ itemStyle
            ! A.href (fromString link)
            $ (H.toHtml text)


