module Webbi.Menu
    ( Menu(..)
    , showMenu
    , fromTreeZipper
    )
where

import           Data.String
import           System.FilePath                ( takeFileName
                                                , takeExtension
                                                , dropTrailingPathSeparator
                                                , splitPath
                                                , joinPath
                                                )
import qualified Webbi.Utils.RoseTree          as RT
import Debug.Trace
import Webbi.Utils.ListZipper

import           System.FilePath                ( splitPath )
import           Data.Maybe
import qualified Text.Blaze.Html5              as H

import qualified Webbi.Utils.TreeZipper        as TZ
import qualified Webbi.Utils.RoseTree          as R
import qualified Webbi.Utils.Trie              as T


import qualified Data.Map                      as M

import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Webbi.Utils.Free              as F


data Menu = Menu (TZ.TreeZipper FilePath)
    deriving (Show)


fromTreeZipper :: TZ.TreeZipper FilePath -> Menu
fromTreeZipper  = Menu



style :: H.AttributeValue
style = "menu"

showMenu :: Menu -> H.Html
showMenu (Menu tz) = do
    let h = TZ.hierachy tz
    H.header ! A.class_ "header" $ H.nav ! A.class_ style $ mapM_ showItems' h



showItem' :: H.AttributeValue -> TZ.TreeZipper String -> Maybe H.Html
showItem' _ (TZ.Tree (RT.RoseTree _ []) _ _ _) = Nothing
showItem' itemStyle tz                              = item
  where
    link = F.link ("/" ++ (foldl (++) "" (TZ.path tz)))
    text = F.title <$> dropTrailingPathSeparator <$> RT.datum <$> TZ.toRoseTree tz
    text' = (takeFileName (joinPath (traceShow (TZ.path tz) (TZ.path tz))))
    item =
        (\l t -> H.a ! A.class_ (itemStyle) ! A.href (fromString l) $ (H.toHtml t))
            <$> (Just link)
            <*> (text)


showItems' :: ListZipper (TZ.TreeZipper String) -> H.Html
showItems' (ListZipper ls x xs) = content
  where
    itemStyle = style <> "-link"
    lss = fmap (showItem' itemStyle) ls
    xx = (showItem' (itemStyle <> "-selection") x)
    rss = fmap (showItem' itemStyle) xs
    content = ((H.ul ! A.class_ (style <> "-level")) . F.foldMapM (H.li ! A.class_ (style <> "-item"))) $ catMaybes $ (lss ++ (xx : rss))
