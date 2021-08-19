module Webbi.Utils.TreeZipper where

import Webbi.Utils.Trie (Trie)
import Webbi.Utils.RoseTree as RT

import Data.String
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A


data Context a = Context [RoseTree a] a [RoseTree a]
    deriving (Show, Eq, Ord)


data TreeZipper a = TreeZipper (RoseTree a) [Context a]
    deriving (Show, Eq, Ord)


toRoseTree :: TreeZipper a -> RoseTree a
toRoseTree (TreeZipper item _) = item


toContext :: TreeZipper a -> Maybe (Context a)
toContext (TreeZipper _ []) = Nothing
toContext (TreeZipper _ (x:xs)) = Just x


fromRoseTree :: RoseTree a -> TreeZipper a
fromRoseTree x = TreeZipper x []


fromTrie :: String -> Trie String -> TreeZipper String
fromTrie root trie = fromRoseTree (RT.fromTrie root trie)


down :: (Show a, Eq a) => a -> TreeZipper a -> Maybe (TreeZipper a)
down x (TreeZipper (RoseTree parent items) bs) =
    let
        (ls, rs) = break (\item -> datum item == x) items
    in
        case rs of
            y:ys -> Just (TreeZipper y (Context ls parent ys:bs))
            _ -> Nothing


up :: TreeZipper a -> Maybe (TreeZipper a)
up (TreeZipper _ []) = Nothing
up (TreeZipper item ((Context ls x rs):bs)) =
    Just (TreeZipper (RoseTree x (ls <> [item] <> rs)) bs)


showItem :: H.AttributeValue -> RoseTree String -> H.Html
showItem color rt = H.a ! A.style color ! A.href (fromString link) $ H.toHtml link
    where link = datum rt

showItems :: H.AttributeValue -> [RoseTree String] -> H.Html
showItems color xs = mapM_ (showItem color) xs

showChildren :: TreeZipper String -> H.Html
showChildren = showItems "background: white" . children . toRoseTree

showLevel :: H.AttributeValue -> TreeZipper String -> H.Html
showLevel color (TreeZipper rt []) = do
    showItem "background: gold" rt
showLevel color (TreeZipper rt ((Context ls p rs):bs)) = do
    showItems "background: white" ls
    showItem color rt
    showItems "background: white" rs

showHierachy :: TreeZipper String -> H.Html
showHierachy m = do
    case up m of
        Nothing -> return ()
        Just parent ->
                showHierachy parent
    showLevel "background: blue" m

showMenu :: TreeZipper String -> H.Html
showMenu s = do
    showHierachy s
    showChildren s
