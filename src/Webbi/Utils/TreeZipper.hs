module Webbi.Utils.TreeZipper where

import Webbi.Utils.Trie (Trie)
import Webbi.Utils.RoseTree as RT

import Data.Maybe
import Data.String

import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A


data Context a = Context [RoseTree a] a [RoseTree a]
    deriving (Show, Eq, Ord)
    deriving (Functor)


data TreeZipper a = TreeZipper (RoseTree a) [Context a]
    deriving (Show, Eq, Ord)
    deriving (Functor)


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


lefts :: TreeZipper a -> [RoseTree a]
lefts (TreeZipper _ []) = []
lefts (TreeZipper item ((Context ls x rs):bs)) = ls


rights :: TreeZipper a -> [RoseTree a]
rights (TreeZipper _ []) = []
rights (TreeZipper item ((Context ls x rs):bs)) = rs


path :: TreeZipper String -> String
path (TreeZipper rt []) = datum rt
path tz = case up tz of
            Nothing -> (datum (toRoseTree tz))
            Just tz' -> path tz' ++ (datum (toRoseTree tz))


name :: TreeZipper String -> String
name = datum . toRoseTree


showItem :: H.AttributeValue -> TreeZipper String -> H.Html
showItem color tz = H.a ! A.style color ! A.href (fromString link) $ H.toHtml text
    where link = path tz
          text = name tz


showItems :: H.AttributeValue -> [TreeZipper String] -> H.Html
showItems color xs = mapM_ (showItem color) xs


showChildren :: TreeZipper String -> H.Html
showChildren x = showItems "background: white" (catMaybes asTrees)
    where childrens= children (toRoseTree x)
          asTrees = fmap (\v -> down (datum v) x) childrens


showLevel :: H.AttributeValue -> TreeZipper String -> H.Html
showLevel color tz = do
    let upp = fromJust $ up tz
    let ls' = catMaybes $ fmap (\v -> down (datum v) upp) (lefts tz)
    let rs' = catMaybes $ fmap (\v -> down (datum v) upp) (rights tz)
    showItems "background: white" ls'
    showItem color tz
    showItems "background: white" rs'

showHierachy :: TreeZipper String -> H.Html
showHierachy m = do
    case up m of
        Nothing -> return ()
        Just parent ->
                showHierachy parent
    H.div $ showLevel "background: orange" m

showMenu :: TreeZipper String -> H.Html
showMenu s = do
    showHierachy s
    H.div $ showChildren s
