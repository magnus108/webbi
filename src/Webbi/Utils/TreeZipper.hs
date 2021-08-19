module Webbi.Utils.TreeZipper where

import Webbi.Utils.Trie (Trie)
import Webbi.Utils.RoseTree as RT

import Data.Maybe
import Data.String

import Control.Comonad

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


down :: Eq a => a -> TreeZipper a -> Maybe (TreeZipper a)
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



rights :: Eq a => TreeZipper a -> [TreeZipper a]
rights tz =
    let
        next = nextSibling tz
    in
        case next of
               Nothing -> []
               Just next' -> next' : (rights next')

lefts :: Eq a => TreeZipper a -> [TreeZipper a]
lefts tz =
    let
        prev = previousSibling tz
    in
        case prev of
               Nothing -> []
               Just prev' -> prev' : (lefts prev')


childs :: Eq a => TreeZipper a -> [TreeZipper a]
childs tz =
    let
        child = firstChild tz
    in
        case child of
               Nothing -> []
               Just child' -> child' : rights child'

forward :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
forward zipper =
    firstOf [ firstChild, nextSibling, nextSiblingOfAncestor ] zipper

firstOf :: [(a -> Maybe b)] -> a -> Maybe b
firstOf options v =
    case options of
        [] -> Nothing
        option : rest ->
            case option v of
                Just r -> Just r
                Nothing -> firstOf rest v


firstChild :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
firstChild tz =
    case children (toRoseTree tz) of
        [] -> Nothing
        c : cs -> down (datum c) tz


rights' :: TreeZipper a -> [RoseTree a]
rights' (TreeZipper _ []) = []
rights' (TreeZipper item ((Context ls x rs):bs)) = rs


lefts' :: TreeZipper a -> [RoseTree a]
lefts' (TreeZipper _ []) = []
lefts' (TreeZipper item ((Context ls x rs):bs)) = ls


nextSibling :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
nextSibling tz =
    case rights' tz of
        [] -> Nothing
        next : rest ->
            down (datum next) =<< up tz


previousSibling :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
previousSibling tz =
    case lefts' tz of
        [] -> Nothing
        prev : rest ->
            down (datum prev) =<< up tz


nextSiblingOfAncestor :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
nextSiblingOfAncestor tz =
    case up tz of
      Nothing -> Nothing
      Just p ->
          case nextSibling p of
            Nothing ->
                nextSiblingOfAncestor p
            Just s ->
                Just s


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
showChildren = showItems "background: white" . childs


showLevel :: H.AttributeValue -> TreeZipper String -> H.Html
showLevel color tz = do
    showItems "background: blue" (lefts tz)
    showItem color tz
    showItems "background: green" (rights tz)


showHierachy :: TreeZipper String -> H.Html
showHierachy m = do
    case up m of
        Nothing -> return ()
        Just parent -> showHierachy parent
    H.div $ showLevel "background: orange" m


showMenu :: TreeZipper String -> H.Html
showMenu tz = do
    showHierachy tz
    H.div $ showChildren tz
