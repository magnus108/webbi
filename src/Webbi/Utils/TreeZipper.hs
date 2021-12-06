module Webbi.Utils.TreeZipper where

import Debug.Trace
import           Data.Char
import Webbi.Utils.Trie (Trie)
import qualified Webbi.Utils.Trie as T
import qualified Webbi.Utils.Free as F
import Webbi.Utils.RoseTree (RoseTree,Forest(..))
import qualified Webbi.Utils.RoseTree as RT

import Data.Maybe
import Data.String

import Control.Monad
import Control.Comonad

import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

import           System.FilePath                ( takeFileName, takeExtension, dropTrailingPathSeparator, splitPath )


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


fromForest :: FilePath -> Forest String -> TreeZipper String
fromForest path (Forest xs) = fromRoseTree $ RT.RoseTree "/" xs


fromTrie :: FilePath -> Trie String -> TreeZipper String
fromTrie path trie = fromForest path (RT.fromTrie trie)


fromList :: FilePath -> [FilePath] -> TreeZipper FilePath
fromList path = navigateTo path . fromTrie path . T.fromList T.insert . fmap splitPath


down :: Eq a => a -> TreeZipper a -> Maybe (TreeZipper a)
down x (TreeZipper rt bs) =
    let
        (ls, rs) = break (\item -> RT.datum item == x) (RT.children rt)
    in
        case rs of
            y:ys -> Just (TreeZipper y (Context ls (RT.datum rt) ys:(bs)))
            _ -> Nothing



up :: TreeZipper a -> Maybe (TreeZipper a)
up (TreeZipper _ []) = Nothing
up (TreeZipper item ((Context ls x rs):bs)) =
    Just (TreeZipper (RT.RoseTree x (ls <> [item] <> rs)) bs)



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
               Just prev' -> lefts prev' ++ [prev']


leafs :: Eq a => TreeZipper a -> [TreeZipper a]
leafs = filter (isNothing . firstChild) . children

children :: Eq a => TreeZipper a -> [TreeZipper a]
children tz =
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
    case RT.children (toRoseTree tz) of
        [] -> Nothing
        c : cs -> down (RT.datum c) tz


rights' :: TreeZipper a -> [RoseTree a]
rights' (TreeZipper _ []) = []
rights' (TreeZipper item ((Context ls x rs):bs)) = rs


lefts' :: TreeZipper a -> [RoseTree a]
lefts' (TreeZipper _ []) = []
lefts' (TreeZipper item ((Context ls x rs):bs)) = reverse ls


nextSibling :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
nextSibling tz =
    case rights' tz of
        [] -> Nothing
        next : rest ->
            down (RT.datum next) =<< up tz


previousSibling :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
previousSibling tz =
    case lefts' tz of
        [] -> Nothing
        prev : rest ->
            down (RT.datum prev) =<< up tz


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


navigateToParent :: FilePath -> TreeZipper FilePath -> TreeZipper FilePath
navigateToParent route item = find routes item
  where
    routes = splitPath route
    find [] m = m
    find (x : []) m = m
    find (x : xs) m =
        case (down x m) of
            Nothing -> m
            Just y -> find xs y

navigateTo :: FilePath -> TreeZipper FilePath -> TreeZipper FilePath
navigateTo route item = find routes item
  where
    routes = splitPath route
    find [] m = m
    find (x : xs) m =
        case (down x m) of
            Nothing -> m
            Just y -> find xs y


path :: TreeZipper String -> String
path (TreeZipper rt []) = RT.datum rt
path tz = case up tz of
            Nothing -> (RT.datum (toRoseTree tz))
            Just tz' -> path tz' ++ (RT.datum (toRoseTree tz))



showItem :: H.AttributeValue -> TreeZipper String -> H.Html
showItem color tz = H.li $ H.a ! A.style color ! A.href (fromString link) $ H.toHtml text
    where link = path tz
          text' = F.title (RT.datum (toRoseTree tz))
          children' = children tz
          text = if length children' == 1 then dropTrailingPathSeparator text' else text'



showItems :: H.AttributeValue -> [TreeZipper String] -> H.Html
showItems color xs = mapM_ (showItem color) xs


showChildren :: TreeZipper String -> H.Html
showChildren = H.ul . showItems "background: white" . filter (\x -> takeFileName  (RT.datum (toRoseTree x)) /= "index.html") . children


showLevel :: H.AttributeValue -> TreeZipper String -> H.Html
showLevel color tz = H.ul $ do
    showItems "background: cyan" $ filter (\x -> takeFileName  (RT.datum (toRoseTree x)) /= "index.html") (lefts tz)
    let b = takeFileName  (RT.datum (toRoseTree tz)) /= "index.html"
    when b (showItem color tz)
    showItems "background: green" $filter (\x -> takeFileName  (RT.datum (toRoseTree x)) /= "index.html")  (rights tz)


showHierachy :: TreeZipper String -> H.Html
showHierachy m = do
    case up m of
        Nothing -> return ()
        Just parent -> showHierachy parent
    showLevel "background: orange" m


showMenu :: TreeZipper String -> H.Html
showMenu tz = do
    showHierachy tz
    showChildren tz
