module Webbi.Utils.TreeZipper where

--- OVERVEJ TEST AF ALLE METHODER!??
--- OVERVEJ TEST AF ALLE METHODER!??
--- OVERVEJ TEST AF ALLE METHODER!??
--- OVERVEJ TEST AF ALLE METHODER!??
--- OVERVEJ TEST AF ALLE METHODER!??

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


data TreeZipper a = Root [RoseTree a]
                  | Tree (RoseTree a) [Context a] [RoseTree a] [RoseTree a]
    deriving (Show, Eq, Ord)
    deriving (Functor)


fromRoseTree :: RoseTree a -> TreeZipper a
fromRoseTree x = Tree x [] [] []

toRoseTree :: TreeZipper a -> Maybe (RoseTree a)
toRoseTree (Root _ ) = Nothing
toRoseTree (Tree item _ _ _) = Just item


toContext :: TreeZipper a -> Maybe (Context a)
toContext (Root _ ) = Nothing
toContext (Tree _ (x:_) _ _) = Just x


fromForest :: Forest String -> TreeZipper String
fromForest (Forest xs) = Root xs


fromList :: FilePath -> [FilePath] -> TreeZipper FilePath
fromList path = navigateTo path . fromForest . RT.fromTrie . T.fromList T.insert . fmap splitPath


down :: Eq a => a -> TreeZipper a -> Maybe (TreeZipper a)
down x (Root rts) =
    let
        (ls, rs) = break (\item -> RT.datum item == x) rts
    in
        case rs of
            y:ys -> Just (Tree y [] ls ys)
            _ -> Nothing
down x (Tree rt bs lss rss) =
    let
        (ls, rs) = break (\item -> RT.datum item == x) (RT.children rt)
    in
        case rs of
            y:ys -> Just (Tree y (Context ls (RT.datum rt) ys:(bs)) lss rss)
            _ -> Nothing


up :: TreeZipper a -> Maybe (TreeZipper a)
up (Root _) = Nothing
up (Tree item [] lss rss) = Just $ Root $ lss ++ (item : rss)
up (Tree item ((Context ls x rs):bs) lss rss) =
    Just (Tree (RT.RoseTree x (ls <> [item] <> rs)) bs lss rss)


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
children r@(Root xs) = catMaybes $ fmap (\x -> down (RT.datum x) r) xs
children tz =
    let
        child = firstChild tz
    in
        case child of
               Nothing -> []
               Just child' ->  child' : (rights child')

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
firstChild (Root (x:xs)) = Just (Tree x [] [] xs)
firstChild tz = firstChild' =<< toRoseTree tz
    where
        firstChild' x = case RT.children x of
            [] -> Nothing
            c : cs -> down (RT.datum c) tz


rights' :: TreeZipper a -> [RoseTree a]
rights' (Root _) = []
rights' (Tree _ [] _ rs) = rs
rights' (Tree item ((Context ls x rs):bs) _ _) = rs


lefts' :: TreeZipper a -> [RoseTree a]
lefts' (Root _) = []
lefts' (Tree _ [] ls _) = ls
lefts' (Tree item ((Context ls x rs):bs) _ _) = reverse ls


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


path :: TreeZipper String -> Maybe String
path (Root _) = Nothing
path (Tree rt [] _ _) = Just (RT.datum rt)
path tz = case up tz of
            Nothing -> (fmap RT.datum (toRoseTree tz))
            Just tz' -> liftM2 (\x y -> x ++ y) (path tz') (fmap RT.datum (toRoseTree tz))




















showChildren :: TreeZipper String -> H.Html
showChildren tz = items'
    where
        items = filterM (\x -> (\y -> y /= "index.html") <$> (takeFileName  <$> (RT.datum <$> (toRoseTree x)))) $ children tz
        items' = case items of
                    Nothing -> H.div $ "bad"
                    Just i -> H.ul $ showItems "background: white" i


showLevel :: H.AttributeValue -> TreeZipper String -> H.Html
showLevel color tz = H.ul $ do
    let mLefts = filterM (\x -> (\y -> y /= "index.html") <$> (takeFileName <$> (RT.datum <$> (toRoseTree x)))) (lefts tz)
    _ <- case mLefts of
            Nothing -> H.div $ "bad"
            Just i -> showItems "background: cyan" i
    let b = ((\x -> x /= "index.html") <$> (takeFileName  <$> (RT.datum <$> (toRoseTree tz))))
    case b of
        Nothing -> H.div $ "bad"
        Just b -> if b then showItem color tz else H.div "bad"
    mapM_ (showItems "background: green") $ filterM (\x -> (\y -> y /= "index.html") <$> takeFileName  <$> (RT.datum <$> (toRoseTree x))) (rights tz)

showHierachy :: TreeZipper String -> H.Html
showHierachy m = do
    case up m of
        Nothing -> return ()
        Just parent -> showHierachy parent
    showLevel "background: orange" m






showItem :: H.AttributeValue -> TreeZipper String -> H.Html
showItem color tz = case item of
                      Nothing -> H.div $ "bad"
                      Just x -> x
    where link = path tz
          text' = F.title <$> (RT.datum <$> (toRoseTree tz))
          children' = children tz
          text = if length children' == 1 then dropTrailingPathSeparator <$> text' else text'
          item = liftM2 (\l t -> H.li $ H.a ! A.style color ! A.href (fromString l) $ (H.toHtml t)) link text

showItems :: H.AttributeValue -> [TreeZipper String] -> H.Html
showItems color xs = mapM_ (showItem color) xs







showItem' :: H.AttributeValue -> TreeZipper String -> Maybe H.Html
showItem' color tz = item
    where link = (\x -> "/" ++ x) <$> path tz
          text = F.title <$> dropTrailingPathSeparator <$> RT.datum <$> toRoseTree tz
          item = (\l t -> H.a ! A.style color ! A.href (fromString l) $ (H.toHtml t)) <$> link <*> text


showItems' :: H.AttributeValue -> [TreeZipper String] -> H.Html
showItems' color xs = fromMaybe (H.div "lol") content
    where
        content = fmap (H.ul . F.foldMapM H.li) $ sequence $ fmap (showItem' color) xs


showsTop :: TreeZipper String -> H.Html
showsTop (Tree x _ ls rs) = do
    showItems' "background: cyan" $ fmap fromRoseTree $ x : ls ++ rs


showChildren' :: TreeZipper String -> H.Html
showChildren' tz = items'
    where
        items = filterM (\x -> (\y -> y /= "index.html") <$> (takeFileName  <$> (RT.datum <$> (toRoseTree x)))) $ (traceShow (children tz) (children tz))
        items' = case items of
                    Nothing -> H.div $ "bad"
                    Just i -> H.ul $ showItems' "background: white" i



showMenu :: TreeZipper String -> H.Html
showMenu tz = do
    --showsTop tz
--    showHierachy tz
    showChildren' tz
