module Webbi.Utils.TreeZipper2 where

import           Webbi.Utils.RoseTree           ( RoseTree
                                                , Forest(..)
                                                )
import qualified Webbi.Utils.Free              as F
import           Webbi.Utils.ListZipper
import qualified Webbi.Utils.RoseTree          as RT
import qualified Webbi.Utils.Trie              as T
import           System.FilePath                ( splitPath )
import           Data.Maybe


data Context a = Context [RoseTree a] a [RoseTree a]
    deriving (Show, Eq, Ord)
    deriving (Functor)


data TreeZipper a = TreeZipper (RoseTree a) [Context a] [RoseTree a] [RoseTree a]
    deriving (Show, Eq, Ord)
    deriving (Functor)


fromRoseTree :: RoseTree a -> TreeZipper a
fromRoseTree x = TreeZipper x [] [] []


toRoseTree :: TreeZipper a -> RoseTree a
toRoseTree (TreeZipper item _ _ _) = item


toContext :: TreeZipper a -> Context a
toContext (TreeZipper _ (x : _) _ _) = x


toContexts :: TreeZipper a -> [Context a]
toContexts (TreeZipper _ xs _ _) = xs


fromList :: FilePath -> [FilePath] -> TreeZipper FilePath
fromList path =
    navigateTo' route . RT.fromTrie . T.fromList T.insert . fmap splitPath
    where route = splitPath path


down :: Eq a => a -> TreeZipper a -> Maybe (TreeZipper a)
down x (TreeZipper rt bs lss rss) =
    let (ls, rs) = break (\item -> RT.datum item == x) (RT.children rt)
    in  case rs of
            y : ys ->
                Just (TreeZipper y (Context ls (RT.datum rt) ys : (bs)) lss rss)
            _ -> Nothing


up :: TreeZipper a -> Maybe (TreeZipper a)
up (TreeZipper item [] lss rss) = Nothing
up (TreeZipper item ((Context ls x rs) : bs) lss rss) =
    Just (TreeZipper (RT.RoseTree x (ls <> [item] <> rs)) bs lss rss)


rights :: Eq a => TreeZipper a -> [TreeZipper a]
rights tz =
    let next = nextSibling tz
    in  case next of
            Nothing    -> []
            Just next' -> next' : (rights next')


lefts :: Eq a => TreeZipper a -> [TreeZipper a]
lefts tz =
    let prev = previousSibling tz
    in  case prev of
            Nothing    -> []
            Just prev' -> lefts prev' ++ [prev']


leafs :: Eq a => TreeZipper a -> [TreeZipper a]
leafs = filter (isNothing . firstChild) . children


children :: Eq a => TreeZipper a -> [TreeZipper a]
children tz =
    let child = firstChild tz
    in  case child of
            Nothing     -> []
            Just child' -> child' : (rights child')


forward :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
forward zipper =
    firstOf [firstChild, nextSibling, nextSiblingOfAncestor] zipper


firstOf :: [(a -> Maybe b)] -> a -> Maybe b
firstOf options v = case options of
    []            -> Nothing
    option : rest -> case option v of
        Just r  -> Just r
        Nothing -> firstOf rest v


firstChild :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
firstChild tz = firstChild' (toRoseTree tz)
  where
    firstChild' x = case RT.children x of
        []     -> Nothing
        c : cs -> down (RT.datum c) tz


rights' :: TreeZipper a -> [RoseTree a]
rights' (TreeZipper _    []                       _ rs) = rs
rights' (TreeZipper item ((Context ls x rs) : bs) _ _ ) = rs


lefts' :: TreeZipper a -> [RoseTree a]
lefts' (TreeZipper _    []                       ls _) = reverse ls
lefts' (TreeZipper item ((Context ls x rs) : bs) _  _) = reverse ls


nextSibling :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
nextSibling tz = case rights' tz of
    []          -> Nothing
    next : rest -> down (RT.datum next) =<< up tz


previousSibling :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
previousSibling tz = case lefts' tz of
    []          -> Nothing
    prev : rest -> down (RT.datum prev) =<< up tz



fromForest :: (Eq a) => a -> Forest a -> Maybe (TreeZipper a)
fromForest x (Forest rts) =
    let (ls, rs) = break (\item -> RT.datum item == x) rts
    in  case rs of
            y : ys -> Just (TreeZipper y [] ls ys)
            _      -> Nothing


nextSiblingOfAncestor :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
nextSiblingOfAncestor tz = case up tz of
    Nothing -> Nothing
    Just p  -> case nextSibling p of
        Nothing -> nextSiblingOfAncestor p
        Just s  -> Just s


navigateTo :: (Eq a) => [a] -> Forest a -> Maybe (TreeZipper a)
navigateTo []           _  = Nothing
navigateTo (r : routes) fs = find routes =<< (fromForest r fs)
  where
    find []       m = Just m
    find (x : xs) m = case (down x m) of
        Nothing -> Nothing
        Just y  -> find xs y


datum :: TreeZipper a -> a
datum tz = RT.datum (toRoseTree tz)


siblings :: Eq a => TreeZipper a -> ListZipper (TreeZipper a)
siblings tz = ListZipper (lefts tz) tz (rights tz)


toForest :: TreeZipper a -> Forest a
toForest (TreeZipper x _ ls rs) = Forest (ls ++ x : rs)






fromForest' :: (Eq a) => a -> Forest a -> TreeZipper a
fromForest' x (Forest rts) =
    let (ls, rs) = break (\item -> RT.datum item == x) rts
    in  case rs of
            y : ys -> TreeZipper y [] ls ys
            _      -> TreeZipper (RT.RoseTree x []) [] ls rs -- wierd INDEX does go here.


navigateTo' :: (Eq a) => [a] -> Forest a -> TreeZipper a
navigateTo' (r : routes) fs = find routes (fromForest' r fs)
  where
    find []       m = m
    find (x : xs) m = case (down x m) of
        Nothing -> m
        Just y  -> find xs y


path :: TreeZipper String -> [String]
path tz = case up tz of
    Nothing  -> [datum tz]
    Just tz' -> F.myZip (++) (path tz') [datum tz]
