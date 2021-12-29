module Webbi.Utils.TreeZipper where


import           Webbi.Utils.ListZipper
import           Debug.Trace
import           Data.Char
import           Webbi.Utils.Trie               ( Trie )
import qualified Webbi.Utils.Trie              as T
import qualified Webbi.Utils.Free              as F
import           Webbi.Utils.RoseTree           ( RoseTree
                                                , Forest(..)
                                                )
import qualified Webbi.Utils.RoseTree          as RT

import           Data.Maybe
import           Data.String

import           Control.Monad
import           Control.Comonad


import           System.FilePath                ( takeFileName
                                                , takeExtension
                                                , dropTrailingPathSeparator
                                                , splitPath
                                                , joinPath
                                                )

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
toRoseTree (Root _         ) = Nothing
toRoseTree (Tree item _ _ _) = Just item


toContext :: TreeZipper a -> Maybe (Context a)
toContext (Root _            ) = Nothing
toContext (Tree _ (x : _) _ _) = Just x


toContexts :: TreeZipper a -> [Context a]
toContexts (Root _       ) = []
toContexts (Tree _ xs _ _) = xs


fromForest :: Forest a -> TreeZipper a
fromForest (Forest xs) = Root xs


fromList :: FilePath -> [FilePath] -> TreeZipper FilePath
fromList path =
    navigateTo route
        . fromForest
        . RT.fromTrie
        . T.fromList T.insert
        . fmap splitPath
    where route = splitPath path


down :: Eq a => a -> TreeZipper a -> Maybe (TreeZipper a)
down x (Root rts) =
    let (ls, rs) = break (\item -> RT.datum item == x) rts
    in  case rs of
            y : ys -> Just (Tree y [] ls ys)
            _      -> Nothing
down x (Tree rt bs lss rss) =
    let (ls, rs) = break (\item -> RT.datum item == x) (RT.children rt)
    in  case rs of
            y : ys ->
                Just (Tree y (Context ls (RT.datum rt) ys : (bs)) lss rss)
            _ -> Nothing


up :: TreeZipper a -> Maybe (TreeZipper a)
up (Root _              ) = Nothing
up (Tree item [] lss rss) = Just $ Root $ lss ++ (item : rss)
up (Tree item ((Context ls x rs) : bs) lss rss) =
    Just (Tree (RT.RoseTree x (ls <> [item] <> rs)) bs lss rss)


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
children r@(Root xs) = catMaybes $ fmap (\x -> down (RT.datum x) r) xs
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
firstChild (Root (x : xs)) = Just (Tree x [] [] xs)
firstChild tz              = firstChild' =<< toRoseTree tz
  where
    firstChild' x = case RT.children x of
        []     -> Nothing
        c : cs -> down (RT.datum c) tz


rights' :: TreeZipper a -> [RoseTree a]
rights' (Root _) = []
rights' (Tree _ [] _ rs) = rs
rights' (Tree item ((Context ls x rs) : bs) _ _) = rs


lefts' :: TreeZipper a -> [RoseTree a]
lefts' (Root _) = []
lefts' (Tree _ [] ls _) = reverse ls
lefts' (Tree item ((Context ls x rs) : bs) _ _) = reverse ls


nextSibling :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
nextSibling tz = case rights' tz of
    []          -> Nothing
    next : rest -> down (RT.datum next) =<< up tz


previousSibling :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
previousSibling tz = case lefts' tz of
    []          -> Nothing
    prev : rest -> down (RT.datum prev) =<< up tz


nextSiblingOfAncestor :: Eq a => TreeZipper a -> Maybe (TreeZipper a)
nextSiblingOfAncestor tz = case up tz of
    Nothing -> Nothing
    Just p  -> case nextSibling p of
        Nothing -> nextSiblingOfAncestor p
        Just s  -> Just s


navigateTo :: (Eq a) => [a] -> TreeZipper a -> TreeZipper a
navigateTo routes item = find routes item
  where
    find []       m = m
    find (x : xs) m = case (down x m) of
        Nothing -> m
        Just y  -> find xs y


navigateTo' :: (Eq a) => [a] -> TreeZipper a -> Maybe (TreeZipper a)
navigateTo' routes item = find routes item
  where
    find []       m = Just m
    find (x : xs) m = case (down x m) of
        Nothing -> Nothing
        Just y  -> find xs y



path :: TreeZipper String -> [String]
path (Root _) = []
path tz       = case up tz of
    Nothing  -> datum tz
    Just tz' -> F.myZip (++) (path tz') (datum tz)


datum :: TreeZipper a -> [a]
datum tz = fmap RT.datum (maybeToList (toRoseTree tz))


siblings :: Eq a => TreeZipper a -> ListZipper (TreeZipper a)
siblings tz = ListZipper (lefts tz) tz (rights tz)


hierachy :: (Eq a) => TreeZipper a -> [ListZipper (TreeZipper a)]
hierachy m = hierachy' m []
  where
    hierachy' x xs =
        let level = case siblings x of
                (ListZipper [] x []) -> xs
                ys                   -> (ys : xs)
        in  case up x of
                Nothing     -> level
                Just parent -> hierachy' parent level


parents :: TreeZipper a -> [TreeZipper a]
parents tz = tz : case up tz of
    Nothing -> []
    Just p  -> parents p
