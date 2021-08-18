module Webbi.Menu
    ( Menu(..)
    , fromList
    , navigateTo
    , showMenu
    )
where

import           System.FilePath                ( splitPath )
import           Data.Maybe
import qualified Text.Blaze.Html5              as H

import qualified Webbi.Utils.TreeZipper        as TZ
import qualified Webbi.Utils.RoseTree          as R
import qualified Webbi.Utils.Trie              as T


import qualified Data.Map                      as M


data Menu = Menu (TZ.TreeZipper FilePath)
    deriving (Show)


insert :: [String] -> T.Trie String -> T.Trie String
insert [] (T.Trie _ m) = T.Trie True m
insert ["index.html"] (T.Trie _ m) = T.Trie True m
insert (x:xs) (T.Trie b m) = T.Trie b $ M.insertWith (<>) x (insert xs T.empty) m


fromList :: [FilePath] -> Menu
fromList = Menu . TZ.fromTrie "/" . T.fromList insert . fmap splitPath


navigateTo :: FilePath -> Menu -> Menu
navigateTo route menu = find routes menu
  where
    routes = splitPath route
    find [] m = m
    find ["index.html"] m = m
    find (x : xs) (Menu m) = find xs (Menu (fromJust (TZ.down x m)))


showMenu :: Menu -> H.Html
showMenu (Menu m) = showIt m
    where
        showIt (TZ.TreeZipper x []) = H.p (R.showIt x)
        showIt (TZ.TreeZipper x xs) = do
            mapM_ TZ.showIt xs
            H.p (R.showIt x)


            {-
  where
    showcontext ctx = undefined
    showIt i = case i of
        (RT.TreeZipper (RT.Leaf x) []) -> H.p (H.toHtml x)
        (RT.TreeZipper (RT.Branch x xs) []) -> do
            H.p (H.toHtml x)
            mapM_ (H.p . H.toHtml . fromEither . RT.datum) xs
        tree@(RT.TreeZipper (RT.Leaf x) (RT.Context ls p rs : more)) -> do
            showIt $ fromJust (RT.up tree)
            mapM_ (H.p . H.toHtml . fromEither . RT.datum) (filter (\ii -> fromEither (RT.datum ii) /= "index.html" )ls)
            H.p (H.toHtml x)
            mapM_ (H.p . H.toHtml . fromEither . RT.datum) (filter (\ii -> fromEither (RT.datum ii) /= "index.html" )rs)
                                                                    {-
            tree@(RT.TreeZipper (RT.Branch x _) (RT.Context _ _ _:[])) -> 
                                                                H.div $ do
                                                                        showIt $ fromJust (RT.up tree)
                                                                        -}
        --overvej om der er behov for at kunne skelne mellem flere slags branch/leaf
        --måske folder/nonfolder/file
        tree@(RT.TreeZipper (RT.Branch x xs) (RT.Context ls p rs : _)) ->
            H.div ! A.style "background: red" $ do
                H.p (H.toHtml p) ! A.style "background: yellow"
                H.div ! A.style "background: orange" $ do
                    mapM_ (H.p . H.toHtml . fromEither . RT.datum) ls
                H.p (H.toHtml x) ! A.style "background: blue"
                H.div ! A.style "background: green" $ do
                    mapM_ (H.p . H.toHtml . fromEither . RT.datum) rs
                H.div ! A.style "background: purple" $ do
                    mapM_ (H.p . H.toHtml . fromEither . RT.datum) xs
-}
                    {-
                mapM_ (H.p . H.toHtml . fromEither . RT.datum) ls
                H.p (H.toHtml x)
                mapM_ (H.p . H.toHtml . fromEither . RT.datum) rs
                -- kan ikke vise børn de skal jo vises under
                mapM_ (H.p . H.toHtml . fromEither . RT.datum) xs

    -}
