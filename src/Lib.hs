module Lib
    ( compileCss
    , compileMenu
    , compileContent
    , compileTemplates
    )
where

import Debug.Trace
import Data.Maybe

import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html.Renderer.String
                                                ( renderHtml )
import           System.FilePath                ( splitPath, (</>))

import           Hakyll

import           Webbi.Menu                     ( Menu(..) )
import qualified Webbi.Utils.RoseTree          as RT

import qualified Webbi.Menu                    as M


compileCss :: Rules ()
compileCss = match "**/css/*.hs" $ do
    route $ setExtension "css"
    compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])


compileContent :: Rules ()
compileContent = do
    compileMenu
    compilePosts


content :: Pattern
content = "posts/*"


compileMenu :: Rules ()
compileMenu = match content $ do
    version "menu" $ compile $ do
        item  <- setVersion Nothing <$> getUnderlying
        route <- getRoute item
        case route of
            Nothing -> noResult "No menu item"
            Just r  -> makeItem r


compilePosts :: Rules ()
compilePosts = match content $ do
    route $ setExtension "html"
    compile $ do
        ctx <- postContext
        pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

postContext :: Compiler (Context String)
postContext = do
    menu <- getMenu
    return $ constField "menu" menu <> defaultContext

getMenu :: Compiler String
getMenu = do
    menu  <- makeMenu <$> loadAll (fromVersion $ Just "menu")

    route <- getRoute =<< getUnderlying

    let m = findRoute (fromJust route) menu
    --
    -- find route in tree?
    --
    case route of
        Nothing -> noResult "No current route"
        Just r  -> return $ renderHtml $ showMenu menu


findRoute :: FilePath -> Menu -> Menu
findRoute route menu = find routes menu
    where
        routes = splitPath route
        find [] m = m
        find (x:[]) (Menu m) = find [] (Menu (fromJust (RT.down (Right x) m)))
        find (x:xs) (Menu m) = find xs (Menu (fromJust (RT.down (Left x) m)))



makeMenu :: [Item FilePath] -> Menu
makeMenu items = M.fromTrie $ foldl (\acc m -> M.insert m acc) M.empty paths
    where paths = fmap (splitPath . itemBody) items


showMenu :: Menu -> H.Html
showMenu (Menu m) = case m of
    (RT.TreeZipper (RT.Leaf x     ) []             ) -> H.p ""
    (RT.TreeZipper (RT.Branch _ xs) []             ) -> H.p ""
    (RT.TreeZipper (RT.Leaf x     ) (RT.Context _ _ _:_)) -> H.p ""
    (RT.TreeZipper (RT.Branch _ xs) (RT.Context _ _ _:_)) -> H.p ""


compileTemplates :: Rules ()
compileTemplates = match "templates/*" $ compile templateBodyCompiler
