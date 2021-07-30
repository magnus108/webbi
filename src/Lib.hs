module Lib
    ( compileCss
    , compileMenu
    , compileContent
    , compileTemplates
    )
where

import           Debug.Trace
import           Data.Maybe

import           Data.Either.Extra
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Blaze.Html.Renderer.String
                                                ( renderHtml )
import           System.FilePath                ( splitPath
                                                , dropFileName
                                                , takeBaseName
                                                )

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
content = "**.md"


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

    -- maybe find or compile indexfile for given route? if not exists
    route <- getRoute =<< getUnderlying

    --
    -- find route in tree?
    --
    let m = findRoute (fromJust route) menu

    traceShowM m

    case route of
        Nothing -> noResult "No current route"
        Just r  -> return $ renderHtml $ showMenu m



findRoute :: FilePath -> Menu -> Menu
findRoute route menu = find routes menu
  where
    routes = splitPath route
    find [] m = m
    find ["index.html"] m = m
    find (x : []) (Menu m) = find [] (Menu (fromJust (RT.down (Right x) m)))
    find (x : xs) (Menu m) = find xs (Menu (fromJust (RT.down (Left x) m)))



makeMenu :: [Item FilePath] -> Menu
makeMenu items = M.fromTrie (traceShow trie trie)
    where paths = fmap itemBody items
          trie = M.fromList paths


showMenu :: Menu -> H.Html
showMenu (Menu m) = showIt m
  where
    showIt i = case i of
        (RT.TreeZipper (RT.Leaf x) []) -> H.p (H.toHtml x)
        (RT.TreeZipper (RT.Branch x xs) []) ->
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
        tree@(RT.TreeZipper (RT.Branch x _) (RT.Context ls p rs : _)) ->
            H.div ! A.style "background: red" $ do
                showIt $ fromJust (RT.up tree)
                mapM_ (H.p . H.toHtml . fromEither . RT.datum) ls
                H.p (H.toHtml x)
                mapM_ (H.p . H.toHtml . fromEither . RT.datum) rs


compileTemplates :: Rules ()
compileTemplates = match "templates/*" $ compile templateBodyCompiler
