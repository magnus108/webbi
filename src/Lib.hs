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
import           Text.Blaze.Html.Renderer.String
                                                ( renderHtml )
import           System.FilePath                ( splitPath
                                                , dropFileName
                                                , takeBaseName
                                                , replaceFileName
                                                , takeDirectory
                                                , (</>)
                                                )

import           Hakyll

import           Webbi.Menu                     ( Menu(..) )
import qualified Webbi.Utils.RoseTree          as RT
import qualified Webbi.Utils.TreeZipper        as TZ

import qualified Webbi.Menu                    as M

import Data.String
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A


styles :: Pattern
styles = "**css/*.hs"

content :: Pattern
content = "**index.md"


compileCss :: Rules ()
compileCss = do
    compileClay
    compileStyles


compileClay :: Rules ()
compileClay = do
    match styles $ do
        route $ setExtension "css"
        compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])


compileStyles :: Rules ()
compileStyles = do
    match styles $ do
        version "css" $ compile $ do
            item  <- setVersion Nothing <$> getUnderlying
            route <- getRoute item
            case route of
                Nothing -> noResult "No css item"
                Just r  -> makeItem r


compileContent :: Rules ()
compileContent = do
    compileMenu
    compileMarkdown


compileMenu :: Rules ()
compileMenu =
    match content $ do
        version "menu" $ compile $ do
            item  <- setVersion Nothing <$> getUnderlying
            route <- getRoute item
            case route of
                Nothing -> noResult "No menu item"
                Just r  -> makeItem r


compileMarkdown :: Rules ()
compileMarkdown = match content $ do
    route $ setExtension "html"
    compile $ do
        ctx <- contentContext
        pandocCompiler
            >>= loadAndApplyTemplate "templates/content.html"    ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls


contentContext :: Compiler (Context String)
contentContext = do
    menu <- getMenu
    css <- getCss
    return $ constField "menu" menu <> constField "css" css <> defaultContext


getCss :: Compiler String
getCss = do
    css  <- M.fromList' <$> fmap itemBody <$> loadAll (fromVersion $ Just "css")
    route <- getRoute =<< getUnderlying

    case route of
        Nothing -> noResult "No current route"
        Just r  -> do
            traceShowM "SKAL IKKE VÆRE MENU"
            let (Menu m) = M.navigateToParent r css
            traceShowM (m)
            traceShowM (TZ.foldup m)
            return $ renderHtml $  mapM_ tolink (TZ.foldup m)

tolink x = H.link ! A.rel "stylesheet" ! A.href (fromString x)

getMenu :: Compiler String
getMenu = do
    menu  <- M.fromList <$> fmap itemBody <$> loadAll (fromVersion $ Just "menu")
    route <- getRoute =<< getUnderlying

    case route of
        Nothing -> noResult "No current route"
        Just r  -> do
            let m = M.navigateTo r menu
            return $ renderHtml $ M.showMenu m


compileTemplates :: Rules ()
compileTemplates = match "templates/*" $ compile templateBodyCompiler
