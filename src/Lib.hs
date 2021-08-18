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
                                                , (</>)
                                                )

import           Hakyll

import           Webbi.Menu                     ( Menu(..) )
import qualified Webbi.Utils.RoseTree          as RT
import qualified Webbi.Utils.TreeZipper        as TZ

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
    menu  <- M.fromList <$> fmap itemBody <$> loadAll (fromVersion $ Just "menu")
    route <- getRoute =<< getUnderlying

    case route of
        Nothing -> noResult "No current route"
        Just r  -> do
            let m = M.navigateTo r menu
            traceShowM "mmmmm"
            traceShowM r
            traceShowM m
            return $ renderHtml $ M.showMenu m


compileTemplates :: Rules ()
compileTemplates = match "templates/*" $ compile templateBodyCompiler
