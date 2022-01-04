module Lib
    ( compileCss
    , compileMenu
    , compileContent
    , compileTemplates
    , compileRobots
    , compileSitemap
    , compileAtom
    , compileImages
    , compileCV
    )
where
import Hakyll.Core.Compiler.Internal

import Text.Pandoc.UTF8 (toStringLazy, fromText, toString, toText)
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy as BL
import Data.Either
import qualified System.Process       as Process

import Text.Pandoc.PDF
import qualified Text.Pandoc          as Pandoc

import Control.Monad
import           Debug.Trace
import           Data.Maybe

import           Data.Either.Extra
import           Text.Blaze.Html.Renderer.String
                                                ( renderHtml )
import           System.FilePath                ( splitPath
                                                , dropFileName
                                                , takeFileName
                                                , takeBaseName
                                                , replaceFileName
                                                , takeDirectory
                                                , (</>)
                                                , (-<.>)
                                                )

import           Hakyll
import           Hakyll.Web.Pandoc (pandocCompilerWithTransform)

import           Webbi.Menu                     ( Menu(..) )
import qualified Webbi.Utils.RoseTree          as RT
import qualified Webbi.Utils.TreeZipper        as TZ
import qualified Webbi.Utils.Free              as F

import qualified Webbi.Menu                    as M
import qualified Webbi.Css                     as Css

import           Data.String
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A


styles :: Pattern
styles = "**css/*.hs"

images :: Pattern
images = "**images/*.jpg"

content :: Pattern
content = "**/index.md"

root :: String
root = "MISSING"


feedConfiguration :: FeedConfiguration
feedConfiguration =
    FeedConfiguration
        { feedTitle       = "Magnus Møller"
        , feedDescription = "Something cool"
        , feedAuthorName  = "Magnus Møller"
        , feedAuthorEmail = "magnus108@me.com"
        , feedRoot        = root
        }

compileAtom :: Rules ()
compileAtom = create ["atom.xml"] $ do
    route idRoute
    compile $ do
        let feedCtx = postCtx <> bodyField "description"
        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots (content .&&. hasNoVersion) "content"
        renderAtom feedConfiguration feedCtx posts


compileSitemap :: Rules ()
compileSitemap = create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
        pages <- recentFirst =<< loadAll content :: Compiler [Item String]
        singlePages <- loadAll (fromList ["index.html"])
        let allPages = pages <> singlePages
        let sitemapCtx = constField "root" root <>
                         listField "pages" postCtx (return allPages)

        makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

postCtx :: Context String
postCtx =
    constField "root" root <>
    dateField "date" "%Y-%m-%d" <>
    defaultContext


compileTemplates :: Rules ()
compileTemplates = match "templates/*" $ compile templateBodyCompiler


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
    compileFrontPage


compileRobots :: Rules ()
compileRobots = match "robots.txt" $ do
    route   idRoute
    compile copyFileCompiler


compileFrontPage :: Rules ()
compileFrontPage = match "index.html" $ do
        route idRoute
        compile $ do
            ctx <- contentContext
            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls


compileMenu :: Rules ()
compileMenu = match content $ do
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
            >>= loadAndApplyTemplate "templates/content.html" ctx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls


contentContext :: Compiler (Context String)
contentContext = do
    menu <- getMenu
    css  <- getCss
    return
        $  constField "menu" menu
        <> constField "css"  css
        <> defaultContext



getCss :: Compiler String
getCss = do
    route <- getRoute =<< getUnderlying
    case route of
        Nothing -> noResult "No current route"
        Just r  -> do
            items <- loadAll $ fromVersion $ Just "css"
            let css = Css.fromTreeZipper $ TZ.fromList r $ fmap itemBody items
            return $ renderHtml $ Css.showCss css


getMenu :: Compiler String
getMenu = do
    route <- getRoute =<< getUnderlying
    case route of
        Nothing -> noResult "No current route"
        Just r  -> do
            items <- loadAll $ fromVersion $ Just "menu"
            let menu = M.fromTreeZipper $ TZ.fromList r $ fmap itemBody items
            return $ renderHtml $ M.showMenu menu


compileImages :: Rules ()
compileImages =
    match images $ do
      route $ customRoute $ \x -> "images" </> (takeFileName (toFilePath x))
      compile copyFileCompiler


compileCV :: Rules ()
compileCV = do
    compileLatex
    create ["cv.pdf"] $ do
        route idRoute
        compile $ do
            i <- load (setVersion (Just "latex") "cv/index.md")
                >>= readPandoc
                >>= writeLaTex
                >>= loadAndApplyTemplate "templates/cv.tex" defaultContext
                >>= readPandoc
                >>= mapM (unsafeCompiler . Pandoc.runIOorExplode . makePDF "pdflatex" [] Pandoc.writeLaTeX Pandoc.def)

            makeItem (itemBody (fmap (fromRight') i))


compileLatex :: Rules ()
compileLatex = match content $ do
    version "latex" $ compile $ do
        item  <- getResourceBody
        return item


writeLaTex :: Item Pandoc.Pandoc -> Compiler (Item String)
writeLaTex item = fmap (fmap (toString . fromText)) $ unsafeCompiler $ Pandoc.runIOorExplode $ mapM (Pandoc.writeLaTeX Pandoc.def) item
