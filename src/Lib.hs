module Lib
    ( compileCss
    , compileMenu
    , compileContent
    , compileTemplates
    , compileRobots
    , compileSitemap
    , compileAtom
    , compileImages
    , compilePdf
    , compileFrontPagePdf
    )
where

import           Data.String

import qualified Data.Text                     as T
import qualified Text.Pandoc.UTF8              as PUTF8

import           Data.Either
import qualified System.Process                as Process

import           Text.Pandoc.PDF
import qualified Text.Pandoc                   as Pandoc

import           Control.Monad
import           Debug.Trace
import           Data.Maybe

import           Data.Either.Extra
import           Text.Blaze.Html.Renderer.String
                                                ( renderHtml )
import           System.FilePath                ( splitPath
                                                , dropFileName
                                                , takeFileName
                                                , replaceExtension
                                                , takeBaseName
                                                , replaceFileName
                                                , takeDirectory
                                                , (</>)
                                                , (-<.>)
                                                )

import           Hakyll
import           Hakyll.Web.Pandoc              ( pandocCompilerWithTransform )

import Webbi.Menu                    ( Menu(..))
import qualified Webbi.Utils.RoseTree          as RT
import qualified Webbi.Utils.TreeZipper        as TZ
import qualified Webbi.Utils.Free              as F

import qualified Webbi.Menu                    as M
import qualified Webbi.Css                    as Css

import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A


styles :: Pattern
styles = "**css/*.hs"

stylesConfig :: Pattern
stylesConfig = "**css/*.dhall"

images :: Pattern
images = "**images/*.jpg"

content :: Pattern
content = "**/index.md"

root :: String
root = "MISSING"


feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration { feedTitle       = "Magnus Møller"
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
        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots
            (content .&&. hasNoVersion)
            "content"
        renderAtom feedConfiguration feedCtx posts


compileSitemap :: Rules ()
compileSitemap = create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
        pages <-
            recentFirst =<< loadAll (content .&&. hasNoVersion) :: Compiler
                [Item String]
        singlePages <- loadAll (fromList ["index.html"])
        let allPages = pages <> singlePages
        let sitemapCtx = constField "root" root
                <> listField "pages" postCtx (return allPages)

        makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

postCtx :: Context String
postCtx =
    constField "root" root <> dateField "date" "%Y-%m-%d" <> defaultContext


compileTemplates :: Rules ()
compileTemplates = match "templates/*" $ compile templateBodyCompiler


compileCss :: Rules ()
compileCss = do
    compileClay
    compileStylesConfig
    compileStyles


compileStylesConfig :: Rules ()
compileStylesConfig = do
    match stylesConfig $ do
        version "stylesConfig" $ compile $ getResourceBody


compileClay :: Rules ()
compileClay = do
    match styles $ do
        route $ setExtension "css"
        compile $ do
            path <- getResourceFilePath
            makePatternDependency (fromGlob $ (dropFileName path) </> "*")
            getResourceString >>= withItemBody (unixFilter "runghc" [])


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
    route idRoute
    compile copyFileCompiler


compileFrontPage :: Rules ()
compileFrontPage = match "index.html" $ do
    route idRoute
    compile $ do
        ctx <- contentContext
        getResourceBody
            >>= saveSnapshot "pandoc"
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
        getResourceBody
            >>= saveSnapshot "pandoc"
            >>= renderPandoc
            >>= loadAndApplyTemplate "templates/content.html" ctx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls


contentContext :: Compiler (Context String)
contentContext = do
    menu <- getMenu
    css  <- getCss
    pdf  <- getPdf
    return
        $  constField "menu" menu
        <> constField "css"  css
        <> constField "pdf"  pdf
        <> defaultContext


getPdf :: Compiler String
getPdf = do
    route <- getRoute =<< getUnderlying
    case route of
        Nothing -> noResult "No current route"
        Just r  -> do
            --cheap solution. Use zipper
            return
                $ renderHtml
                $ H.a
                ! A.class_ "title-pdflink"
                ! A.href (fromString ("/" </> r -<.> ".pdf"))
                $ "PDF"


getCss :: Compiler String
getCss = do
    route <- getRoute =<< getUnderlying
    case route of
        Nothing -> noResult "No current route"
        Just r  -> do
            items <- loadAll $ fromVersion $ Just "css"
            let css = Css.fromTreeZipper $ TZ.fromList r $ fmap itemBody items
            return $ renderHtml css


getMenu :: Compiler String
getMenu = do
    route <- getRoute =<< getUnderlying
    case route of
        Nothing -> noResult "No current route"
        Just r  -> do
            items <- loadAll $ fromVersion $ Just "menu"
            let menu = M.fromTreeZipper $ TZ.fromList r $ fmap itemBody items
            return $ renderHtml menu


compileImages :: Rules ()
compileImages = match images $ do
    route $ customRoute $ \x -> "images" </> (takeFileName (toFilePath x))
    compile copyFileCompiler


compilePdf :: Rules ()
compilePdf = match content $ version "pdf" $ do
    route $ setExtension "pdf"
    compile $ do
        r <- getResourceFilePath
        loadSnapshot (setVersion Nothing (fromFilePath r)) "pandoc"
            >>= readPandoc
            >>= writeLaTex
            >>= loadAndApplyTemplate "templates/cv.tex" defaultContext
            >>= latex


compileFrontPagePdf :: Rules () -- fix this dublicate
compileFrontPagePdf = match "index.html" $ version "pdf" $ do
    route $ setExtension "pdf"
    compile $ do
        r <- getResourceFilePath
        loadSnapshot (setVersion Nothing (fromFilePath r)) "pandoc"
            >>= readPandoc
            >>= writeLaTex
            >>= loadAndApplyTemplate "templates/cv.tex" defaultContext
            >>= latex


latex :: Item String -> Compiler (Item TmpFile)
latex item = do
    TmpFile texPath <- newTmpFile "latex.tex"
    let tmpDir  = takeDirectory texPath
        pdfPath = replaceExtension texPath "pdf"

    unsafeCompiler $ do
        writeFile texPath $ itemBody item
        _ <-
            Process.system
                $ unwords
                      [ "pdflatex"
                      , "--halt-on-error"
                      , "-output-directory"
                      , tmpDir
                      , texPath
                      ]
        return ()

    makeItem $ TmpFile pdfPath


writeLaTex :: Item Pandoc.Pandoc -> Compiler (Item String)
writeLaTex item =
    fmap (fmap T.unpack) $ unsafeCompiler $ Pandoc.runIOorExplode $ mapM
        (Pandoc.writeLaTeX Pandoc.def)
        item
