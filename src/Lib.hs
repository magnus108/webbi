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

import Text.Pandoc.UTF8 (toStringLazy, fromText, toString, toText)
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy as BL
import Data.Either

import Text.Pandoc.PDF
import qualified Text.Pandoc          as Pandoc

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




    {-
compileCV :: Rules ()
compileCV =
    match "cv/index.md" $ version "pdf" $ do
        route $ constRoute "cv.pdf"
        compile $ do
            i <- writeLaTex =<< readPandoc =<< getResourceBody
            traceShowM "lol1"
            tt <- loadAndApplyTemplate "templates/cv.tex" defaultContext i
            traceShowM "lol2"
            ttt <- readPandoc tt
            traceShowM "lol3"
            let tttt = myWritePandocWith defaultHakyllWriterOptions ttt
            traceShowM "lol33"
            lol <-  mapM (\x -> unsafeCompiler $ Pandoc.runIOorExplode $ fmap fromRight'
                        $ makePDF "pdflatex" [] Pandoc.writeLaTeX defaultHakyllWriterOptions x) ttt
            traceShowM "lol4"
            return $ lol
            -}

compileCV :: Rules ()
compileCV = do
    compileLatex
    create ["cv.pdf"] $ do
        route idRoute
        compile $ do
            readdd <- readPandoc =<< (load "cv.tex")
            traceShowM "wtf"
            traceShowM readdd
            i <- mapM (\x -> unsafeCompiler $ Pandoc.runIOorExplode $ fmap (\x -> fromRight' (traceShow x x))
                $ makePDF "pdflatex" [] Pandoc.writeLaTeX (defaultHakyllWriterOptions {Pandoc.writerReferenceLinks = True}) x) readdd
            (makeItem (toStringLazy (itemBody i)))
    create ["cv.tex"] $ do
        route idRoute
        compile $ do
            g <- makeItem ""
            readdd <- (load (setVersion (Just "latex") "cv/index.md"))
            readd <- readPandoc readdd
            i <- writeLaTex readd
            let ctx = (constField "latex" (itemBody i)) <> defaultContext
            tt <- loadAndApplyTemplate "templates/cv.tex" ctx g
            return tt
                {-
            traceShowM "3"
            traceShowM tt
            ttt <- readLaTex tt
            traceShowM "4"
            --lol <-  mapM (\x -> unsafeCompiler $ Pandoc.runIOorExplode $ fmap (\x -> fromRight' (traceShow x x))
             --           $ makePDF "pdflatex" [] Pandoc.writeLaTeX defaultHakyllWriterOptions x) ttt
            traceShowM "5"
            writeLaTex ttt
            --return lol
            --}
             


compileLatex :: Rules ()
compileLatex = match content $ do
    version "latex" $ compile $ do
        item  <- getResourceBody
        traceShowM "fucker"
        traceShowM item
        return item

    {-
myWritePandocWith :: Pandoc.WriterOptions -> Item Pandoc.Pandoc -> Item String
myWritePandocWith wopt (Item itemi doc) =
    case Pandoc.runPure $ Pandoc.writeLaTeX wopt doc of
        Left err    -> error $ "Hakyll.Web.Pandoc.writePandocWith: " ++ show err
        Right item' -> Item itemi $ T.unpack item'
            -}

readLaTex :: Item String -> Compiler (Item Pandoc.Pandoc)
readLaTex item = unsafeCompiler $ Pandoc.runIOorExplode $ mapM (Pandoc.readLaTeX Pandoc.def) (fmap (toText . fromString) item)


writeLaTex :: Item Pandoc.Pandoc -> Compiler (Item String)
writeLaTex item = fmap (fmap (toString . fromText)) $ unsafeCompiler $ Pandoc.runIOorExplode $ mapM (Pandoc.writeLaTeX (Pandoc.def {Pandoc.writerReferenceLinks = True}) ) item
