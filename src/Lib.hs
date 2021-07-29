module Lib
    ( compileCss
    , compileMenu
    , compileContent
    , compileTemplates
    )
where

import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html.Renderer.String
                                                ( renderHtml )
import           Hakyll

import           Webbi.Menu


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
    -- make prefixtree??
    menu  <- fmap itemBody <$> loadAll (fromVersion $ Just "menu")
    route <- getRoute =<< getUnderlying
    -- find route in tree?
    case route of
        Nothing -> noResult "No current route"
        Just r  -> return $ renderHtml $ showMenu $ makeMenu r menu


makeMenu :: FilePath -> [FilePath] -> Menu
makeMenu = undefined

showMenu :: Menu -> H.Html
showMenu = undefined


compileTemplates :: Rules ()
compileTemplates = match "templates/*" $ compile templateBodyCompiler
