module Lib
    ( compileCss
    , compileMenu
    , compileContent
    , compileTemplates
    )
where


import           Hakyll


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
        item <- setVersion Nothing <$> getUnderlying
        route <- getRoute item
        case route of
            Nothing -> noResult "No menu item"
            Just r -> makeItem r


compilePosts :: Rules ()
compilePosts = match content $ do
    route $ setExtension "html"
    compile
        $   pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls


compileTemplates :: Rules ()
compileTemplates = match "templates/*" $ compile templateBodyCompiler
