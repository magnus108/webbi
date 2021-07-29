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


compileMenu :: Rules ()
compileMenu = return ()


compileContent :: Rules ()
compileContent = do
    compilePosts


compilePosts :: Rules ()
compilePosts = match "posts/*" $ do
    route $ setExtension "html"
    compile
        $   pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls


compileTemplates :: Rules ()
compileTemplates = match "templates/*" $ compile templateBodyCompiler
