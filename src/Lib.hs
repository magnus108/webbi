module Lib
    ( compileCss
    , compileMenu
    , compileContent
    , compileTemplates
    )
where


import           Hakyll


compileCss :: Rules ()
compileCss = match "css/*.hs" $ do
    route $ setExtension "css"
    compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

compileMenu :: Rules ()
compileMenu = return ()

compileContent :: Rules ()
compileContent = return ()

compileTemplates :: Rules ()
compileTemplates = return ()
