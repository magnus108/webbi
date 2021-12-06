{-# LANGUAGE OverloadedStrings #-}
import           Clay

import qualified Data.Text.Lazy.IO             as T

styleMenu :: Css
styleMenu = do
    body ? do
        background red
    return ()

main :: IO ()
main = T.putStr $ renderWith compact [] $ styleMenu
