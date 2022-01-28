{-# LANGUAGE OverloadedStrings #-}
import           Clay

import qualified Data.Text.Lazy.IO             as T

styleMenu :: Css
styleMenu = do
    body ?
        backgroundColor "#A1A2A3"

main :: IO ()
main = T.putStr $ renderWith compact [] $ styleMenu
