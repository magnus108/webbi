{-# LANGUAGE OverloadedStrings #-}
import           Clay
import           Clay

import qualified Data.Text.Lazy.IO             as T

styleMenu :: Css
styleMenu = header |> nav ? do
    background red
    color "#04a"
    fontSize (px 24)
    padding (px 20) nil (px 20) nil
    textTransform uppercase
    position absolute
    left nil
    right nil
    bottom (px (-72))

main :: IO ()
main = T.putStr $ renderWith compact [] $ styleMenu
